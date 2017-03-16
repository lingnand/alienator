{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Alienator.GamePlayScene
  (
    gamePlayScene
  ) where

import qualified Data.Set as S
import Data.Default
import Data.Colour.Names
import Data.Colour
import Data.Functor.Misc
import Linear
import Linear.Affine
import Diagrams ((^&), (@@), deg, unr2)
import Diagrams.TwoD.Vector (e)
import Diagrams.TwoD.Shapes
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Reflex
import Reflex.Extra
import Reflex.Cocos2d
import Reflex.State
import qualified Data.Pool as P
import System.Random
import qualified Control.Monad.Random as R
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import Alienator.PhysicsSprite
import Alienator.Actuator
import Alienator.Constants
import Alienator.State

bulletShouldReset :: CollisionCategory -- ^ bullet collisionCategory
                  -> CollisionCategory -- ^ other collisionCategory
                  -> Bool
bulletShouldReset PlayerBullet PlayerShip = False
bulletShouldReset EnemyBullet EnemyShip = False
bulletShouldReset _ _ = True

keyToUnitV :: KeyCode -> V2 Float
keyToUnitV KeyCode_UpArrow = V2 0 1
keyToUnitV KeyCode_DownArrow = V2 0 (-1)
keyToUnitV KeyCode_LeftArrow = V2 (-1) 0
keyToUnitV KeyCode_RightArrow = V2 1 0
keyToUnitV _ = 0

getRandomV2 :: (Random a, R.MonadRandom m) => (V2 a, V2 a) -> m (V2 a)
getRandomV2 (V2 fx fy, V2 tx ty) =
    (^&) <$> R.getRandomR (fx, tx) <*> R.getRandomR (fy, ty)


buildDiffs ::
  (Eq a, MonadDynState t (P.Pool a) m, MonadSample t m, MonadAccum t m)
  => (P.Id -> Dynamic t (Maybe a) -> m b) -> m ([b], Event t [b])
buildDiffs onDiff = do
    slotsDyn <- watches P.slots
    -- detect the new slots in the pool
    let pidsDyn = IM.keysSet <$> slotsDyn
        newPidsE = attachWithMaybe f (current pidsDyn) (updated pidsDyn)
          where f curr updated | diff <- IS.difference updated curr
                               , not (IS.null diff) = Just diff
                               | otherwise          = Nothing
        processNewPids = flip IS.foldr (pure []) $ \pid ->
          liftM2 (:) (onDiff pid (uniqDyn $ join . IM.lookup pid <$> slotsDyn))
    pids <- sample (current pidsDyn)
    runWithAccumulation (processNewPids pids) $ processNewPids <$> newPidsE

-- | add movement into enemy
reactuateEnemyModifier :: R.MonadRandom m => Float -> m (VelActuator -> VelActuator)
reactuateEnemyModifier enemyBaseVel = do
    -- choose the angle
    ang <- R.getRandomR (90, 270)
    -- choose the velocity
    v <- R.getRandomR (enemyBaseVel, enemyBaseVel*2)
    return $ vel .~ (v *^ e (ang @@ deg))

gamePlayScene ::
  ( NodeBuilder t m
  , MonadFix m, MonadHold t m
  , PostBuild t m, MonadAccum t m, MonadAdjust t m
  , MonadIO m, FastTriggerEvent t m
  , MonadFinalize m, MonadIO (Finalizable m)
  , PerformEvent t m, MonadIO (Performable m)
  )
  => V2 Float                 -- ^ window size
  -> Space CollisionCategory  -- ^ physics space
  -> Event t SpaceStep
  -> EventSelector t (Const2 (Body CollisionCategory) (ShapeAttributes CollisionCategory))
  -> Dynamic t (S.Set KeyCode)    -- ^ keys down
  -> DynStateT t (GamePlaySceneState (Performable m)) m (Event t ()) -- ^ return when game is over
gamePlayScene winSize sp steps collisionsE keysDyn = runWithReplaceFree $ do
    ticks <- lift getFrameTicks
    let bulletBaseAccel :: Float = 10.0
        playerBaseVel :: Float = 50.0
        bulletPosOffset :: Float = 100.0
        enemyBaseVel :: Float = playerBaseVel*1.5
        enemyShipContour :: V2 Float = 130^&80

    finishes <- lift . mapM loadTexture $
      [ "res/img/enemy" ++ show i ++ ".png"
      | i <- [0..3] :: [Int]
      ]
      ++
      [ "res/img/bullet.png"
      , "res/img/player.png" ]
    waitDynMaybe_ . distributeListOverDynWith sequence_ =<< lift (mapM dynMaybe finishes)

    -- fps ticks
    [ fpsD8, fps1, fps2, fps5 ] <- lift $ mapM (`modulate` ticks) [0.8, 1, 2, 5]

    -- render player ship
    playerDyn <- lift . zoomAcc playerShip $ do
      playerHits <- zoomAcc pSprite $ do
        hits <- physicsSprite sp steps collisionsE
        performEvent_ . ffor hits $ \ct -> liftIO . putStrLn $ "Player got hit with " ++ show ct
        adjust $ ffor (updated keysDyn) $
          \ks -> actuator.vel .~ playerBaseVel *^ foldr ((+) . keyToUnitV) 0 ks
        return hits

      let healthHits = ffor playerHits $ \case
            EnemyShip -> 30
            EnemyBullet -> 10
            _ -> 0
      adjust $ (health.meterRead -~) <$> healthHits
      uniqDyn <$> watch

    -- player status
    lift $ do
      label_
        [ dyn text  := (\(Meter r m) -> show r ++ " / " ++ show m) . (^.health) <$> playerDyn
        , systemFontSize  := 20
        , textColor := opaque white
        , position  := 200^&100
        ]

      -- render the enemy ships
      enemiesBulletPoolModE <- zoomAcc enemyShipPool $ do
        let -- randomly add movement into the enemy
            maybeReactuateModifier :: R.MonadRandom n => n (Maybe (VelActuator -> VelActuator))
            maybeReactuateModifier = runMaybeT $ do
                -- first randomly select whether we should reactuate in the first place
                MaybeT $ R.fromList
                  [ (Just (), 0.4)
                  , (Nothing, 0.6)
                  ]
                lift $ reactuateEnemyModifier enemyBaseVel
            maybeAttackModifier :: (R.MonadRandom n, Monad host) => Int -> VelActuator -> n (Maybe [BulletState host])
            maybeAttackModifier bulletType act = R.fromList
                [ (Just bs, 0.8)
                , (Nothing, 0.2)
                ]
              where spawnP = act^.pos - bulletPosOffset^&0
                    bs = [ bullet EnemyBullet (spawnP & _y +~ fromIntegral offset) (norm (act^.vel) *^ (-3^&0)) 0
                         | x <- [0..bulletType-1]
                         , let offset = ((x+1) `div` 2) * 30 * (if odd x then 1 else -1)
                         ]

        -- rendering
        (bulletPoolModEsZ, bulletPoolModEEs) <- buildDiffs $ \pid slotDyn -> do
          let -- profileSignalDyn is a valid profile (enabled = true) when someone takes a slot;
              -- and invalid when slotDyn is Nothing (because enabled = false in def)
              profileModDyn = maybe (enabled .~ False) const <$> slotDyn
          startProfile <- ($ def) <$> sample (current profileModDyn)
          flip evalAccStateT startProfile $ do
            -- insert profileSignals into the stream
            adjust $ updated profileModDyn
            hits <- physicsSprite sp steps collisionsE

            -- when we are being hit by these things, we release the slot
            -- from Pool
            let resetHits = ffilter (`elem` [PlayerBullet, Wall]) hits
            lift . adjust $ P.releaseSlot pid <$ resetHits

            -- behaviors
            enemyBeh <- refine current watch
            let enabledBeh = (^.enabled) <$> enemyBeh
                actBeh = (^.actuator) <$> enemyBeh

            bulletsE <- R.evalRandTIO $ do
              bulletT <- R.getRandomR (1, 3)
              let maybeReactuateRE = maybeReactuateModifier <$ gate enabledBeh fps2
                  maybeAttackRE = attachWith (const . maybeAttackModifier bulletT) actBeh $ gate enabledBeh fps1
              modE <- fmapMaybe id <$> liftRandE maybeReactuateRE
              lift . adjust $ (actuator %~) <$> modE
              fmapMaybe id <$> liftRandE maybeAttackRE

            return (P.takeSlots_ <$> bulletsE)

        -- the bullet pool modifiers are accumulative
        bulletPoolModE <- accumEWith (.) (mergeWith (.) bulletPoolModEsZ)
                                         (mergeWith (.) <$> bulletPoolModEEs)

        -- generate enemies
        randSpawnEnemiesE <- R.evalRandTIO $ liftRandE $ ffor fps5 . const $ do
            rv2 <- getRandomV2 (winSize & _x *~ 0.7 & _y *~ 0.25, winSize & _x *~ 0.9 & _y *~ 0.75)
            i :: Int <- R.getRandomR (0, 3) -- the enemy type
            modifier <- reactuateEnemyModifier enemyBaseVel
            return $ def & actuator  .~ (def & pos .~ 0 .+^ rv2 & modifier)
                         & sCategory .~ EnemyShip
                         & sGeometry .~ Polygon (reverse $ uncurry rect $ unr2 enemyShipContour)
                         & sMass     .~ 6000
                         & sprName   .~ "res/img/enemy" ++ show i ++ ".png"
                         & enabled   .~ True
        adjust $ P.takeSlot_ <$> randSpawnEnemiesE

        return bulletPoolModE

      -- render bullets
      zoomAcc bulletPool $ do
        -- apply the adjusts by enemies
        adjust enemiesBulletPoolModE

        void . buildDiffs $ \pid slotDyn -> do
          let profileModDyn = maybe (enabled .~ False) const <$> slotDyn
          startProfile <- ($ def) <$> sample (current profileModDyn)
          flip evalAccStateT startProfile $ do
            adjust $ updated profileModDyn
            hits <- physicsSprite sp steps collisionsE
            -- once hit the wall/ship/etc., anchor them and deactivate
            cTypeBeh <- refine current $ watches (^.sCategory)
            let resetHits = attachWithMaybe (\x y -> guard $ bulletShouldReset x y) cTypeBeh hits
            lift . adjust $ P.releaseSlot pid <$ resetHits

        -- generate bullets
        let onBulletFreq ks _ | S.member KeyCode_Space ks = Just ()
                              | otherwise = Nothing
            playerBulletFireE = attachPromptlyDynWithMaybe onBulletFreq keysDyn fpsD8
            playerBulletsGen p =
              [ bullet PlayerBullet (p .+^ pOffset) v ac
              | ang <- [30, 0, -30]
              , let uv = e (ang @@ deg)
                    pOffset = bulletPosOffset *^ uv
                    v = (playerBaseVel*5) *^ uv
                    ac = bulletBaseAccel *^ uv
              ]
            playerPosBeh = (^.pSprite.actuator.pos) <$> current playerDyn
        adjust $ P.takeSlots_ . playerBulletsGen <$> tag playerPosBeh playerBulletFireE


    waitEvent $ void $ ffilter (<=90) $ (^.health.meterRead) <$> updated playerDyn
