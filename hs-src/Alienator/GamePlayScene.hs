{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Reflex
import Reflex.Extra
import Reflex.Cocos2d
import Reflex.State
import System.Random
import qualified Control.Monad.Random as R

import qualified Alienator.Pool as P
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


buildDiffs
  :: forall t m a b. (BuilderBase t m)
  => (P.Id -> DynStateT t a (NodeBuilder t m) b) -> DynStateT t (P.Pool a) (NodeBuilder t m) (Event t [b])
buildDiffs onDiff = do
    let f prev curr | diff <- P.difference curr prev
                    , not (P.null diff) = (Just curr, Just diff)
                    | otherwise         = (Just curr, Nothing)
    diffE <- mapAccumMaybe_ f P.empty =<< postponeCurrent =<< watch
    squashAccStateTT $ seqForEvent diffE . P.traverseWithId $ \pid ps -> do
      focus (at pid . pnon ps) $ onDiff pid

-- | add movement into enemy
reactuateEnemyModifier :: R.MonadRandom m => Float -> m (VelActuator -> VelActuator)
reactuateEnemyModifier enemyBaseVel = do
    -- choose the angle
    ang <- R.getRandomR (90, 270)
    -- choose the velocity
    v <- R.getRandomR (enemyBaseVel, enemyBaseVel*2)
    return $ vel .~ (v *^ e (ang @@ deg))

gamePlayScene
  :: forall t m. (BuilderBase t m)
  => V2 Float                 -- ^ window size
  -> Space CollisionCategory  -- ^ physics space
  -> Event t SpaceStep
  -> EventSelector t (Const2 (Body CollisionCategory) (ShapeAttributes CollisionCategory))
  -> Dynamic t (S.Set KeyCode)    -- ^ keys down
  -> DynStateT t (GamePlaySceneState m) (NodeBuilder t m) (Event t ()) -- ^ return when game is over
gamePlayScene winSize sp steps collisionsE keysDyn = seqHoldFree $ do
    ticks <- lift $ view frameTicks
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
    [ fpsD8, fps1, fps2, fps5 ] <- lift $ mapM (flip modulate ticks) [0.8, 1, 2, 5]

    -- render player ship
    playerDyn' <- lift . focus playerShip $ do
      playerHits <- focus pSprite $ do
        hits <- physicsSprite sp steps collisionsE
        seqForEvent_ hits $ \ct -> liftIO . putStrLn $ "Player got hit with " ++ show ct
        adjust $ ffor (updated keysDyn) $
          \ks -> actuator.vel .~ playerBaseVel *^ (foldr ((+) . keyToUnitV) 0 ks)
        return hits

      let healthHits = ffor playerHits $ \case
            EnemyShip -> 30
            EnemyBullet -> 10
            _ -> 0
      adjust $ (health.meterRead -~) <$> healthHits
      watch
    let playerDyn = uniqDyn playerDyn'

    -- player status
    lift $ do
      label_
        [ dyn text  := (\(Meter r m) -> show r ++ " / " ++ show m) . (^.health) <$> playerDyn
        , systemFontSize  := 20
        , textColor := opaque white
        , position  := 200^&100
        ]

      -- render the enemy ships
      bulletPoolModEE <- focus enemyShipPool $ do
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
            maybeAttackModifier enemyType act = R.fromList
                [ (Just bs, 0.8)
                , (Nothing, 0.2)
                ]
              where spawnP = act^.pos - bulletPosOffset^&0
                    bs = [ bullet EnemyBullet (spawnP & _y +~ fromIntegral offset) ((norm $ act^.vel) *^ (-3^&0)) 0
                         | x <- [0..enemyType-1]
                         , let offset = ((x+1) `div` 2) * 30 * (if odd x then 1 else -1)
                         ]

        -- rendering
        enemyModsE <- buildDiffs $ \pid -> do
          hits <- physicsSprite sp steps collisionsE
          let resetHits = ffilter (`elem` [PlayerBullet, Wall]) hits
          adjust $ (enabled .~ False) <$ resetHits
          -- behaviors
          enemyBeh <- refine current watch
          let enabledBeh = (^.enabled) <$> enemyBeh
              actBeh = (^.actuator) <$> enemyBeh
          -- random reactuation
          (modE, bulletsE) <- lift . hoist R.evalRandTIO $ do
            modE <- seqEventMaybe $ maybeReactuateModifier <$ gate enabledBeh fps2
            -- random firing of bullets
            -- TODO: use proper enemyType
            bulletT <- R.getRandomR (1, 3)
            bulletsE <- seqEventMaybe $
              attachWith (const . maybeAttackModifier bulletT) actBeh $ gate enabledBeh fps1
            return (modE, bulletsE)
          adjust $ (actuator %~) <$> modE
          return (P.markIdle pid <$ resetHits, P.putNextIdles <$> bulletsE)

        let (resetModEE, bulletPoolModEE) = splitE $ ffor enemyModsE $ \mods ->
                                                let (as, bs) = unzip mods
                                                    f = mergeWith (.)
                                                in (f as, f bs)
        squashAccStateTT $ seqEvent_ $ (adjust <$> resetModEE)

        -- generate enemies
        randSpawnEnemiesE <- lift . hoist R.evalRandTIO . seqForEvent fps5 . const $ do
            rv2 <- getRandomV2 (winSize & _x *~ 0.7 & _y *~ 0.25, winSize & _x *~ 0.9 & _y *~ 0.75)
            i :: Int <- R.getRandomR (0, 3) -- the enemy type
            modifier <- reactuateEnemyModifier enemyBaseVel
            return $ (actuator  .~ (def & pos .~ 0 .+^ rv2 & modifier))
                   . (sCategory .~ EnemyShip)
                   . (sGeometry .~ Polygon (reverse $ uncurry rect $ unr2 enemyShipContour))
                   . (sMass     .~ 6000)
                   . (sprName   .~ "res/img/enemy" ++ show i ++ ".png")
                   . (enabled   .~ True)
        adjust $ P.modifyIdle <$> randSpawnEnemiesE

        return bulletPoolModEE

      -- render bullets
      focus bulletPool $ do
        resetModsE <- buildDiffs $ \pid -> do
          hits <- physicsSprite sp steps collisionsE
          -- once hit the wall/ship/etc., anchor them and deactivate
          cTypeBeh <- refine current $ watches (^.sCategory)
          let resetHits = attachWithMaybe (\x y -> guard $ bulletShouldReset x y) cTypeBeh hits
          adjust $ (enabled .~ False) <$ resetHits
          return $ P.markIdle pid <$ resetHits

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
        adjust $ P.putNextIdles . playerBulletsGen <$> tag playerPosBeh playerBulletFireE

        squashAccStateTT $ do
          seqEvent_ $ (adjust . mergeWith (.) <$> resetModsE)
          -- generate enemy bullets
          seqEvent_ $ (adjust <$> bulletPoolModEE)


    waitEvent $ void $ ffilter (<=0) $ (^.health.meterRead) <$> updated playerDyn
