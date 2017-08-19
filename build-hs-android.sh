set -e

cabal clean

cabal -v configure --with-ghc=arm-unknown-linux-androideabi-ghc --with-ld=arm-linux-androideabi-ld.gold --with-ghc-pkg=arm-unknown-linux-androideabi-ghc-pkg

cabal -v build

cp -v dist/build/libhaskell.so/libhaskell.so proj.android-studio/app/jni/libhaskell.so
