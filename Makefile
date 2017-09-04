ifndef TARGET
$(error TARGET is not set; should be one of [mac, android])
endif

.PHONY: configure build clean newbuild

newbuild: clean configure build

ifeq ($(TARGET),android)

# if inside the nixos container
ifneq ("$(wildcard /nix/store)","")

configure:
	cabal configure --with-ghc=arm-unknown-linux-androideabi-ghc --with-ld=arm-linux-androideabi-ld.gold --with-ghc-pkg=arm-unknown-linux-androideabi-ghc-pkg -f target-android && \
	cp -rv dist /target/
build:
	cabal -v build && \
	cp -rv dist /target/ && \
	cp -v dist/build/libhaskell/libhaskell /target/libhaskell.so && \
  { [ -d Classes ] && cp -rv Classes/* /target/Classes/ || true; }
clean:
	cabal clean

# else we should make use of the docker container
else

configure:
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android configure
build:
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android build && \
	cp -v libhaskell.so proj.android-studio/app/jni/ && \
	mv -v libhaskell.so proj.android-studio/app/libs/armeabi/
clean:
	cabal clean; docker rmi nix-cross-android 2>/dev/null || true

endif

else ifeq ($(TARGET),mac)

# assume on a mac
configure:
	cabal configure -f target-mac
build:
	cabal -v build && \
	mv -v libhaskell.a proj.ios_mac/mac/libs/libhaskell.a
clean:
	cabal clean

endif
