ifndef TARGET
$(error TARGET is not set; should be one of [mac, android])
endif

.PHONY: hs.configure hs.build  hs.clean hs.newbuild


ifeq ($(TARGET),android)

# assum inside the nixos container
hs.configure:
	cabal configure --with-ghc=arm-unknown-linux-androideabi-ghc --with-ld=arm-linux-androideabi-ld.gold --with-ghc-pkg=arm-unknown-linux-androideabi-ghc-pkg -f target-android
hs.build:
	cabal -v build && \
	cp -v dist/build/libhaskell/libhaskell proj.android-studio/app/jni/libhaskell.so
hs.clean:
	cabal clean

else ifeq ($(TARGET),mac)

# assume on a mac
hs.configure:
	cabal configure -f target-mac
hs.build:
	cabal -v build  && \
	mv -v libhaskell.a proj.ios_mac/mac/libs/libhaskell.a
hs.clean:
	cabal clean

endif

hs.newbuild: hs.clean hs.configure hs.build
