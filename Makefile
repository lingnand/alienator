ifndef TARGET
$(error TARGET is not set; should be one of [mac, android])
endif

.PHONY: configure build debug clean newbuild

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
	mv -v dist/build/libhaskell/libhaskell proj.android-studio/app/jni/libhaskell.so && \
	ndk-build -C proj.android-studio/app NDK_MODULE_PATH=$$(eval "echo $$(jq -r '.ndk_module_path | join(":")' proj.android-studio/build-cfg.json)") NDK_TOOLCHAIN_VERSION=4.9 NDK_DEBUG=1 && \
	cp -v proj.android-studio/app/libs/armeabi/*.so /target/proj.android-studio/app/libs/armeabi/ && \
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
	docker run --rm -v `pwd`:/target nix-cross-android build
debug: build
	cd proj.android-studio && ./gradlew openDebug && \
	sleep 0.5 && \
	adb shell ps | grep -m1 'org\.cocos' | awk '{ print $$2 }' | xargs adb logcat --pid
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
