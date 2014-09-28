build:
	GHC_PACKAGE_PATH=/usr/lib/ghc-7.8.3/package.conf.d:`pwd`/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d ghc -threaded Main.hs
clean:
	rm *.o *.hi Gatekeeper/*.o Gatekeeper/*.hi
dependencies:
	cabal sandbox init
	cabal install uuid hex cereal protobuf network random ldap
