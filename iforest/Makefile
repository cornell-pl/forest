update:
	cd ../syb-with-class-and-effects; cabal install syb-with-class-and-effects.cabal --force-reinstalls -j1
	cd ../../HsAdapton; cabal install adapton.cabal --force-reinstalls -j1
icpapers2:
	ghci -isrc -ddump-splices -fcontext-stack=60 Examples/IC/Papers2.hs
icpapers2tx:
	ghci -isrc -ddump-splices -fcontext-stack=60 Examples/IC/Papers2Tx.hs
icpapers2x:
	ghc -ddump-splices -fcontext-stack=60 Examples/IC/Papers2Tx.hs -o Papers2Tx
icuniversal:
	ghci -isrc -ddump-splices -fcontext-stack=60 Examples/IC/Universal.hs
pureaccount:
	ghci -ddump-splices -fcontext-stack=60 Examples/Pure/Beautiful/Account.hs
icaccount:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Beautiful/Account.hs
icaccounts:
	ghci -isrc -ddump-splices -fcontext-stack=60 Examples/IC/Beautiful/Accounts.hs
ictpcc:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/TPCC.hs
icsanta:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Beautiful/Santa.hs
toys:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Toy/Toy.hs
all:
	ghci -ddump-splices -fcontext-stack=60 Examples.hs
clean:
	find . -name "*.hi" -type f -delete
	find . -name "*.hi-boot" -type f -delete
	find . -name "*.p_o" -type f -delete 
	find . -name "*.dyn_hi" -type f -delete 
	find . -name "*.dyn_o" -type f -delete 
	find . -name "*.o" -type f -delete
	find . -name "*.o-boot" -type f -delete 
	rm -rf dist
run:
	ghc --make Run.hs -fcontext-stack=60
	./Run
exprof:
	ghc --make Examples/IC/Papers2Tx.hs -fcontext-stack=60 -rtsopts -isrc -o Papers2Tx
	ghc --make Examples/IC/Papers2Tx.hs -fcontext-stack=60 -osuf p_o -prof -auto-all -caf-all -rtsopts -isrc -o Papers2Tx
	./Papers2Tx +RTS -pa -s
