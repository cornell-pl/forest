update:
	cd ../syb-with-class-and-effects; cabal install syb-with-class-and-effects.cabal --force-reinstalls -j1
	cd ../../HsAdapton; cabal install adapton.cabal --force-reinstalls -j1
icpapers2:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Papers2.hs
icuniversal:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Universal.hs
pureaccount:
	ghci -ddump-splices -fcontext-stack=60 Examples/Pure/Beautiful/Account.hs
icaccount:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/Beautiful/Account.hs
ictpcc:
	ghci -ddump-splices -fcontext-stack=60 Examples/IC/TPCC.hs
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
