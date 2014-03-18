forest
======

Forest project

* padsrepo: copy of the original pads SVN repository
* pasd-haskell: clone of the Galois repository (https://github.com/GaloisInc/pads-haskell) that has a more updated version of pads-haskell
* forest: forest source code updated to work with GHC 7.6.3
* lens some internal functions used from the lens package
* tyb some internal functions used from the tyb package

Running

A good way to start is to load some example module. For instance, inside directory forest/, you will typically want:

ghci Examples/Papers2.hs -i../pads-haskell:../lens:../tyb  -ddump-splices
