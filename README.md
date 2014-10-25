forest
======

Requirements:
*	GHC 7.8.3
*	nilfs-utils (https://github.com/konis/nilfs-utils/tree/diff-v2) with patched Linux kernel (https://github.com/konis/nilfs2/tree/diffapi-v2) for log-based filesystem support

Dependencies:
*	pads-haskell: data description language
*	adapton: general-purpose incremental programming library (https://github.com/cornell-pl/HsAdapton/tree/master)

Running:

A good way to start is to load some example module. For instance, inside directory forest/, you will typically want:

ghci Examples/Papers2.hs -ddump-splices -fcontext-stack=60
