{-# LANGUAGE TemplateHaskell, CPP, PatternGuards #-}
{-# OPTIONS_GHC -W -Wall #-}

{-|
This is the code for \"Template Your Boilerplate\" under review at the Haskell Symposium 2012.

A draft copy of that paper is available at <http://cs.pdx.edu/~adamsmic/projects/tyb/TYB.pdf>
and provides more thorough documentation.
-}

#define BENCHMARK 0
module Data.Generics.TH
  ( -- * Primitives
    thcase, thcase', thfoldl
    -- * Single-layer traversals
  , thmapT, thmapM, thmapQ, thmapQl, thmapQr
    -- * Memoization
  , memoizeDec, memoizeDec2 -- TODO: 3, 4, 5, etc.
  , memoizeExp, memoizeExp2 -- TOOD: 3, 4, 5, etc.
    -- * Traversals
    -- ** Transformations
  , everywhere, everywhere', everywhereBut
  , everywhereM, everywhereM', everywhereButM'
  , everywhereFor, everywhereForM
  , somewhere, somewhereM
    -- ** Queries
  , everything, everythingBut
  , everythingAccL, everythingAccL', everythingButAccL, everythingButAccL'
  , everythingAccR, everythingButAccR
  , everythingForR, everythingForL, everythingForL'
    -- * Extentions and adaptors
  , extN, extE, extE'
  , mkT, mkTs, mkQ, mkQs, mkM, mkMs
    -- * Type manipulation functions
  , eqType, eqTypes
  , inType, inTypes
  , constructorsOf, typeOfName
#if BENCHMARK
    -- * Benchmarking implementations
  , everything_slow
  , everywhereM_slow
#endif
) where

-- Imports for the 'seen' table in 'inType'
import Control.Monad.State
import Debug.Trace

-- Imports for memoization tables
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Imports for Template Haskell
import Language.Haskell.TH hiding (cxt{-avoid warnings when cxt is a variable-})
import Language.Haskell.TH.Syntax hiding (lift{-conflicts with monadic 'lift'-})

-- Import for listing the 'primitive types'.
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Array (Array)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)

-- Imports of other TYB modules
import Data.Generics.TH.Instances ()
import Data.Generics.TH.VarSet (varSet)

--------------------------------------------------------------------------------
-- thcase', thcase and thfoldl
--------------------------------------------------------------------------------

-- |Primitive case expression generation.  Most users will want to use
-- 'thcase' instead.
thcase' :: (Quasi m)
        => (Either Name {- prim :: t -}
                    (Name {- ctor :: a -> b -> t -}, [(Type, Name)] {- args :: [a, b] -})
             -> m Exp {- c t -})
        -- ^ Case handling function.  If the 'Type' being inspected is
        -- a primitive type, argument is @'Left' var@ where @var@ is a
        -- variable bound to the case discriminant.  Otherwise,
        -- argument is @'Right' (ctor, args)@ where @ctor@ is the
        -- constructor name and @args@ is a list of the argument's
        -- types and the variable bound to the argument.
        -> m Type -- ^ The type to inspect.
        -> m Exp  -- ^ The expression containing the @case@
                  -- expression.  If the type to inspect is @t@ and
                  -- the type of the 'Exp' returned by the case
                  -- handling function is @r@, the 'Exp' returned by
                  -- @thcase'@ is of type @t -> r@.
thcase' g t0 = do
  ctors <- constructorsOf =<< t0
  x <- qNewName "_x"
  body <- case ctors of
            Nothing -> g (Left x)
            Just ctors' -> return (CaseE (VarE x)) `ap` mapM doClause ctors'
  return $ LamE [VarP x] body
    where doClause (cons, ts) = do
            xs <- mapM (const $ qNewName "arg") ts --sequence [do x <- qNewName "arg"; return (t, x) | t <- ts]
            body <- g (Right (cons, zip ts xs))
            return $ Match (ConP cons (map VarP xs)) (NormalB body) []

-- |Case expression generation.  This is the core function of the
-- Template Your Boilerplate library.
-- 
-- This function is similar to @thcase'@, except that since most users
-- will note care about the distinction between types and primitive
-- types, this function smooths over the differences by treating primitive
-- types as types with nullary constructors.
thcase :: (Quasi m)
  => (m Exp -> [(Type, m Exp)] -> m Exp)
  -- ^ Case handling function.  The first argument is the constructor.
  -- The second argument is the list of arguments and their types.
  -> m Type -- ^ The type to inspect.
  -> m Exp  -- ^ The expression containing the @case@ expression.  If
            -- the type to inspect is @t@ and the type of the 'Exp'
            -- returned by the case handling function is @r@, the
            -- 'Exp' returned by @thcase@ is of type @t -> r@.
thcase g t0 = thcase' g' t0 where
  g' (Left var) = g (return (VarE var)) []
  g' (Right (name, args)) = g (return (ConE name)) [(t, return (VarE n)) | (t, n) <- args]

-- |Scrap Your Boilerplate style case expression generation.  The
-- 'thcase' function is generally simpler to use instead of this and
-- is more powerful.
thfoldl :: Quasi m
  => (m Exp -> Type -> m Exp -> m Exp)
  -- ^ Constructor argument application.  If the first 'Exp' is of
  -- type @c (a -> b)@, the 'Type' is @a@, and the second 'Exp' is of
  -- type @a@, should return an 'Exp' of type @c b@.
  -> (m Exp -> m Exp) -- ^ Constructor injection.  The argument 'Exp'
                      -- will be one of the constructors from the type
                      -- to be inspected.  If the argument 'Exp' is of
                      -- type @a@, should return an 'Exp' of type @c
                      -- a@.
  -> m Type -- ^ The type to inspect.
  -> m Exp  -- ^ The expression containing the @case@ expression.  If
            -- the type to inspect is @t@ and the type of the 'Exp'
            -- returned by the case handling function is @c t@, the
            -- 'Exp' returned by @thcase@ is of type @t -> c t@.
thfoldl k z t = thcase g t where
  g ctor args = foldl (uncurry . k) (z ctor) args

--------------------------------------------------------------------------------
-- Single-layer traversals
--------------------------------------------------------------------------------

-- |Generic single-layer transformation
thmapT :: (Quasi m)
  => (Type -> m Exp) -- ^ The transformation.  If the 'Type' is @t@, must
                     -- return an 'Exp' of type @t -> t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each immediate child.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> t0@.
thmapT f t0 = thcase g t0 where
  g ctor [] = ctor
  g ctor ((t, x) : xs) = do ctor' <- ctor
                            f' <- f t
                            x' <- x
                            g (return (AppE ctor' (AppE f' x'))) xs
                            -- g [|$(ctor) ($(f t) $(x))|] xs

-- |Generic single-layer query.
thmapQ :: (Quasi m)
  => (Type -> m Exp)  -- ^ The query.  Extracts data from the given
                      -- type.  If the 'Type' is @t@, must return an 'Exp'
                      -- of type @t -> a@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each immediate child.  If the 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> [a]@.
thmapQ query topTy = thcase g topTy where
  g _ctor [] = return (ConE '[])
  g ctor ((t, x) : xs) = do f' <- query t
                            rest <- g ctor xs
                            x' <- x
                            return (AppE (AppE (ConE '(:)) (AppE f' x')) rest)
                            -- [| $(f t) $(x) : $(g ctor xs) |]
                                   

-- |Generic single-layer query (right associative).
thmapQr :: (Quasi m)
  => m Exp            -- ^ Combining function.  'Exp' must have type @r' -> r -> r@
  -> m Exp            -- ^ Starting value.  'Exp' must have type @r@.
  -> (Type -> m Exp)  -- ^ The query.  Extract data from the given
                      -- type.  If the 'Type' is @t@, must return an 'Exp'
                      -- of type @t -> r'@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each immediate child and uses the
                       -- starting value and combining functions to
                       -- fold the query results.  If the 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r@.
thmapQr combine zero query t0 = thcase g t0 where
  g _ctor [] = zero
  g ctor ((t, x) : xs) = do combine' <- combine
                            left <- query t
                            x' <- x
                            right <- g ctor xs
                            return (AppE (AppE combine' (AppE left x')) right)
                            -- [|$(combine) ($(query t) $x) $(g ctor xs)|]

-- |Generic single-layer query (left associative).
thmapQl :: (Quasi m)
  => (m Exp)          -- ^ Combining function.  'Exp' must have type @r -> r' -> r@
  -> (m Exp)          -- ^ Starting value.  'Exp' must have type @r@.
  -> (Type -> m Exp)  -- ^ The query.  Extract data from the given
                      -- type.  If the 'Type' is @t@, must return an 'Exp'
                      -- of type @t -> r'@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each immediate child and uses the
                       -- starting value and combining functions to
                       -- fold the query results.  If the 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r@.
thmapQl combine zero query t0 = thcase g' t0 where
  g' _ctor xs = zero >>= \acc -> g acc xs
  g acc [] = return acc
  g acc ((t, x) : xs) = do combine' <- combine
                           left <- query t
                           x' <- x
                           g (AppE (AppE combine' acc) (AppE left x')) xs
                           --g [|$combine $acc ($(query t) $x)|] xs

#if BENCHMARK
thmapM_slow :: (Type -> Q Exp) -> Q Type -> Q Exp
thmapM_slow f t0 = thfoldl k z t0 where
  z ctor = [| return $(ctor) |]
  k ctor t arg =
    [| $ctor >>= \c -> $(f t) $(arg) >>= \a -> return (c a) |]
#endif

-- | Generic single-layer monadic transformation.
thmapM :: (Quasi m)
  => (Type -> m Exp) -- ^ The monadic transformation.  If the 'Type' is
                     -- @t@, must return an 'Exp' of type @t -> m t@
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- monadic transformation to each immediate
                       -- child.  If the 'Type' is @t0@, returns an 'Exp'
                       -- of type @t0 -> m t0@.
thmapM f t0 = thcase g t0 where
  g ctor [] = liftM (AppE (VarE 'return)) ctor -- [| return $(ctor) |]
  g ctor ((t, x) : xs) =
    do f' <- f t
       x' <- x
       xprime <- qNewName "x'"
       body <- g (do ctor' <- ctor; return (AppE ctor' (VarE xprime))) xs
       return $ InfixE (Just $ AppE f' x')
                       (VarE '(>>=))
                       (Just $ LamE [VarP xprime] body)
       -- [| $(f t) $(x) >>= \x' -> $(g [| $(ctor) x' |] xs) |]

--------------------------------------------------------------------------------
-- Memoization
--------------------------------------------------------------------------------

-- internal helper function
memoizeDec' :: (Quasi m, Ord a)
  => IORef (Map.Map a Name) -- ^ Name table.
  -> IORef [(Name, Exp)]    -- ^ Body table.
  -> (a -> m Exp)           -- ^ Function to be memoized.
  -> (a -> m Exp)           -- ^ Memoized version of the function.
memoizeDec' nameRef bodyRef f1 a = do
  names <- qRunIO (readIORef nameRef)
  case Map.lookup a names of
    Just name -> return $ VarE name
    Nothing -> do
      name <- qNewName $ "memoized" -- ++ map (\x -> if isAlpha x then x else '_') (pprint a)
      qRunIO $ modifyIORef nameRef (Map.insert a name)
      body <- f1 a
      qRunIO $ modifyIORef bodyRef ((name, body):)
      return $ VarE name

{-|

Memoizes a code generation function.  Most users will want to use
'memoizeExp' instead as it provides a simplified interface, but all
the notes about this function also apply to 'memoizeExp'.

We memoize a function returning an 'Exp' by creating a 'Dec' with a
body that is the 'Exp' returned by that function.  The return value
of the function is replaced with a 'VarE' that refers to the 'Dec'.
This allows functions like 'everywhere' to avoid infinite
recursions when they traverse recursive types like lists.

The memoization functions come in two flavors: 'memoizeDec' and
'memoizeExp'.  With 'memoizeDec' it is the responsibility of the
caller to place the 'Dec' in an appropriate place.  The
'memoizeExp' function automatically handles the 'Dec' by wrapping
them in a local 'LetE' form.

Every memoized function is passed a memoized version of itself.
This is the function that should be used in recursive calls.
Failing to do so will prevent those calls from being memoized.

Mutually recursive functions are possible using 'memoizeDec2',
etc. and 'memoizeExp2', etc.

If the function being memoized needs to accept multiple arguments,
then they must be packed into a tuple and passed as a single argument.

Effects in the @m@ monad are only performed the first time the
memoized function is called with a particular argument.  Subsequent
times the monad is simply the result of a 'return'.  Thus while it
is tempting to store extra return values in the monad, this should
be avoided due to the high likelihood of unexpected behavior.

Implementation Notes:

* Note that @m@ should not store a copy of the function, otherwise
  a memory leak is introduced.  It wouldn't even make sense to do
  it anyway since the results refer to expressions that might not
  be in scope.

* The memoized function stores a reference to the memoization
  table, Thus if a reference to the memoized function gets tucked
  inside @m@, then a memory leak can be introduced.  We could
  eliminate this leak by clearing and invalidating the table when
  'memoizeDec' returns.  To fully do this properly the table would
  have to be invalidated in such a way that the memoized version of
  the function would not continue to try populating the table if
  the user called it after 'memoizeDec' return.

* Conceptually we should use a State monad instead of an IORef but
  we choose IORef since we can embed IO operations in a Quasi
  without imposing extra restrictions on @m@.

* Other designs are possible.  This design was choosen for its
  simplicity of use.  The choice of memoization interface is
  largely orthogonal to the rest of this library.

* Type synonyms and kind annotations may lead to duplicate versions
  of the code (e.g. versions for both 'String' and @['Char']@)
  Usually this isn't a problem, but if it is, then the type
  synonyms should be expanded before each call to the memoized
  function.

* GADTs and data/type families haven't been considered in this
  code.  It is unknown whether they work.

Note that polymorphically recursive types (e.g. @data F a = N a | F (F
(Int, a))@) have an infinite number of types in them and thus despite
memoization this function will not terminate on those types.
-}

memoizeDec :: (Quasi m, Ord a)
  => ((a -> m Exp) -> a -> m Exp) -- ^ The function to memoize.  Takes
                                  -- a memoized version of the
                                  -- function as argument.
  -> a                            -- ^ The initial argument to the function.
  -> m ([Dec], Exp)               -- ^ The result of applying the
                                  -- function to the initial argument.
                                  -- The |Exp| is the result, but
                                  -- expects the @['Dec']@ to be in
                                  -- scope.
memoizeDec f1 a = do
  nameRef1 <- qRunIO $ newIORef Map.empty
  decsRef1 <- qRunIO $ newIORef []
  let f1' = memoizeDec' nameRef1 decsRef1 (f1 f1')
  expr <- f1' a
  decs1 <- qRunIO $ readIORef decsRef1
  return $ filterDecs decs1 expr

-- Unreferenced variables may cause ambiguous types (e.g. "foo = return")
-- that fun afoul of the monomorphism restriction.
-- TODO: filter out recursive definitions by using SCC
filterDecs :: [(Name, Exp)] -> Exp -> ([Dec], Exp)
filterDecs decs expr = (decs', expr) where
  decs' = map (\(name, body) -> ValD (VarP name) (NormalB body) []) $
          --decs
          filter (\x -> Set.member (fst x) vars) $ decs
  vars = Set.unions (varSet expr : map (varSet . snd) decs)

-- | Simultaneously memoizes two code generation functions.  All of
-- the notes about 'memoizeDec' also apply to this function.  Most
-- users will want to use 'memoizeExp2' instead of this function as it
-- provides a simplified interface.
memoizeDec2 :: (Quasi m, Ord a, Ord b)
  => ((a -> m Exp) -> (b -> m Exp) -> a -> m Exp)
  -- ^ The first function to memoize.  Takes memoized versions of the
  -- two functions as arguments.
  -> ((a -> m Exp) -> (b -> m Exp) -> b -> m Exp)
  -- ^ The second function to memoize.  Takes memoized versions of the
  -- two functions as arguments.
  -> a -- ^ The initial argument.
  -> m ([Dec], Exp) -- ^ The result of applying the function to the
                    -- initial argument.  The |Exp| is the result, but
                    -- expects the @['Dec']@ to be in scope.
memoizeDec2 f1 f2 a = do
  nameRef1 <- qRunIO $ newIORef Map.empty
  nameRef2 <- qRunIO $ newIORef Map.empty
  decsRef1 <- qRunIO $ newIORef []
  decsRef2 <- qRunIO $ newIORef []
  let f1' = memoizeDec' nameRef1 decsRef1 (f1 f1' f2')
      f2' = memoizeDec' nameRef2 decsRef2 (f2 f1' f2')
  expr <- f1' a
  decs1 <- qRunIO $ readIORef decsRef1
  decs2 <- qRunIO $ readIORef decsRef2
  return $ filterDecs (decs1 ++ decs2) expr

-- |Memoizes a code generation function.  Behaves identically to
-- 'memoizeDec' except that it returns a 'LetE' that binds the 'Dec'
-- resulting from 'memoizeDec' for the 'Exp' resulting from
-- 'memoizeDec'.
memoizeExp :: (Quasi m, Ord a)
  => ((a -> m Exp) -> a -> m Exp)
  -> a -> m Exp
memoizeExp f1 a = liftM (uncurry LetE) (memoizeDec f1 a)

-- |Simultaneously memoizes two code generation functions.  Behaves
-- identically to 'memoizeDec2' except that it returns a 'LetE' that
-- binds the 'Dec' resulting from 'memoizeDec2' for the 'Exp'
-- resulting from 'memoizeDec2'.
memoizeExp2 :: (Quasi m, Ord a, Ord b)
  => ((a -> m Exp) -> (b -> m Exp) -> a -> m Exp)
  -> ((a -> m Exp) -> (b -> m Exp) -> b -> m Exp)
  -> a -> m Exp
memoizeExp2 f1 f2 a = liftM (uncurry LetE) (memoizeDec2 f1 f2 a)

--------------------------------------------------------------------------------
-- Transform traversals
--------------------------------------------------------------------------------

-- |Generic recursive transformation (bottom-up)
everywhere :: (Quasi m)
  => (Type -> m Exp) -- ^ The transformation.  If the 'Type' is @t@, must
                     -- return an 'Exp' of type @t -> t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> t0@.
everywhere f t0 = do
	exp <- t0 >>= memoizeExp rec
	error $ show exp
  where rec r t = composeE (f t) (thmapT r (return t))

-- |Generic recursive transformation (top-down)
everywhere' :: (Quasi m)
  => (Type -> m Exp) -- ^ The transformation.  If the 'Type' is @t@, must
                     -- return an 'Exp' of type @t -> t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> t0@.
everywhere' f t0 = do
	exp <- t0 >>= memoizeExp rec
	return exp
  where rec r t = composeE (thmapT r (return t)) (f t)

-- |Generic recursive transformation (bottom-up) with selective traversal.
-- Skips traversal when a given query returns 'True'.
everywhereBut :: (Quasi m)
  => (Type -> m Bool)  -- ^ The query.  Should return 'True' when a
                       -- given type should not be traversed.
  -> (Type -> m Exp)   -- ^ The transformation.  If the 'Type' is @t@,
                       -- must return an 'Exp' of type @t -> t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant (except
                       -- for parts skipped due to the query returning
                       -- 'True').  If the 'Type' is @t0@, returns an
                       -- 'Exp' of type @t0 -> t0@.
everywhereBut q f t0 = t0 >>= memoizeExp rec where
  rec k t = do q' <- q t
               if q' then return (VarE 'id)
                     else composeE (f t) (thmapT k (return t))

-- |Generic recursive monadic transformation (bottom-up)
everywhereM :: (Quasi m)
  => (Type -> m Exp) -- ^ The monadic transformation.  If the 'Type' is @t@, must
                     -- return an 'Exp' of type @t -> m t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> m t0@.
everywhereM f t0 = t0 >>= memoizeExp rec where
  rec r t = composeM (f t) (thmapM r (return t))

#if BENCHMARK
everywhereM_slow ::
  (Type {- forall a. -} -> Q Exp {- a -> a -})
  ->  Q Type {- forall a. -} -> Q Exp {- a -> a -}
everywhereM_slow f t0 = t0 >>= memoizeExp rec where
  rec k t = composeM (f t) (thmapM_slow k (return t))
#endif

-- | Generic recursive monadic transformation (top-down)
everywhereM' :: (Quasi m)
  => (Type -> m Exp) -- ^ The monadic transformation.  If the 'Type' is @t@, must
                     -- return an 'Exp' of type @t -> m t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> m t0@.
everywhereM' f t0 = t0 >>= memoizeExp rec where
  rec k t = composeM (thmapM k (return t)) (f t)


-- |Generic recursive monadic transformation (top-down) with selective traversal.
-- Skips traversal when a given query returns 'True'.
everywhereButM' :: (Quasi m)
  => (Type -> m Bool)  -- ^ The query.  Should return 'True' when a
                       -- given type should not be traversed.
  -> (Type -> m Exp)   -- ^ The monadic transformation.  If the 'Type' is @t@,
                       -- must return an 'Exp' of type @t -> m t@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- monadic transformation to each descendant (except
                       -- for parts skipped due to the query returning
                       -- 'True').  If the 'Type' is @t0@, returns an
                       -- 'Exp' of type @t0 -> m t0@.
everywhereButM' q f t0 = t0 >>= memoizeExp rec where
  rec k t = do q' <- q t
               if q' then return (VarE 'return)
                     else composeM (thmapM k (return t)) (f t)

-- |Generic recursive transformation (bottom-up) with selective
-- traversal.  Recurs on only types that can contain a type with type
-- specific behavior.
everywhereFor :: (Quasi m)
  => Name              -- ^ Name of a function of type @t -> t@
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- transformation to each descendant that is of
                       -- type @t@.  If the 'Type' is @t0@, returns an
                       -- 'Exp' of type @t0 -> t0@.
everywhereFor func topTy = do
  everywhereBut (liftM not . inType (argTypeOfName func))
                (const (return (VarE 'id)) `extE`
                 (eqType (argTypeOfName func), return (VarE func))) topTy

-- |Generic recursive monadic transformation (bottom-up) with
-- selective traversal.  Recurs on only types that can contain a type
-- with type specific behavior.
everywhereForM :: (Quasi m)
  => Name              -- ^ Name of a function of type @t -> m t@
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the
                       -- monadic transformation to each descendant
                       -- that is of type @t@.  If the 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> m t0@.
everywhereForM func topTy = do
  everywhereButM' (liftM not . inType (argTypeOfName func))
                  (const (return (VarE 'return)) `extE`
                   (eqType (argTypeOfName func), return (VarE func))) topTy

{-
This doesn't work because of the following:

        Dec <---+
       /  \     |
     ...  ...   |
     /      \   |
 [Needed]   Exp-+

It is impossible to know when processing Exp
whether [Needed] will be hit because answering
that requires processing all of the children of
Dec which then requires Exp, etc.

Memoization does not help this problem.

-- skip useless recursions
-- Names: everywhereNeeded, everywhereCan, ???
-- effective, useful, isJust, May, Can
-- We use a state monad instead of a writer monad
-- because the memoization doesn't replay old effects
everywhere_ :: (Quasi m)
 => (Type {- forall a. -} -> m (Maybe Exp) {- a -> a -})
            -> m Type {- forall a. -} -> m Exp {- a -> a -}
everywhere_ f t = do t' <- t; (e, _) <- runStateT (memoizeExp rec t') (Set.empty, Set.empty); return e where
  --rec :: (Type -> WriterT Any Q Exp) -> Type -> WriterT Any Q Exp
  rec k t = do r <- thmapT k' (return t)
               f' <- lift $ f t
               when (isJust f') (setSelf t)
               needsChildren <- needChildren t
               case (needsChildren, f') of
                 (False, Nothing) -> return (VarE 'id)
                 (True, Nothing) -> return r
                 (False, Just f'') -> return f''
                 (True, Just f'') -> lift $ composeE (return f'') (return r)
    where k' t' = do e <- k t'
                     needSelf t' >>= \x -> when x (setChildren t)
                     return e
          needSelf t = gets (Set.member t . fst)
          needChildren t = gets (Set.member t . snd)
          setSelf t = modify (\(s1, s2) -> (Set.insert t s1, s2))
          setChildren t = modify (\(s1, s2) -> (s1, Set.insert t s1))

everywhereM'_ :: (Quasi m)
 => (Type {- forall a. -} -> m (Maybe Exp) {- a -> a -})
            -> m Type {- forall a. -} -> m Exp {- a -> a -}
everywhereM'_ f t = do t' <- t; (e, s) <- runStateT (memoizeExp rec t') (Set.empty, Set.empty); trace (show (Set.toList (fst s), Set.toList (snd s))) $ return e where
  --rec :: (Type -> WriterT Any Q Exp) -> Type -> WriterT Any Q Exp
  rec1 k t = do f' <- lift $ f t
                when (isJust f') (setSelf t f')
                thmapM k (return t)
  rec2 k t = do r <- thmapM k' (return t)
                f' <- lift $ f t
                needsChildren <- needChildren t
                when needsChildren (setSelf t)
                -- TODO: requires "Just"ness of "f" to be idempotent
    where k' t' = needSelf t' >>= \x -> when x (setChildren t) >> k t'

{-
  rec k t = do r <- thmapM k' (return t)
               f' <- lift $ f t
               needsChildren <- needChildren t
               trace ("     ******     " ++ show (isJust f') ++ show needsChildren ++ show t) $ return ()
               when (isJust f' || needsChildren) (setSelf t)
               case (needsChildren, f') of
                 (False, Nothing) -> return (VarE 'return)
                 (True, Nothing) -> return r
                 (False, Just f'') -> return f''
                 (True, Just f'') -> lift $ composeM (return r) (return f'')
    where k' t' = do e <- k t'
                     needSelf t' >>= \x -> when x (setChildren t)
                     return e
          needSelf t = gets (Set.member t . fst)
          needChildren t = gets (Set.member t . snd)
          setSelf t = modify (\(s1, s2) -> (Set.insert t s1, s2))
          setChildren t = modify (\(s1, s2) -> (s1, Set.insert t s1))
-}
-}

{-
-- alternative interface to everywhereNeeded
everywhereBut' :: (Quasi m)
  => (Type -> m Bool)
  -> (Type {- forall a. -} -> m Exp {- a -> a -})
  ->  Type {- forall a. -} -> m Exp {- a -> a -}
everywhereBut' q f t0 = do (e, _) <- runWriterT (memoizeExp rec t0); return e where
--  rec :: (Quasi m) => (Type -> WriterT Any m Exp) -> Type -> WriterT Any m Exp
  rec k t = do (r, Any childRec) <- listen (thmapT k (return t))
               q' <- lift $ q t
               tell (Any (not q'))
               case (childRec, q') of
                 (False, True) -> return (VarE 'id)
                 (True, True) -> return r
                 (False, False) -> lift $ f t
                 (True, False) -> lift $ composeE (return r) (f t)
-}


-- |Generic recursive transformation (bottom-up) with selective traversal.
somewhere :: (Quasi m)
  => ((Type -> m Exp) -> (Type -> m (Maybe Exp)))
  -- ^ The transformation.  The first argument is the memoized
  -- recursion.  If 'Nothing' is returned, then the standard,
  -- automatic recursion is done.  If 'Just' is returned, then no
  -- automatic recursion is done and the resulting 'Exp' is used at
  -- that type.  In that case, if further recursion is desired, then
  -- the expression should include a call to the memoized recursion.
  -- If the 'Type' is @t@, then the returned 'Exp' must be of type @t
  -- -> t@.
  --
  -- We use 'Maybe' instead of 'MonadPlus' to avoid the user having to
  -- play games with 'runMaybeT' and so forth.
  -> (m Type -> m Exp)
  -- ^ Generates an 'Exp' that applies the transformation to each
  -- descendant.  If 'Type' is @t0@, returns an 'Exp' of type @t0 ->
  -- t0@.
somewhere f t0 = t0 >>= memoizeExp rec where
  rec k t = do
    f' <- f k t
    case f' of
      Nothing -> thmapT k (return t) -- `mplus` return (VarE 'id)
      Just e -> return e

-- |Generic recursive monadic transformation (bottom-up) with selective traversal.
somewhereM :: (Quasi m)
  => ((Type -> m Exp) -> (Type -> m (Maybe Exp)))
  -- ^ The monadic transformation.  The first argument is the memoized
  -- recursion.  If 'Nothing' is returned, then the standard,
  -- automatic recursion is done.  If 'Just' is returned, then no
  -- automatic recursion is done and the resulting 'Exp' is used at
  -- that type.  In that case, if further recursion is desired, then
  -- the expression should include a call to the memoized recursion.
  -- If the 'Type' is @t@, then the returned 'Exp' must be of type @t
  -- -> m t@.
  --
  -- We use 'Maybe' instead of 'MonadPlus' to avoid the user having to
  -- play games with 'runMaybeT' and so forth.
  -> (m Type -> m Exp)
  -- ^ Generates an 'Exp' that applies the transformation to each
  -- descendant.  If 'Type' is @t0@, returns an 'Exp' of type @t0 ->
  -- m t0@.
somewhereM f t0 = t0 >>= memoizeExp rec where
  rec k t = do
    f' <- f k t
    case f' of
      Nothing -> thmapM k (return t)
      Just e -> return e

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- |Generic recursive query (bottom-up).
everything :: (Quasi m)
  => m Exp            -- ^ Combining function.  'Exp' must have type @r -> r -> r@.
  -> (Type -> m Exp)  -- ^ The query.  Extract data from the given
                      -- type.  If the 'Type' is @t@, must return an 'Exp'
                      -- of type @t -> r'@.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each descendant and uses the combining
                       -- function to combine the query results.  If
                       -- the 'Type' is @t0@, returns an 'Exp' of type
                       -- @t0 -> r@.
everything collect query t0 = t0 >>= memoizeExp rec where
  rec r t = do
    --[|\x -> $(thmapQl collect [|$(query t) x|] r (return t)) x|]
    x <- qNewName "x"
    f <- query t
    mapQ <- thmapQl collect (return (AppE f (VarE x))) r (return t)
    return (LamE [VarP x] (AppE mapQ (VarE x)))

#if BENCHMARK
everything_slow :: (Quasi m)
              => (m Exp)           -- Combine collections, expr :: c a -> c a -> c a
              -> (Type -> m Exp)   -- Extract values,      expr :: d -> c a
              -> m Type            -- The top type, the first 'd'.
              -> m Exp             -- expr :: d -> c a
everything_slow collect query topTy = collect >>= \cExp -> topTy >>= memoizeExp (ething cExp)
  where
    ething cExp memoF currTy = do
      f_curr <- query currTy
      f_rest <- thmapQ memoF (return currTy)
      x <- qNewName "x"
      return (LamE [VarP x]
              (AppE
               (AppE (AppE (VarE 'foldl) cExp)
                      (AppE f_curr (VarE x)))
                      (AppE f_rest (VarE x))))
#endif

-- |Generic recursive query with left-associative accumulation.
everythingAccL
  :: (Type -> Q Exp) -- ^ The query and combining function.  If the
                     -- 'Type' is @t@, must return an 'Exp' of type @t
                     -- -> r -> r@.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingAccL query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      e <- query currTy
      [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Left accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = [|$(memoF t) $v $(g acc ctor vs)|]

-- |Generic recursive query with strict left-associative accumulation
everythingAccL'
  :: (Type -> Q Exp) -- ^ The query and combining function.  If the
                     -- 'Type' is @t@, must return an 'Exp' of type @t
                     -- -> r -> r@.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingAccL' query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      e <- query currTy
      [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Left accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = [|$acc `seq` $(memoF t) $v $(g acc ctor vs)|]


-- |Generic recursive query with left-associative accumulation and selective traversal
everythingButAccL
  :: (Type -> Q (Exp, Bool))
  -- ^ The query, combining, selectivity function.  If the 'Type' is
  -- @t@, must return an 'Exp' of type @t -> r -> r@.  If the 'Bool'
  -- is 'True' the traversal does not proceed further into the
  -- recursion.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingButAccL query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      (e, stop) <- query currTy
      if stop then return e
              else [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Left accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = [|$(memoF t) $v $(g acc ctor vs)|]

-- |Generic recursive query with strict left-associative accumulation and selective traversal
everythingButAccL'
  :: (Type -> Q (Exp, Bool))
  -- ^ The query, combining, selectivity function.  If the 'Type' is
  -- @t@, must return an 'Exp' of type @t -> r -> r@.  If the 'Bool'
  -- is 'True' the traversal does not proceed further into the
  -- recursion.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingButAccL' query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      (e, stop) <- query currTy
      if stop then return e
              else [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Left accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = [|$acc `seq` $(memoF t) $v $(g acc ctor vs)|]

-- |Generic recursive query with right-associative accumulation
everythingAccR
  :: (Type -> Q Exp) -- ^ The query and combining function.  If the
                     -- 'Type' is @t@, must return an 'Exp' of type @t
                     -- -> r -> r@.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingAccR query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      e <- query currTy
      [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Right accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = g [|$(memoF t) $v $acc|] ctor vs

-- |Generic recursive query with right-associative accumulation and selective traversal
everythingButAccR
  :: (Type -> Q (Exp, Bool))
  -- ^ The query, combining, selectivity function.  If the 'Type' is
  -- @t@, must return an 'Exp' of type @t -> r -> r@.  If the 'Bool'
  -- is 'True' the traversal does not proceed further into the
  -- recursion.
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r -> r@
everythingButAccR query t0 = t0 >>= memoizeExp rec
  where
    rec memoF currTy = do
      (e, stop) <- query currTy
      if stop then return e
              else [|\x acc -> $(return e) x ($(thcase (g [|acc|]) (return currTy)) x)|]
      where -- Right accumulator
            g acc _ctor [] = acc
            g acc ctor ((t, v):vs) = g [|$(memoF t) $v $acc|] ctor vs

-- |Generic recursive query with selective traversal
everythingBut :: (Quasi m)
  => (m Exp) -- ^ Combining function.  'Exp' must have type @r -> r -> r@.
  -> (Type -> m (Exp,Bool))
  -- ^ The query, combining, selectivity function.  If the 'Type' is
  -- @t@, must return an 'Exp' of type @t -> r -> r@.  If the 'Bool'
  -- is 'True' the traversal does not proceed further into the
  -- recursion.
  -> (m Type -> m Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant.  If 'Type' is @t0@,
                       -- returns an 'Exp' of type @t0 -> r@
everythingBut collect query topTy = topTy >>= memoizeExp rec
  where
   rec memoF currTy = do
     x <- qNewName "x"
     (f_curr,b) <- query currTy
     if b then return f_curr
          else do mapQ <- thmapQl collect (return (AppE f_curr (VarE x))) memoF (return currTy)
                  return (LamE [VarP x] (AppE mapQ (VarE x)))

-- |Generic recursive traversal using left-associative accumulation
everythingForL
  :: Name -- ^ Name of a function of type @t -> r -> r@
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant that is of type @t@.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> r -> r@
everythingForL func topTy = do
    a1 <- argTypeOfName func
    let op  = VarE func
        g t = do
         b <- eqType (return a1) t
         c <- inType (return a1) t
         if b then return (op, not c)
              else return (AppE (VarE 'const) (VarE 'id), not c)
    everythingButAccL g topTy

-- |Generic recursive traversal using strict left-associative accumulation
everythingForL'
  :: Name -- ^ Name of a function of type @t -> r -> r@
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant that is of type @t@.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> r -> r@
everythingForL' func topTy = do
    a1 <- argTypeOfName func
    let op  = VarE func
        g t = do
         b <- eqType (return a1) t
         c <- inType (return a1) t
         if b then return (op, not c)
              else return (AppE (VarE 'const) (VarE 'id), not c)
    everythingButAccL' g topTy

-- |Generic recursive traversal using right-associative accumulation
everythingForR
  :: Name -- ^ Name of a function of type @t -> r -> r@
  -> (Q Type -> Q Exp) -- ^ Generates an 'Exp' that applies the query
                       -- to each decendant that is of type @t@.  If
                       -- 'Type' is @t0@, returns an 'Exp' of type @t0
                       -- -> r -> r@
everythingForR func topTy = do
    a1 <- argTypeOfName func
    let op  = VarE func
        g t = do
         b <- eqType (return a1) t
         c <- inType (return a1) t
         if b then return (op, not c)
              else do h <- [|const id|]
                      return (h, not c)
    everythingButAccR g topTy

--------------------------------------------------------------------------------
-- implementing "mkT" and friends
--------------------------------------------------------------------------------

{-

In general "mkT" is simply a partially applied "extT" as in the
following thus we focus the description on "extT".

mkT :: Type -> ExpQ -> (Type -> ExpQ)
mkT = extT (const [|id|])

The naive implementation of "extT" simply tests for type equality as
in the following.  However, this would fail to treat "String" and
"[Char]" as equal.

extT :: (Type -> ExpQ) -> Type -> ExpQ -> Type -> ExpQ
extT def t ext t' | t' == t = ext
                  | otherwise = def t'

-}

{-

Since there are multiple notions of type equality (e.g. by name, exact
equality, instantiability, etc.), we do not write an "extT" version
for each.  Instead we parameterize by the equality predicate.

We pair the predicate with the function to use when that predicate is
true rather than passing them as separate arguments so that "extT" is
easier to use infix.  An alternative would be to play with the
precedence.

-}

-- |Returns the type of a variable, method or constructor name.
typeOfName :: (Quasi m) => Name -> m Type
typeOfName n = do
  info <- qReify n
  case info of
    DataConI _ ty _ _ -> return ty
    VarI _ ty _ _     -> return ty
    ClassOpI _ ty _ _ -> return ty
    _                 -> fail $ "'typeOfName' only applies to classes, variables, " ++
                                "and constructors that are in the current environment, but " ++
                                "was applied to :" ++ show n

-- |Returns the type of the first argument of a variable, method or constructor name.
argTypeOfName :: (Quasi m) => Name -> m Type
argTypeOfName n = do
  t <- typeOfName n
  let getArg (AppT (AppT ArrowT t') _) = return t'
      getArg (ForallT _ _ t') = getArg t'
      getArg _ = fail $ "'argTypeOfName' applied to `" ++ show n ++
                        " which has non-function type `" ++ show t ++ "'."
  getArg t

-- |Extends a generic operation with type specific behavior based on the type of the given name.
extN :: (Quasi m)
 => (Type -> m Exp) -- ^ The operation to be extended.
 -> Name            -- ^ Name of the function implementing the type specific behavior.
 -> (Type -> m Exp)
 -- ^ The result of extending the operation.  If the 'Name' has type
 -- @t -> s@, then the extended operation has type specific behavior
 -- at @t@.  At other types it behaves as the original operation.
extN def name t0 =
  extE def (eqType t', return (VarE name)) t0
    where t' = do ty <- typeOfName name
                  ty' <- argTypeOfName name
                  when (hasFreeVars ty') $ fail $
                         "Underspecified type when using extN (or something based on it).\n" ++
                         "    A type variable or forall occurs in the type of the first argument to `" ++ show name ++ "'.\n" ++
                         "    Namely, `" ++ pprint ty' ++ "'\n" ++
                         "    in the type `"++ pprint ty++"'."
                  return ty'

          hasFreeVars (VarT _) = True
          hasFreeVars (ForallT _tyVarBndrs _cxt _ty) = True
          hasFreeVars (ConT _) = False
          hasFreeVars (TupleT _) = False
          hasFreeVars (UnboxedTupleT _) = False
          hasFreeVars (ArrowT) = False
          hasFreeVars (ListT) = False
          hasFreeVars (AppT t1 t2) = hasFreeVars t1 || hasFreeVars t2
          hasFreeVars (SigT t _kind) = hasFreeVars t

-- |Extends a generic operation with type specific behavior.
extE :: (Quasi m)
  => (Type -> m exp) -- ^ The operation to be extended.
  -> (Type -> m Bool, m exp)
  -- ^ The 'fst' of the pair should return 'True' on types for which
  -- the operation should be extended.  The 'snd' of the pair is the
  -- expression to use on those types.
  -> (Type -> m exp) -- ^ The result of extending the operation.
extE def (typePred, ext) t = extE' def (typePred, const ext) t

-- |Extends a generic operation with type specific behavior.
extE' :: (Quasi m)
 => (Type -> m exp) -- ^ The operation to be extended.
 -> (Type -> m Bool, Type -> m exp)
  -- ^ The 'fst' of the pair should return 'True' on types for which
  -- the operation should be extended.  The 'snd' of the pair when given one of these 'Type's
  -- should return the expression to use on that type.
 -> (Type -> m exp) -- ^ The result of extending the operation.
extE' def (typePred, ext) t = do test <- typePred t
                                 if test then ext t else def t

-- |Makes a transformation from a named function.
mkT :: (Quasi m)
  => Name            -- ^ Name of a function of type @t -> t@
  -> (Type -> m Exp) -- ^ The generic transformation.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> t0@.
                     -- The 'Exp' is the named function at @t@, and
                     -- 'id' elsewhere.
mkT f = mkTs [f]

-- |Makes a transformation from several named functions.
mkTs :: (Quasi m)
  => [Name]          -- ^ Names of functions of type @t -> t@ for various @t@.
  -> (Type -> m Exp) -- ^ The generic transformation.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> t0@.
                     -- If one of the named functions matches @t0@,
                     -- then 'Exp' is that function.  Otherwise, it is
                     -- 'id'.
mkTs = mkXs (return (VarE 'id))

-- |Makes a monadic transformation from a named function.
mkM :: (Quasi m)
  => Name            -- ^ Name of a function of type @t -> m t@
  -> (Type -> m Exp) -- ^ The generic monadic transformation.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> m t0@.
                     -- The 'Exp' is the named function at @t@, and
                     -- 'return' elsewhere.
mkM f = mkMs [f]

-- |Makes a monadic transformation from several named functions.
mkMs :: (Quasi m)
  => [Name]          -- ^ Names of functions of type @t -> m t@ for various @t@.
  -> (Type -> m Exp) -- ^ The generic monadic transformation.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> m t0@.
                     -- If one of the named functions matches @t0@,
                     -- then 'Exp' is that function.  Otherwise, it is
                     -- 'return'.
mkMs = mkXs (return (VarE 'return))

-- |Makes a query from a named function.
mkQ :: (Quasi m)
  => m Exp           -- ^ Default value to return on types other than @t@.
                     -- The 'Exp' must be of type @r@.
  -> Name            -- ^ Name of a function of type @t -> r@
  -> (Type -> m Exp) -- ^ The generic transformation.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> r@.
                     -- The 'Exp' is the named function at @t@, and
                     -- the provided default value elsewhere.
mkQ z f t = mkQs z [f] t

-- |Makes a query from several named functions.
mkQs :: (Quasi m)
  => m Exp           -- ^ Default value to return on types that do not
                     -- match the @t@ from any named function.  The
                     -- 'Exp' must be of type @r@.
  -> [Name]          -- ^ Names of functions of type @t -> r@ for various @t@.
  -> (Type -> m Exp) -- ^ The generic query.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @t0 -> r@.
                     -- If one of the named functions matches @t0@,
                     -- then 'Exp' is that function.  Otherwise, it is
                     -- 'const' of the provided default value.
mkQs zM = mkXs (zM >>= \z -> return (AppE (VarE 'const) z))

-- |Makes operations from several named functions.
mkXs :: (Quasi m)
  => m Exp           -- ^ Default function for types that do not
                     -- match the @t@ from any named function.  The
                     -- 'Exp' must be of type @c t@.
  -> [Name]          -- ^ Names of functions of type @c t@ for various @t@.
  -> (Type -> m Exp) -- ^ The generic query.  If the 'Type'
                     -- is @t0@, then the 'Exp' has type @c t0@.
                     -- If one of the named functions matches @t0@,
                     -- then 'Exp' is that function.  Otherwise, it is
                     -- the provided default function.
mkXs zM []     = const zM
mkXs zM (f:fs) = extN (mkXs zM fs) f


{- TODO:

-- name vs expr
-- for expr: type predicate vs not
-- single vs plural

-- extF appears pointless though it I think it is actually useful (thus omit from paper?)
extF :: ( Type{-|c|-}-> Q Exp{-|c -> h c|-})
     -> (Q Type{-|b|-}, Type -> Q Exp{-|b -> h b|-})
     -> Type{-|a|-} -> Q Exp{-|a -> h a|-}

extNs :: (Type{-|c|-}-> Q Exp{-|c -> h c|-})
     ->                  [Name]{-|b -> h b|-}
     -> Type{-|a|-}-> Q Exp{-|a -> h a|-}
extEs :: (Type -> Q Exp)
     -> ([Q Type], Q Exp)
     -> Type -> Q Exp

extFs :: (Type -> Q Exp)
     -> ([Q Type], Type -> Q Exp)
     -> Type -> Q Exp


ext? :: (Quasi m)
      => (Type -> m exp)
      -> (Q Type, m exp)
      -> Type -> m exp

ext?s :: (Quasi m)
      => (Type -> m exp)
      -> ([Q Type], m exp)
      -> Type -> m exp

ext? :: (Quasi m)
      => (Type -> m exp)
      -> (Q Type, Type -> m exp)
      -> Type -> m exp

--TODO: extT' that doesn't take a type in the "ext"
extT' :: (Quasi m)
      => (Type -> m exp)
      -> (Type -> m Bool, m exp)
      -> Type -> m exp
extT' = extE

-}

--------------------------------------------------------------------------------
-- Type equality and containment
--------------------------------------------------------------------------------

-- |Tests if two types are equal modulo type synonyms and kind
-- annotations.  Naive equality would fail to equate "String" and
-- "[Char]".
eqType :: (Quasi m) => m Type -> Type -> m Bool
eqType t1 t2 = do t1' <- expandType =<< t1
                  t2' <- expandType t2
                  return $ t1' == t2'

-- |Test if any of a list of types is equal to a particular type
-- modulo type synonyms and kind annotations.  Useful when multiple
-- types share the same type-specific behavior.
eqTypes :: (Quasi m) => [m Type] -> Type -> m Bool
eqTypes ts t = liftM or $ mapM (flip eqType t) ts

-- |@inType t1 t2 = True@ iff @t1@ is (even recursively) inside @t2@
inType :: (Quasi m) => m Type -> Type -> m Bool
inType t1_0 t2_0 =
  evalStateT (flip rec t2_0 =<< lift (expandType =<< t1_0)) Set.empty where
    rec t1 t2 = do
      t2' <- expandType t2
      s <- gets (Set.member t2')
      if s then return False -- We've already seen and checked this type
        else if t1 == t2' then return True -- We found a matching type
        else do modify (Set.insert t2') -- Remember that we were here
                ctors <- constructorsOf t2' -- We need to recur on the constructors
                case ctors of
                  Nothing -> return False -- It's a function or primitive type
                  Just ctors' -> check t1 (concatMap snd ctors') -- Now we recur
    check _t1 [] = return False
    check t1 (t : ts) = do
      t' <- rec t1 t
      if t' then return True else check t1 ts

-- |@inTypes ts t2 = True@ iff any of @ts@ is (even recursively) inside @t2@
inTypes :: (Quasi m) => [m Type] -> Type -> m Bool
inTypes t1qs t2 = fmap or (mapM (`inType` t2) t1qs)

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

----------------------------------------
-- Constructor queries
----------------------------------------

-- We treat these types as primitive.  They were generated from the list of
-- instances of the Data.Data class that have unboxed arguments.
primTypes :: [Name]
primTypes = [
 ''Char,
 ''Double,
 ''Float,
 ''Int,
 ''Int8,
 ''Int16,
 ''Int32,
 ''Int64,
 ''Integer,
 ''Word,
 ''Word8,
 ''Word16,
 ''Word32,
 ''Word64,
 ''Ptr,
 ''ForeignPtr,
 ''Array]

-- |Returns the constructors of a given type.
-- Returns @Nothing@ if the type is primitive.
constructorsOf :: (Quasi m) => Type -> m (Maybe [(Name, [Type])])
constructorsOf t = do
  c <- getCtors t []
  case c of
    Left _ -> return $ Nothing
    Right c' -> return $ Just (filter (not . isPrimConstructor) $ map f c')
  where f (name, ts) = (name, map snd ts)

isPrimConstructor :: (Name,[Type]) -> Bool
isPrimConstructor (n,args) = or $ map isPrim args

isPrim :: Type -> Bool
isPrim (ConT n) = last (nameBase n) == '#'
isPrim _ = False

-- getCtors is used to implement constructorsOf.
-- 
-- getCtors returns all the information about the constructors of the
-- type.  In constructorsOf, we limit our selves to the useful
-- information.
-- 
-- getCtors must dive into the lhs of type applications app to get
-- constructors and then substitute appropriately
getCtors :: (Quasi m) => Type -> [Type] -> m (Either (Int, Bool) [(Name, [(Strict, Type)])])
getCtors (ForallT _tyVarBndrs _cxt ty) [] = getCtors ty [] -- needs in-scope variables?
getCtors (VarT name) _ = fail $ "constructorsOf: type variable `"++show name++"' in head position of type application"
getCtors (ConT name) _args | name `elem` primTypes || or (map isPrim (ConT name:_args)) = return $ Left (0, False)
getCtors (ConT name) args = do
-- TODO: check arg count
  t <- qReify name
  case t of
    TyConI (DataD _cxt _tyName tyVarBndrs cons _derive) ->
      return $ Right $ map (doCons tyVarBndrs) cons
    TyConI (NewtypeD _cxt _tyName tyVarBndrs con _derive) ->
      return $ Right [doCons tyVarBndrs con]
    TyConI (TySynD _tyName tyVarBndrs ty) ->
      getCtors (subst (zip tyVarBndrs args) ty) (drop (length tyVarBndrs) args)
    PrimTyConI _name arity isUnLifted -> return $ Left (arity, isUnLifted)
      -- PrimTyConI handles types that can't be expressed with a data type such as (->), Int#
    _ -> fail $ "Attempt to call `constructorsOf' on non-type: " ++ (show t)
  where doCons bndrs (NormalC name' ts) =
          (name', [(strict, subst (zip bndrs args) t) | (strict, t) <- ts])
        doCons bndrs (InfixC t1 name' t2) =
          doCons bndrs (NormalC name' [t1, t2])
        doCons bndrs (RecC name' ts) =
          doCons bndrs (NormalC name' (map (\ (_, s, t) -> (s, t)) ts))
        --doCons bndrs (ForallC tyVarBndrs cxt con) = fail $ "GADT constructor found in "++show name++", but GADTs are not (yet) supported"
        -- TODO: gadt in ForallC
--        foo (ClassP name' tys) = ...
--        foo (EqP ty1 ty2)
  -- data Foo a b c = forall d e f. (a ~ b, c ~ Int) => Foo1 a b e f
getCtors (TupleT n) args | n == length args =
  return $ Right [(tupleDataName n, [(NotStrict, t) | t <- args])]
getCtors (ArrowT) [_t1, _t2] = return $ Left (2, False) -- constants taken from "reify ''(->)"
getCtors (ListT) [t] =
  return $ Right [('[], []), ('(:), [(NotStrict, t), (NotStrict, AppT ListT t)])]
getCtors (AppT t1 t2) args = getCtors t1 (t2 : args)
getCtors (SigT t _kind) args = getCtors t args
getCtors t args =
  fail $ "constructorsOf: wrong number of arguments passed to type constructor\n" ++
         "    Constructor: " ++ show t ++ "\n" ++
         "    Arguments: " ++ show args ++ "\n"

-- Apply a substitution (a.k.a. a list of type-variable bindings) to a type.
--
-- Note: Predicates might be able to be simplified as a result of the
-- substitution, but this function does not perform that
-- simplification.
subst :: [(TyVarBndr, Type)] -> Type -> Type
subst env0 t0 = subst' [(stripKind bndr, Just t) | (bndr, t) <- env0] t0 where
  stripKind (PlainTV name) = name
  stripKind (KindedTV name _kind) = name

  subst' :: [(Name, Maybe Type)] -> Type -> Type
  subst' env (ForallT tyVarBndrs cxt t) =
    ForallT tyVarBndrs
              (map (substPred env) cxt)
              (subst' ([(stripKind bndr, Nothing) | bndr <- tyVarBndrs] ++ env) t)
  subst' env t@(VarT name) | Just (Just t') <- lookup name env = t'
                           | otherwise = t
  subst' env (AppT t1 t2) = AppT (subst' env t1) (subst' env t2)
  subst' env (SigT t kind) = SigT (subst' env t) kind
  subst' _ t@(ConT _) = t
  subst' _ t@(TupleT _) = t
  subst' _ t@(UnboxedTupleT _) = t
  subst' _ t@(ArrowT) = t
  subst' _ t@(ListT) = t

  substPred env (ClassP name ts) = ClassP name (map (subst' env) ts)
  substPred env (EqualP t1 t2) = EqualP (subst' env t1) (subst' env t2)

----------------------------------------
-- Type expansion
----------------------------------------

-- | Expands all type synonyms in its argument.  Also strips all kind
-- annotations since those might be incorrect once type synonyms are
-- applied to their arguments.
expandType :: (Quasi m) => Type -> m Type
expandType t = expandType' t []

-- most of these clauses can be written cleaner using [t| ... |] quotes
-- but then we couldn't use "Quasi m" and would only work on "Q".
--
-- This code does not check arity and may produce unexpected results
-- in the presence of partially applied type synonyms.
expandType' :: (Quasi m) => Type -> [Type] -> m Type
expandType' (ForallT tyVarBndrs cxt ty) args = do
  cxt' <- mapM doCxt cxt
  ty' <- expandType' ty []
  let t' = ForallT tyVarBndrs cxt' ty'
  return $ foldl AppT t' args
  where doCxt (ClassP name tys) = liftM (ClassP name) (mapM (flip expandType' []) tys)
        doCxt (EqualP t1 t2) = liftM2 EqualP (expandType' t1 []) (expandType' t2 [])
expandType' (VarT name) args = return $ foldl AppT (VarT name) args
expandType' (ConT name) args = do
  t <- qReify name
  let nonSynonym = return $ foldl AppT (ConT name) args
  case t of
    TyConI (TySynD _name tyVarBndrs ty) ->
      expandType' (subst (zip tyVarBndrs args) ty) (drop (length tyVarBndrs) args)
    TyConI (DataD {}) -> nonSynonym
    TyConI (NewtypeD {}) -> nonSynonym
    PrimTyConI {} -> nonSynonym
    _ -> error ("expandType': Can not expand type " ++ show t)
expandType' (TupleT n) args = return $ foldl AppT (TupleT n) args
expandType' (UnboxedTupleT n) args = return $ foldl AppT (UnboxedTupleT n) args
expandType' (ArrowT) args = return $ foldl AppT (ArrowT) args
expandType' (ListT) args = return $ foldl AppT (ListT) args
expandType' (AppT t1 t2) args = do t2' <- expandType' t2 []
                                   expandType' t1 (t2' : args)
expandType' (SigT t _kind) args = expandType' t args

----------------------------------------
-- Common error messages
----------------------------------------

-- We don't yet implement tests for polymorphic and quantified types
-- for two reasons:
--  * First, testing the equivalence of Cxt on a "forall" requires a
--    class solver which is beyond the scope of this project
--  * Second, even with empty Cxt, it is hard to define exactly what
--    as equal "forall"s.  For example, are "forall a b. (a, b)" and
--    "forall b a. (a, b)" equal?  What about "forall a. a -> a" and
--    "forall a b. b -> b" or "forall a. Int" and "Int"?  Or "forall
--    a. ([a], a -> a)" and "forall c d. ([c], d -> d)"?
-- SYB also does not have strong support for polymorphic and
-- quantified types, so this problem is not unique to this system.
{-
failForall :: (Monad m) => String -> Type -> m a
failForall functionName t =
  fail $ functionName ++
         ": Comparing polymorphic or quantified types is not (yet) implemented." ++
         " Found `" ++ pprint t ++ "'."
-}

----------------------------------------
-- Function and monadic composition
----------------------------------------

-- | A helper function: @composeE f g = [| $(f) . $(g) |]@.
-- Though this works for any 'Quasi' monad and not just 'Q'.
composeE :: (Quasi m) => m Exp -> m Exp -> m Exp
composeE f1 f2 = do
  f1' <- f1
  f2' <- f2
  x <- qNewName "x"
  return (LamE [VarP x] (AppE f1' (AppE f2' (VarE x))))
  --[|\x -> f1 (f2 x)|]

-- | A helper function: |@composeM f g = [| $(f) <=< $(g) |]@.
-- Though this works for any 'Quasi' monad and not just 'Q'.
composeM :: (Quasi m) => m Exp -> m Exp -> m Exp
composeM f1 f2 = do
  f1' <- f1
  f2' <- f2
  x <- qNewName "x"
  return (LamE [VarP x] (InfixE (Just (AppE f2' (VarE x))) (VarE '(>>=)) (Just f1')))
  --[|\x -> f2 x >>= f1|]


--------------------------------------------------------------------------------
-- TODO
--------------------------------------------------------------------------------

{-

- Support for GADTs
- (NOTE) the M of gmapMp becomes an object level monad, but the "p" is meta-level

- version of "somewhere" that doesn't recur where useless

- version of "somewhere" where (MonadPlus m) => m (Exp, Bool) (True means recur)

- note that recurive calls to everything may not terminate
       for that use "memoizeExp" directly

-- TODO: memoizeDecIO :: (a -> b) -> IO (a -> IO b)
--       can't be done because of qNewName

-- TODO: memoize via thunks

-- TODO: explain implementation of memoize instead of just interface

-- memoizeDec3, memoizeDec4, etc.
-- memoizeExp3, memoizeExp4, etc.
-- TODO: generate 2, 3, 4, 5, 6, 7, 8, and 9 via Template Haskell

-- TODO: remove "T/M/Q" suffix and replace the "'" suffix with a letter
-- TODO: suffixes: by name vs by type predicate; passing type vs not (only for type pred version)

-- extByName
-- extC
-- extWithType

-}
