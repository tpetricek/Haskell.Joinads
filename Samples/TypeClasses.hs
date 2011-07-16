-- | This module defines the 'Joinad' type class that is used by the 'docase' notation
-- as described in /Extending Monads with Pattern Matching/, which can be found at:
-- <http://www.cl.cam.ac.uk/~tp322/papers/docase.html>.
-- The 'Joinad' type class is a combination of operations provided by three distinct
-- type classes (that already appear in some form in various Haskell libraries). To keep
-- the samples self-contained, this module re-defines additional type classes that are 
-- used in the definition. The 'Joinad' type class then combines the three other type
-- classes and relates their operations using equations.
--
-- > class (MonadAlias m, MonadZip m, MonadOr m) 
-- >    => Joinad m
-- 
-- The individual type classes that are required by a joinad are:
--
-- * 'MonadAlias' extends the 'Monad' type class with support for aliasing operation
--   that can be used for controlling the evaluation of monads. For example, a 
--   parallelism monad may use this operation to get a speculative evaluation semantics.
--
-- * 'MonadZip' extends 'Monad' type class with an additional generalized zipping 
--   operation. The operation is used to combine two values of types @m a@ and 
--   @m b@ into @m (a, b)@. The operation usually behaves differently than the 
--   operation of this type that can be defined in terms of @>>=@ and @return@. 
--   In addition, we require the operation to be symmetric.
--
-- * 'MonadZero' and 'MonadOr' add two operations that add monoidal structure to a
--   monad. Together, they replace the standard 'MonadPlus' type class and specify more
--   clearly the required laws. In particular, the operation of 'MonadOr' is
--   /left-biased/.
--
-- The type classes are inspired by (or derived from) several existing Haskell libraries:
--
-- * The types 'MonadZero', 'MonadPlus' and 'MonadOr' mostly follow the proposal
--   from <http://www.haskell.org/haskellwiki/MonadPlus_reform_proposal>, with the
--   only difference that left-bias of 'MonadOr' is expressed differently.
--
-- * The type 'MonadZip' is based on the patch that re-implements monad comprehensions
--   (<http://hackage.haskell.org/trac/ghc/ticket/4370>), but is also required to be
--   associative and symmetric (together with a few other, less important, equations).
--
-- * The type 'MonadAlias' adds an operation that can be viewed as a 'cojoin' of a 
--   comonad. It is similar to the 'Extend' type class from the comonad package
--   <http://hackage.haskell.org/packages/archive/comonad/1.1.0/doc/html/Data-Functor-Extend.html>,
--   although one of the laws is not included (as it is not required by 'codo') and 
--   several additional laws relate the operation with other operations of joinads.
--
module Control.Monad.Joinads
  (MonadZero(..), MonadPlus(..), MonadOr(..), MonadZip(..), MonadAlias(..), Joinad(..)) where

import Control.Monad (Monad, liftM)

-----------------------------------------------------------------------------------------
-- MonadZero
-----------------------------------------------------------------------------------------

-- | Monad that also supports an operation representing a /failed/ computation.
--   The failed computation is a value of type 'm a' and it behaves as a zero
--   with respect to the monadic /bind/ operation (as specified by the left and
--   right zero equations).
class Monad m => MonadZero m where
   -- | The operation representing a failed computation (zero).
   -- | It should also satisfy two equations:
   --
   -- >       mzero >>= f  =  mzero
   -- > v >>= \x -> mzero  =  mzero
   --
   mzero :: m a


-----------------------------------------------------------------------------------------
-- MonadOr, MonadPlus
-- * two alternative ways of defining a monoid (with or without left-bias)
-----------------------------------------------------------------------------------------

-- | Extends the 'MonadZero' type class by adding an associative binary operation 
--   working on @m a@ values that forms a monoid with 'mzero'. The type class also 
--   specifies that the operation shouldn't be left-biased (which is called left 
--   distributivity law). This replaces original 'MonadPlus' for non-left-biased instances 
--   (such as @[]@)
class MonadZero m => MonadPlus m where
   -- | The associative operation that forms a monoid (together with 'mzero').
   --   It should satisfy the laws of a /monoid/:
   --
   -- >         mzero `mplus` a  =  mzero
   -- >         a `mplus` mzero  =  mzero
   -- > (a `mplus` b) `mplus` c  =  a `mplus` (b `mplus` c)
   --
   -- In addition, the operation should satisfy the /left distributivity/ 
   --   equation that distinguishes it from the MonadOr type class:
   --
   -- >       a `mplus` b >>= k  =  (a >>= k) `mplus` (b >>= k)
   --
   mplus :: m a -> m a -> m a


-- | Extends the 'MonadZero' type class by adding an associative binary operation 
--   working on @m a@ values that forms a monoid with 'mzero'. The type class also 
--   specifies that the operation should be left-biased (which is called left 
--   catch law). This replaces original 'MonadPlus' for left-biased instances 
--   (such as @Maybe@ and @IO@)
class MonadZero m => MonadOr m where
   -- | The associative operation that forms a monoid (together with 'mzero').
   --   It should satisfy the laws of a /monoid/:
   --
   -- >           mzero `morelse` a  =  mzero
   -- >           a `morelse` mzero  =  mzero
   -- > (a `morelse` b) `morelse` c  =  a `morelse` (b `morelse` c)
   --
   -- In addition, the operation should satisfy the /left bias/ equation
   --   that distinguishes it from the 'MonadPlus' type class:
   --
   -- >     a `morelse` (map f a)  =  a
   --
   morelse :: m a -> m a -> m a


-----------------------------------------------------------------------------------------
-- MonadZip
-----------------------------------------------------------------------------------------

-- | 'MonadZip' extends the 'Monad' type class with an additional generalized zipping 
--   operation. The operation is used to combine two values of types @m a@ and 
--   @m b@ into @m (a, b)@. The operation usually behaves differently than the 
--   monoidal operation that can be defined in terms of @>>=@ and @return@. In addition,
--   we require the operation to be symmetric.
class Monad m => MonadZip m where
  -- | Generalized zipping operation that is required to be symmetric and associative:
  --
  -- > (a `mzip` b) `mzip` c  =  map assoc (a `mzip` (b `mzip` c))
  -- >            a `mzip` b  =  map swap  (b `mzip` a)
  --
  -- In addition, the operation should behave nicely with respect to the
  --   remaining operations of a 'Monad':
  --
  -- >  return a `mzip` return b  =  return (a, b)
  -- >                a `mzip` a  =  map (\v -> (v,v)) a
  mzip :: m a -> m b -> m (a, b)


-----------------------------------------------------------------------------------------
-- MonadAlias
-----------------------------------------------------------------------------------------

-- | 'MonadAlias' extends the 'Monad' type class with support for aliasing operation
--   that can be used for controlling the evaluation of monads. For example, a 
--   parallelism monad may use this operation to get a speculative evaluation semantics.
class Monad m => MonadAlias m where
  -- | An operation that represents an aliasing of computations. When given a computation
  --   of type @m a@ it produces a new computation @m a@ (inside the monad @m@) that
  --   represents access to the value of the computation. This may use a mutable 
  --   reference cell to share the result once calculated (call-by-need) or implement
  --   speculative behavior, etc. The operation should satisfy several equations:
  --
  -- >        malias a >>= id  =  a
  -- >      malias (return a)  =  return (return a)
  -- > map (map f) (malias a)  =  malias (map f a)
  -- >  malias m <*> malias n  =  map swap (malias m <*> malias n)
  -- 
  -- Where: 
  --
  -- > ma <*> mb = ma >>= \a -> mb >>= \b -> (a, b)
  --
  -- Notably, some of the laws overlap with the laws of /computational comonads/, but this
  -- topic is outside the scope of this documentation.
  malias :: m a -> m (m a)


-----------------------------------------------------------------------------------------
-- Joinad
-----------------------------------------------------------------------------------------

-- | 'Joinad' type class simply combines 'MonadZip', 'MonadOr' and 'MonadAlias' and 
--   relates their operations using additional laws. The type class is required by the
--   @docase@ notation. For example:
-- 
-- > docase (ma, mb) of
-- > (0, ?) -> return 0
-- > (a, b) -> return (a * b)
--
-- Is translated as follows:
--
-- > malias ma >>= \ma ->
-- > malias mb >>= \mb ->
-- >   ( (ma >>= \arg -> case arg of
-- >        0 -> return (return 0) 
-- >        otherwise -> mzero) `morelse`
-- >     (ma `mzip` mb >>= \arg -> case arg of
-- >        (a, b) -> return (return (a * b))) ) >>= id
--
-- The additional laws that are required to hold about the joinad operations are:
--
-- > (malias m >>= f) `mzip` n  =  malias m >>= ((`mzip` n) . f)
-- >              malias mzero  =  return mzero
-- >            a `mzip` mzero  =  mzero
--
-- The following distributivity law relates the 'mzip' and 'morelse' operation:
--
-- > (a `mzip` b) `morelse` (a `mzip` c)  =  malias a >>= \a -> (a `mzip` (b `morelse` c))
class (MonadAlias m, MonadZip m, MonadOr m) 
  => Joinad m