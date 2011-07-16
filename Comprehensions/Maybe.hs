{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}

import Control.Monad.Zip

-------------------------------------------------------------------------------
-- MonadZip instance for Maybe: Maybe is commutative so we can implement 
-- 'mzip' using bind and we still get symmetric implementation.
-------------------------------------------------------------------------------

instance MonadZip Maybe where
  mzip ma mb = ma >>= \a -> mb >>= \b -> (a, b)

-------------------------------------------------------------------------------
-- Now we can use zip comprehensions with Maybe
-------------------------------------------------------------------------------

addDo :: Maybe Integer -> Maybe Integer -> Maybe Integer
addDo a b = 
  do aa <- a
     bb <- b
     return (aa + bb)

addCompr :: Maybe Integer -> Maybe Integer -> Maybe Integer
addCompr a b = 
  [ aa + bb | aa <- a | bb <- b  ]

	 
main = do
  print (addDo (Just 1) (Just 2))
  print (addCompr (Just 1) (Just 2))
  