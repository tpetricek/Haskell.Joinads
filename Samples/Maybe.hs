{-# LINE 1 "Maybe.jhs" #-}
module Main (main) where
{-# LINE 1 "Maybe.jhs" #-}
import Control.Monad.Joinads
{-# LINE 2 "Maybe.jhs" #-}
import Control.Monad (Monad, liftM)
 
{-# LINE 9 "Maybe.jhs" #-}
instance MonadZero Maybe where
        {-# LINE 10 "Maybe.jhs" #-}
        mzero = Nothing
 
{-# LINE 14 "Maybe.jhs" #-}
instance MonadZip Maybe where
        {-# LINE 15 "Maybe.jhs" #-}
        mzip (Just a) (Just b) = Just $ (a, b)
        {-# LINE 16 "Maybe.jhs" #-}
        mzip _ _ = Nothing
 
{-# LINE 20 "Maybe.jhs" #-}
instance MonadOr Maybe where
        {-# LINE 21 "Maybe.jhs" #-}
        morelse (Just a) _ = Just a
        {-# LINE 22 "Maybe.jhs" #-}
        morelse _ b = b
 
{-# LINE 27 "Maybe.jhs" #-}
instance MonadPlus Maybe where
        {-# LINE 28 "Maybe.jhs" #-}
        mplus (Just a) (Nothing) = Just a
        {-# LINE 29 "Maybe.jhs" #-}
        mplus (Nothing) (Just b) = Just b
        {-# LINE 30 "Maybe.jhs" #-}
        mplus _ _ = Nothing
 
{-# LINE 33 "Maybe.jhs" #-}
instance MonadAlias Maybe where
        {-# LINE 34 "Maybe.jhs" #-}
        malias = return
 
{-# LINE 45 "Maybe.jhs" #-}
kleeneOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
{-# LINE 46 "Maybe.jhs" #-}
kleeneOr a b
  = (malias (a)) >>=
      (\ avar1 ->
         (malias (b)) >>=
           (\ avar2 ->
              (((avar1) >>=
                  \ jvar1 ->
                    case (jvar1) of
                        True -> return (return True)
                        _ -> mzero)
                 `morelse`
                 ((avar2) >>=
                    \ jvar2 ->
                      case (jvar2) of
                          True -> return (return True)
                          _ -> mzero)
                 `morelse`
                 ((avar1) `mzip` (avar2) >>=
                    \ jvar3 ->
                      case (jvar3) of
                          (a, b) -> return (return $ a || b)
                          _ -> mzero))
                >>= id))
 
{-# LINE 55 "Maybe.jhs" #-}
kleeneOrT :: Maybe Bool -> Maybe Bool -> Maybe Bool
{-# LINE 56 "Maybe.jhs" #-}
kleeneOrT a b
  = malias a >>=
      \ a ->
        malias b >>=
          \ b ->
            ((a >>=
                \ a' ->
                  case a' of
                      True -> return (return True)
                      _ -> mzero)
               `morelse`
               (b >>=
                  \ b' ->
                    case b' of
                        True -> return (return True)
                        _ -> mzero)
               `morelse`
               ((a `mzip` b) >>= (\ (a, b) -> return (return $ a || b))))
              >>= id
{-# LINE 72 "Maybe.jhs" #-}
showThree (Nothing) = "unknown"
{-# LINE 73 "Maybe.jhs" #-}
showThree (Just True) = "true   "
{-# LINE 74 "Maybe.jhs" #-}
showThree (Just False) = "false  "
 
{-# LINE 76 "Maybe.jhs" #-}
main :: IO ()
{-# LINE 77 "Maybe.jhs" #-}
main
  = let {-# LINE 78 "Maybe.jhs" #-}
        opts = [Just True, Just False, Nothing]
      in
      let {-# LINE 79 "Maybe.jhs" #-}
          prod = [(a, b) | a <- opts, b <- opts]
        in
        do putStrLn $ "Kleene 'or' operation for three-valued logic:\n"
           sequence_
             (map
                (\ (a, b) ->
                   let {-# LINE 83 "Maybe.jhs" #-}
                       res = kleeneOr a b
                     in
                     putStrLn $ "   " ++ (showThree a) ++ " | " ++ (showThree b) ++
                       " = "
                       ++ (showThree $ res))
                prod)
