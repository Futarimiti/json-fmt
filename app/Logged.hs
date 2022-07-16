module Logged where

import           Control.Applicative (liftA2)

type Log = String

data Logged a = Logged { getLogs :: [Log]
                       , getVal  :: a
                       } deriving (Show, Read)

instance Eq a => Eq (Logged a) where
  (Logged _ a) == (Logged _ b) = a == b

instance Functor Logged where
  fmap f (Logged logs a) = Logged logs $ f a

instance Applicative Logged where
  pure a = Logged [] a
  liftA2 f (Logged logs1 a) (Logged logs2 b) = Logged (logs1 ++ logs2) $ f a b

instance Monad Logged where
  (Logged logs a) >>= f = Logged (logs ++ newLogs) fa
    where (Logged newLogs fa) = f a

instance Semigroup a => Semigroup (Logged a) where
  (Logged logs1 a) <> (Logged logs2 b) = Logged (logs1 ++ logs2) (a <> b)

instance Monoid a => Monoid (Logged a) where
  mempty = pure mempty

log :: Log -> Logged a -> Logged a
log msg (Logged logs a) = Logged (logs ++ [msg]) a

logs :: [Log] -> Logged a -> Logged a
logs msgs (Logged logs a) = Logged (logs ++ msgs) a
