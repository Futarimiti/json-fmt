{-# LANGUAGE PatternSynonyms #-}

module Logged where

import           Control.Applicative        (liftA2)
import           Control.Monad.Trans.Writer (Writer, WriterT (..), tell)
import           Data.Functor.Identity      (Identity (..))

type Log = String

type Logged a = Writer [String] a

pattern Logged :: w -> a -> Writer w a
pattern Logged w a = WriterT (Identity (a, w))

log :: Log -> Logged a -> Logged a
log = (>>) . tell . pure

logs :: [Log] -> Logged a -> Logged a
logs msgs (Logged logs a) = Logged (logs ++ msgs) a
