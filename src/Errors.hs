module Errors where

newtype Error = Error String

instance Show Error where
  show (Error e) = show e