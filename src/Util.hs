module Util (safeHead) where

-- |Safe head function
safeHead [] = Nothing
safeHead (x:_) = Just x


