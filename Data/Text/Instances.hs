{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Data.Text.Instances where

import Control.Monad.Error (Error(..))
import Data.Text (Text)

import qualified Data.Text as T

instance Error Text where
    strMsg = T.pack
