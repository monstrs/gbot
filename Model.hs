{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

let mongoSettings = mkPersistSettings (ConT ''MongoContext)
    in share [mkPersist mongoSettings]
        $(persistFileWith upperCaseSettings "config/models")
