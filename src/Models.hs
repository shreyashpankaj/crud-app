{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- module Models where
module Models where

-- import Data.Time (Day (ModifiedJulianDay))
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    password Text
    UniqueUser name
    deriving Show Eq
    
UserSession json
    user UserId
    token Text
    UniqueToken token
    deriving Show Eq

Movie json
    name Text
    rating Int
    genre Text
    userId UserId
    deriving Show
|]