{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Users (allUsers) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data User = User { name :: String, email :: String }
            deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

allUsers :: [User]
allUsers = [User {email="j@j.com", name="john"}, User {email="b@b.com", name="sam"}]