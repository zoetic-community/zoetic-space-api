{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Users (userRoutes) where

import Web.Scotty (get, post, json, ScottyM)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data User = User { name :: String, email :: String }
            deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

userRoutes ::  ScottyM ()
userRoutes = do
  get "/users" $ json allUsers
  post "/users" $ json allUsers
    
allUsers :: [User]
allUsers = [User {email="j@j.com", name="john"}, User {email="b@b.com", name="sam"}]

