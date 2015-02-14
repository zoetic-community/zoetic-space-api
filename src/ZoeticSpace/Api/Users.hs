{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Api.Users (userRoutes) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Web.Scotty (get, post, json, text, ScottyM)
import Data.Aeson (FromJSON, ToJSON)
import Database.Neo4j
import Data.String.Conversions

import ZoeticSpace.Persistence

data User = User { name :: String, email :: String }
            deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

userRoutes ::  ScottyM ()
userRoutes = do
  get "/users" $ text "pending"
  post "/users" $ do
    node <- liftIO create
    text $ (cs . nodeId) node
    
