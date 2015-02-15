{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Api.Users (userRoutes) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Web.Scotty (get, post, json, text, ScottyM)
import Data.Aeson (FromJSON, ToJSON)
import Database.Neo4j
import Data.String.Conversions


import ZoeticSpace.Persistence

data User = User { name :: T.Text, email :: T.Text }
            deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

instance ToNeo4j User where
  entityProperties user = M.fromList [
                                       "name" |: (name user :: T.Text)
                                     , "email" |: (email user :: T.Text)
                                     ]
  entityLabel _ = "User"

userRoutes ::  ScottyM ()
userRoutes = do
  get "/users" $ text "pending"
  post "/users" $ do
    let user = User {name="John", email="bluey@blue.com"}
    node <- liftIO $ create user
    text $ (cs . nodeId) node
    
