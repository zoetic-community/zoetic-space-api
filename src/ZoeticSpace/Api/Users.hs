{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Api.Users (userRoutes) where

import GHC.Generics (Generic)
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Web.Scotty (get, post, json, text, ScottyM, jsonData, ActionM)
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
                                       "name" |: (name user)
                                     , "email" |: (email user)
                                     ]
  entityLabel _ = "User"
  
instance FromNeo4j User where
  fromProperties properties = User {
                                     name = (getTextProperty "name" properties)
                                   , email = (getTextProperty "email" properties)
                                   }

getUsers :: IO [User]
getUsers = do
  users <- allByLabel "User" :: IO [User]
  return users

userRoutes ::  ScottyM ()
userRoutes = do
  get "/v1/users" $ do
    users <- liftIO getUsers
    json users
    
  post "/v1/users" $ do
    user <- jsonData :: ActionM User
    node <- liftIO $ create user
    text $ (cs . nodeId) node
    
