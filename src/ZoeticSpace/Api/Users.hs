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

import Data.Lens.Light (setL)


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

getUsers :: IO [User]
getUsers = do
  nodes <- allByLabel "User"
  return $ fmap convert nodes
  where convert node = User {name = (toText $ lookup "name" node), email = (toText $ lookup "email" node)}
        lookup :: T.Text -> Node -> Maybe PropertyValue
        lookup key node = M.lookup key $ getNodeProperties node
        
        toText (Just (ValueProperty (TextVal val))) =  val
        toText Nothing = "not found"



userRoutes ::  ScottyM ()
userRoutes = do
  get "/users" $ do
    users <- liftIO getUsers
    json users
  post "/users" $ do
    user <- jsonData :: ActionM User
    node <- liftIO $ create user
    text $ (cs . nodeId) node
    
