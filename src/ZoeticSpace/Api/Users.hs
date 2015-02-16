{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Api.Users (userRoutes) where

import Prelude hiding (id)

import GHC.Generics (Generic)
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.ByteString as S

import Web.Scotty (get, post, json, text, ScottyM, jsonData, ActionM)
import Data.Aeson (FromJSON, ToJSON, Value(Object), (.:), (.:?), parseJSON)
import Database.Neo4j
import Data.String.Conversions

import ZoeticSpace.Persistence

type ValidationError = String

data User = User { id :: Maybe T.Text, name :: T.Text, email :: T.Text }
            deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .:? "id"
         <*> v .: "name"
         <*> v .: "email"

  
  parseJSON _ = mzero
  
instance ToJSON User

instance ToNeo4j User where
  entityProperties user = M.fromList [
                                       "name" |: (name user)
                                     , "email" |: (email user)
                                     ]
  entityLabel _ = "User"
  
instance FromNeo4j User where
  fromNode node = let properties = getNodeProperties node
                  in User {
                            id = Just $ (cs . nodeId) node
                          , name = (getTextProperty "name" properties)
                          , email = (getTextProperty "email" properties)
                          }

errorsFor :: User -> [ValidationError]
errorsFor user = []

createOrError successFunc entity = do
  case errorsFor entity of
    [] -> successFunc entity
    errors -> json errors

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
    createOrError persistUser user
    
  where persistUser user =  do
          node <- liftIO $ create user
          json $ (fromNode node :: User)

    
