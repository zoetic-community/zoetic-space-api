{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Api.Users (userRoutes) where

import Prelude hiding (id)

import GHC.Generics (Generic)
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as SM
import qualified Data.Text as T
import qualified Data.ByteString as S

import Web.Scotty (get, post, json, text, ScottyM, jsonData, ActionM, status)
import Data.Aeson (FromJSON, ToJSON, Value(Object), (.:), (.:?), parseJSON, toJSON, object, (.=), encode)
import Network.HTTP.Types (status400)
import Database.Neo4j
import Data.String.Conversions

import ZoeticSpace.Persistence
import ZoeticSpace.Validation

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

instance Validatable User where
  errorsFor user = case collectErrors user of
                     [(_, [])] -> Nothing
                     errors -> Just (ValidationErrors $ SM.fromList errors)
                     
                 where
                   collectErrors user = [("name", nameErrors user)]
                   nameErrors user = runValidations (name user) [blankValidation]

                   blankValidation "" = Just "must not be blank"
                   blankValidation _ = Nothing
                 

createOrValidationFail :: (Validatable e) => (e -> ActionM ()) -> e -> ActionM ()
createOrValidationFail successFunc entity = do
  case errorsFor entity of
    Nothing -> successFunc entity
    Just errors -> status status400 >> json errors

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
    createOrValidationFail persistUser user
    
  where persistUser user =  do
          node <- liftIO $ create user
          json $ (fromNode node :: User)

    
