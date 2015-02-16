{-# LANGUAGE OverloadedStrings  #-}

module ZoeticSpace.Persistence where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Database.Neo4j
import Database.Neo4j.Transactional.Cypher

port :: Port
port = 7474

host :: Hostname
host = "localhost"

class ToNeo4j a where
  entityProperties :: a -> Properties
  entityLabel :: a -> Label
  
class FromNeo4j a where
  fromNode :: Node -> a

allByLabel :: (FromNeo4j a) => Label -> IO [a]
allByLabel label = withConnection host port $ do
  nodes <- getNodesByLabelAndProperty label Nothing
  return $ fmap fromNode nodes

getTextProperty :: T.Text -> Properties -> T.Text
getTextProperty property properties = valToText $ M.lookup property properties

valToText :: Maybe PropertyValue -> T.Text
valToText (Just (ValueProperty (TextVal val))) =  val
valToText Nothing = error "PROPERTY NOT FOUND"

create :: (ToNeo4j e) => e -> IO Node
create entity = withConnection host port $ do
  n <- createNode $ entityProperties entity
  addLabels [(entityLabel entity)] n
  return n