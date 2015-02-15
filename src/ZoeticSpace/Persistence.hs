{-# LANGUAGE OverloadedStrings  #-}

module ZoeticSpace.Persistence where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Database.Neo4j
import Database.Neo4j.Transactional.Cypher

port :: Port
port = 7474

host :: Hostname
host = "localhost"

class ToNeo4j a where
  entityProperties :: a -> Properties
  entityLabel :: a -> Label

allByLabel :: Label -> IO [Node]
allByLabel label = withConnection host port $ do
  getNodesByLabelAndProperty label Nothing

create :: (ToNeo4j e) => e -> IO Node
create entity = withConnection host port $ do
  n <- createNode $ entityProperties entity
  addLabels [(entityLabel entity)] n
  return n