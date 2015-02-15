{-# LANGUAGE OverloadedStrings  #-}

module ZoeticSpace.Persistence where

import Database.Neo4j

port :: Port
port = 7474

host :: Hostname
host = "localhost"

class ToNeo4j a where
  entityProperties :: a -> Properties
  entityLabel :: a -> Label

create :: (ToNeo4j e) => e -> IO Node
create entity = withConnection host port $ do
  n <- createNode $ entityProperties entity
  addLabels [(entityLabel entity)] n
  return n