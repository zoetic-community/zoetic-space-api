{-# LANGUAGE OverloadedStrings  #-}

module ZoeticSpace.Persistence where

import Database.Neo4j

port :: Port
port = 7474

host :: Hostname
host = "localhost"

create :: Label -> Properties -> IO Node
create label properties = withConnection host port $ do
  n <- createNode properties
  addLabels [label] n
  return n