{-# LANGUAGE OverloadedStrings  #-}

module ZoeticSpace.Persistence where

import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Database.Neo4j

port :: Port
port = 7474

host :: Hostname
host = "localhost"

-- | Dummy properties
someProperties :: Properties
someProperties = M.fromList [ "name" |: ("John" :: T.Text)
                            , "email" |: ("j@j.com" :: T.Text)
                            ]

create :: IO Node
create = withConnection host port $ do
   createNode someProperties