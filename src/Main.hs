{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Aeson (encode)
import Api.Users (allUsers)

main = scotty 3000 $ do
  get "/" $ do
    text "Nothing to see here"
    
  get "/users" $ do
    json allUsers