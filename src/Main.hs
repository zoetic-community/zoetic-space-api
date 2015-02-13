{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Aeson (encode)
import Api.Users (userRoutes)

main :: IO ()
main = scotty 3000 $ do
  get "/" $ text "Nothing to see here"
  userRoutes
