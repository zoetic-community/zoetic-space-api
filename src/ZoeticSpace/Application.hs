{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Application where

import Web.Scotty
import Network.Wai (Application)

import ZoeticSpace.Api.Users (userRoutes)

app :: IO Application
app = scottyApp $ do
  get "/" $ do
    text "Nothing to see here"

  userRoutes
