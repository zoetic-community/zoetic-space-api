{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai

import ZoeticSpace.Application

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

  describe "GET /v1/users" $ do
    it "sets CORS header" $ do
      get "/v1/users" `shouldRespondWith` 200 {matchHeaders = ["Access-Control-Allow-Origin" <:> "*"]}
