{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main (main, spec) where

import           Test.Hspec hiding (shouldContain)
import           Network.Wai.Test (SResponse)
import           Data.ByteString (ByteString)
import           Control.Applicative

import Helper

import ZoeticSpace.Application

main :: IO ()
main = hspec spec

get :: ByteString -> IO SResponse
get path = app >>= getPath path

statusOk :: ByteString -> Expectation
statusOk path = (statusCode <$> get path) `shouldReturn` 200

spec :: Spec
spec = do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      statusOk "/"

    it "says 'Hello!'" $ do
      (body <$> get "/") `shouldReturn` "Nothing to see here"
  
  describe "GET /v1/useres" $ do
    it "responds with a 200" $ do
      statusOk "/v1/users"

  context "when given an invalid request path" $ do
    it "responds with HTTP status 404" $ do
      (statusCode <$> get "/anything") `shouldReturn` 404

