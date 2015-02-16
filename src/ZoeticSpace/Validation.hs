{-# LANGUAGE OverloadedStrings #-}

module ZoeticSpace.Validation where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Aeson (object, ToJSON, (.=), toJSON)

data ValidationErrors = ValidationErrors (HashMap Text [Text])

instance ToJSON ValidationErrors where
  toJSON (ValidationErrors errors) = object [ "errors" .= (toJSON errors) ]

class Validatable e where
  errorsFor :: e -> Maybe ValidationErrors

type Value = Text
type Error = Text

runValidations :: Value -> [(Value -> Maybe Error)] -> [Error]
runValidations value validations = foldl (runValidation value) [] validations
  where
    runValidation value errors validation = case validation value of
                                              Just error -> error : errors
                                              Nothing -> errors
