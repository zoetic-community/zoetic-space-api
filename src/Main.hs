module Main where

import Network.Wai.Handler.Warp
import ZoeticSpace.Application

main :: IO ()
main = run 3000 =<< app
