{-# LANGUAGE OverloadedStrings #-}
module Server(listen) where

import Snap.Core (ifTop,writeText)
import Snap.Http.Server (quickHttpServe)

listen :: IO()
listen = quickHttpServe $ ifTop (writeText "Backgammon")