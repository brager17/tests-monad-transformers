{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Main where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import GHC.Generics
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
data DivByError = ErrorDiv String
  deriving(Show)
main = putStr ""

type Position = Int
type Dice = (Int,Int)

data ChipState = 
    Open Position 
  | Close Position 
  | KnockedOut

data Player = White | Black
type Winner = Player

data Action = 
    Dice1
  | Move GameState

data WaitUserActionT = 
    BlackPlayerAction Action
  | WhitePlayerAction Action

data GameState = 
    Start 
  | End Winner
  | WaitUserAction WaitUserActionT

instance Show GameState where
  show Start = "start"
  show (End winner) = "end"
  show (WaitUserAction _) = "WaitUserAction"

data BoardState = BoardState 
                          {  blackPlayer :: [ChipState]
                           , whitePlayer :: [ChipState]
                           , dice :: Dice
                           }


startGame :: Reader GameState (Either String GameState)
startGame = reader (\x -> Right $ WaitUserAction $ WhitePlayerAction Dice1)


move :: Player -> Position -> ExceptT String (Reader GameState) GameState
move pl po = do
    st <- lift ask
    guard (case (pl,st) of (White, (WaitUserAction (BlackPlayerAction _))) -> False
                           (Black, (WaitUserAction (WhitePlayerAction _))) -> False
                           _ -> True)
    return Start

testPlayerAction = WaitUserAction $ WhitePlayerAction Dice1

testMove = runIdentity $ runReaderT (runExceptT $ move Black 1) Start

