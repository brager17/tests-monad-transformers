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


data Chip = Chip{
  id :: Int,
  state :: ChipState
}
data ChipState = 
    Open Position 
  | Close Position 
  | KnockedOut

data Player = White | Black
type Winner = Player

data Action = 
    Dice1
  | Move BoardState

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
                          {  blackPlayer :: [Chip]
                           , whitePlayer :: [Chip]
                           , dice :: Dice
                           }


startGame :: Reader GameState (Either String GameState)
startGame = reader (\x -> Right $ WaitUserAction $ WhitePlayerAction Dice1)


playerGuard :: Player ->  ExceptT String (Reader GameState) GameState
playerGuard pl = do
    st <- lift ask
    case (pl,st) of (White, (WaitUserAction (BlackPlayerAction _))) -> throwError  "now is not your turn"
                    (Black, (WaitUserAction (WhitePlayerAction _))) -> throwError "now is not your turn"
                    _ -> return st

gameIsActiveGuard :: ExceptT String (Reader GameState) GameState
gameIsActiveGuard = do
  state <- lift ask
  case state of Start -> throwError "need start game"
                End _ ->  throwError "need end game"
                _ -> return state

waitMoveGuard :: ExceptT String (Reader GameState) BoardState
waitMoveGuard = do
  state <- lift ask
  case state of WaitUserAction (BlackPlayerAction(Move board)) -> return board
                WaitUserAction (WhitePlayerAction(Move board)) -> return board
                _ -> throwError "is not move"


move :: Player -> Int -> Position -> ExceptT String (Reader GameState) GameState
move pl chipId newPosition = 
  do
    playerGuard pl
    gameIsActiveGuard
    board <- waitMoveGuard
    case pl of Black -> 
                      let 
                        id :: Chip -> Int
                        id  x = id x
                        chip = head $ filter (\x -> (id x) == chipId) $ blackPlayer board
                        otherChips = filter (\x -> (id x) /= chipId) $ blackPlayer board
                      in return (WaitUserAction (WhitePlayerAction $ Move (board {blackPlayer = chip:otherChips})))
               _ -> return Start

testPlayerAction = WaitUserAction $ WhitePlayerAction Dice1

-- testMove = runIdentity $ runReaderT (runExceptT $ move Black 1) Start

run m = runIdentity $ runReaderT (runExceptT m) (WaitUserAction (BlackPlayerAction Dice1))

run1 = runExceptT 

p :: Position
p = 1