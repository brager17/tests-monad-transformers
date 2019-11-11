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
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
data DivByError = ErrorDiv String
  deriving(Show)
main = putStr ""

myLift :: (Monad m) => m a ->  m (Maybe a)
myLift m = liftM Just m

run = runStateT test ""

myState :: StateT String Identity Int
myState = return 1

st :: State String Int
st = state (\s -> (1,s))

test :: StateT String Identity (Maybe Int)
test = do 
  x <- myLift myState
  return x

data User = User {
  id :: Int,
  name :: String
} deriving (Generic,Show)

instance FromJSON User

myRead :: IO B.ByteString
myRead = B.readFile "users.json" 

getU :: MaybeT IO [User] =
    do
    x <- lift myRead
    users <-  MaybeT $ return $ decode x
    return users

tes :: MaybeT IO [User]
tes = do
  x <- lift myRead
  users <- getU
  return users

decodeTest :: Maybe User
decodeTest = decode "{\"name\":\"Isaac Newton\",\"id\":1}" 