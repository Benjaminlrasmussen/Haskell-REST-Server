{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Prelude hiding (id)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent (newMVar, readMVar, takeMVar, putMVar)
import Control.Monad.Trans.Class (lift)

main :: IO ()
main = do
  let lst = [(1, Member 1 "Kurt" "Kurt@Kurtmail.dk"), (2, Member 2 "Not-Kurt" "DefinetelyNotKurt@gmail.com")]
  membersRef <- newMVar $ IntMap.fromList lst

  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello ", name, " from Scotty!</h1><hr/>"]
    get "/member/count" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.size members
    get "/member/:id" $ do
      idText <- param "id"
      let iden = (read idText) :: Int
      members <- lift $ readMVar membersRef
      json $ IntMap.lookup iden members
    post "/member" $ do
      member <- jsonData
      members <- lift $ takeMVar membersRef
      let (updatedMember, afterMembers) = insertMember member members
      lift $ putMVar membersRef afterMembers
      json $ updatedMember

data Member = Member {
  id :: Int,
  name :: String,
  email :: String
} deriving (Show, Generic)

instance FromJSON Member
instance ToJSON Member

insertMember :: Member -> IntMap Member -> (Member, IntMap Member)
insertMember member intMap =
  if (IntMap.member (id member) intMap) then
    (member, IntMap.insert (id member) member intMap)
  else
    let
      m = Member (IntMap.size intMap + 1) (name member) (email member)
    in
      (m, IntMap.insert (id m) m intMap)
