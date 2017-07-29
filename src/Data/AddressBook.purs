module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)
import Data.String (joinWith)

type Address = 
  { street :: String 
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

emptyBook :: AddressBook
emptyBook = empty

showAddress :: Address -> String
showAddress addr = joinWith ", " [addr.street, addr.city, addr.state]

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = (filter filterEntry >>> head) book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
