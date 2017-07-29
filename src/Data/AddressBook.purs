module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
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

containsName :: String -> String -> AddressBook -> Boolean
containsName firstName lastName book = not $ null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: String -> AddressBook -> Maybe Entry
findEntryByAddress street book = (filter filterEntry >>> head) book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

findEntryByName :: String -> String -> AddressBook -> Maybe Entry
findEntryByName firstName lastName book = (filter filterEntry >>> head) book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy isDuplicate book
  where
    isDuplicate :: Entry -> Entry -> Boolean
    isDuplicate a b = a.firstName == b.firstName && a.lastName == b.lastName

showAddress :: Address -> String
showAddress addr = joinWith ", " [addr.street, addr.city, addr.state]

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address
