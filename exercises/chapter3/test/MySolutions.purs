module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter filterEntry >>> head
    where
      filterEntry :: Entry -> Boolean
      filterEntry = _.address.street >>> eq street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = filter filterEntry >>> null >>> not
    where
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq eqEntries
    where
        eqEntries :: Entry -> Entry -> Boolean
        eqEntries a b = a.firstName == b.firstName && a.lastName == b.lastName
