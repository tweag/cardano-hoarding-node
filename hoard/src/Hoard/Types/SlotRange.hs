module Hoard.Types.SlotRange (SlotRange (..)) where


data SlotRange = SlotRange
    { from :: Maybe Int64
    , to :: Maybe Int64
    }
