module Hoard.Effects (type (::>)) where

import Effectful ((:>))


-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a :> b
