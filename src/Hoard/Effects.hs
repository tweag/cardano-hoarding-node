module Hoard.Effects (type (::>)) where


-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a :> b
