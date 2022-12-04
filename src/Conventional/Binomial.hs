module Conventional.Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint
binomial _ xs k =
    let notXs = map not xs
    in  if length xs - 1 == k
            then [notXs]
            else combinations notXs $ k + 1
