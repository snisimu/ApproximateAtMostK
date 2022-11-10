module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint
binomial _ xs k = if length xs - 1 == k
    then [map not xs]
    else combinations (map not xs) $ k + 1
