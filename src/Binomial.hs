module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint a ()
binomial literals k =
    let literal's = lifts literals
    in  combinations (map not literal's) $ k + 1
