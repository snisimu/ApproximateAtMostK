module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint a () ()
binomial x's k =
    let xs = lifts x's
    in  combinations (map not xs) $ k + 1
