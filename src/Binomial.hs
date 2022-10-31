module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint a () 
binomial literals k = combinations (map not literals) $ k + 1
