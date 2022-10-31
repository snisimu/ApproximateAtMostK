module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

type Vbinomial = ()

binomial :: NumberConstraint Vbinomial
binomial literals k = combinations (map not literals) $ k + 1
