module Binomial (atMost) where

import Base

type Vbinomial = ()

atMost :: AtMost Vbinomial
atMost literals k = combinations (map not' literals) $ k + 1
