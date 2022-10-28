module Pairwise(atMost) where

import Base

atMost k n = combinations (map X [1..n]) (n - k + 1)
