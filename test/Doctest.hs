{-# OPTIONS -Wall #-}

import Test.DocTest

main :: IO ( )
main = doctest [ "src/DSS/Parser.hs" ]
