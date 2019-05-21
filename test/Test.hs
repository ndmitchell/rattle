{-# LANGUAGE ScopedTypeVariables #-}

module Test(main) where

import qualified Test.Limit
import qualified Test.Simple

main = do
    Test.Limit.main
    Test.Simple.main
