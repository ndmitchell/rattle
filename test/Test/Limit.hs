{-# LANGUAGE ScopedTypeVariables #-}

module Test.Limit(main) where

import Test.Type
import Development.Rattle.Limit
import System.Time.Extra
import Control.Concurrent.Async
import Control.Monad.Extra
import Data.IORef


main = do
    putStrLn "Check that parallelism really runs in parallel"
    m <- meetup 100
    assertWithin 1 $ void $ mapConcurrently id $ replicate 100 m

    putStrLn "Check that limits can reach the limit"
    m <- meetup 100
    limit <- newLimit 100
    assertWithin 1 $ void $ mapConcurrently id $ replicate 100 $ withLimit limit m

    putStrLn "Check speculative limits can reach the limit"
    m <- meetup 100
    limit <- newLimit 100
    assertWithin 1 $ void $ mapConcurrently id $ replicate 100 $ withLimitMaybe limit m

    putStrLn "Check you can't exceed the limit"
    m <- meetup 65
    limit <- newLimit 65
    ref <- newIORef (0, 0)
    let f op = atomicModifyIORef ref $ \(mx,v) -> let v2 = op v in ((max v2 mx, v2), ())
    assertWithin 1 $ void $ mapConcurrently id $ replicate 100 $ withLimit limit $ do f succ; m; sleep 0.1; f pred
    (mx, _) <- readIORef ref
    mx === 65

    putStrLn "Check you can't exceed the limit speculatively"
    m <- meetup 65
    limit <- newLimit 65
    ref <- newIORef 0
    let f = atomicModifyIORef ref $ \v -> (v+1, ())
    assertWithin 1 $ void $ mapConcurrently id $ replicate 100 $ withLimitMaybe limit $ do f; m; sleep 0.1
    mx <- readIORef ref
    mx === 65
