{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, ViewPatterns, LambdaCase #-}

module Development.Rattle.UI(
    UI, withUI, addUI,
    ) where


import System.Time.Extra
import Control.Exception
import Data.List.Extra
import qualified System.Console.Terminal.Size as Terminal
import Numeric.Extra
import General.EscCodes
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Control.Concurrent.Async
import Control.Monad.Extra


data S = S
    {sTraces :: [Maybe (String, String, Seconds)] -- ^ the traced items, in the order we display them, and relative start time
    ,sUnwind :: Int -- ^ Number of lines we used last time around
    }

emptyS :: S
emptyS = S [] 0

addTrace :: String -> String -> Seconds -> S -> S
addTrace msg1 msg2 time s = s{sTraces = f (msg1,msg2,time) $ sTraces s}
    where
        f v (Nothing:xs) = Just v:xs
        f v (x:xs) = x : f v xs
        f v [] = [Just v]

delTrace :: String -> String -> Seconds -> S -> S
delTrace msg1 msg2 time s = s{sTraces = f (msg1,msg2,time) $ sTraces s}
    where
        f v (Just x:xs) | x == v = Nothing:xs
        f v (x:xs) = x : f v xs
        f v [] = []


display :: Int -> String -> Seconds -> S -> (S, String)
display width header time s = (s{sUnwind=length post}, escCursorUp (sUnwind s) ++ unlines (map pad post))
    where
        post = "" : (escForeground Green ++ "Status: " ++ header ++ escNormal) : map f (sTraces s)

        pad x = x ++ escClearLine
        f Nothing = " *"
        f (Just (s1,s2,t))
            | width - endN1 > 20 = " * " ++ take (width - endN1 - 4) s1 ++ end1
            | width - endN2 > 20 = " * " ++ take (width - endN2 - 4) s1 ++ end2
            | otherwise = take width $ " * " ++ s1
            where
                end1 = g (time - t) s2
                endN1 = length $ removeEscCodes end1

                end2 = g (time - t) ""
                endN2 = length $ removeEscCodes end2

        g i m | showDurationSecs i == "0s" = if null m then "" else "(" ++ m ++ ")"
              | i < 10 = " (" ++ s ++ ")"
              | otherwise = " (" ++ escForeground (if i > 20 then Red else Yellow) ++ s ++ escNormal ++ ")"
            where s = m ++ [' ' | m /= ""] ++ showDurationSecs i

newtype UI = UI (String -> String -> IO (IO ()))

addUI :: UI -> String -> String -> IO (IO ())
addUI (UI x) = x


showDurationSecs :: Seconds -> String
showDurationSecs = replace ".00s" "s" . showDuration . intToDouble . round


-- | Run a compact UI, with the ShakeOptions modifier, combined with
withUI :: IO String -> (UI -> IO a) -> IO a
withUI header act = do
    b <- checkEscCodes
    if b then withUICompact header act else withUISerial act

withUICompact :: IO String -> (UI -> IO a) -> IO a
withUICompact header act = do
    ref <- newIORef emptyS
    let tweak f = atomicModifyIORef ref $ \s -> (f s, ())
    time <- offsetTime
    let tick = do
            h <- header
            t <- time
            w <- maybe 80 Terminal.width <$> Terminal.size
            mask_ $ putStr =<< atomicModifyIORef ref (display w h t)
    withAsync (forever (tick >> sleep 0.4) `finally` tick)  $ \_ ->
        act $ UI $ \s1 s2 -> do
            t <- time
            tweak $ addTrace s1 s2 t
            return $ tweak $ delTrace s1 s2 t

withUISerial :: (UI -> IO a) -> IO a
withUISerial act =
    act $ UI $ \msg1 msg2 -> do
        BS.putStrLn $ BS.pack $ msg1 ++ if null msg2 then "" else " (" ++ msg2 ++ ")"
        return $ return ()
