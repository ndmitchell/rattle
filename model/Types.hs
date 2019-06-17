{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, FlexibleInstances #-}

module Types(Cmd(..), State(..), Hazard(..), Recoverable(..), Action(..)
            , T(..), t0, add, ReadOrWrite(..), Tree(..), TreeOrHazard(..)
            , reduce) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

data ReadOrWrite = Read | Write deriving (Show,Eq) -- copied from Server.hs

data Cmd = Cmd { id :: String
               , pos :: !T
               , cost :: !T -- predetermined time it takes to "run" this cmd
               , start :: [T]
               , stop :: [T]
               , rfiles :: [String] -- files actually read
               , wfiles :: [String] -- files actually written
               , traces :: [([String],[String])]} -- files recorded read and written
           deriving(Eq) -- arbitrary

instance Show Cmd where
  show Cmd{..} = id

data State = State { toRun :: [Cmd]
                   , prevRun :: [Cmd] -- speculation list in rattle
                   , running :: [Cmd]
                   , done :: (Tree,Map.HashMap FilePath (ReadOrWrite, T, Cmd))
                   , timer :: !T }

data Hazard = ReadWriteHazard FilePath Cmd Cmd Recoverable
            | WriteWriteHazard FilePath Cmd Cmd deriving (Eq)

data Recoverable = Recoverable | NonRecoverable deriving (Show,Eq)

data Action = Done | Start | Finished | Wait

newtype T = T Int -- timestamps
  deriving (Enum,Eq,Ord,Show,Read)

t0 :: T
t0 = T 0

add :: T -> T -> T
add (T i) (T i2) = T (i + i2)

data Tree = Par {pc1 :: Tree
                ,pc2 :: Tree}
          | Seq {c1 :: Tree
                ,c2 :: Tree}
          | L {c :: Cmd}
          | E {}

instance Show Tree where
  show E{} = ""
  show L{..} = show c
  show Seq{..} = "(seq " ++ (show c1) ++ " " ++ (show c2) ++ ")"
  show Par{..} = "(par " ++ (show pc1) ++ " " ++ (show pc2) ++ ")"
  
instance Eq Tree where
  t1 == t2 = f (reduce t1) (reduce t2)
    where f E E = True
          f (L c) (L c2) = c == c2
          f (Seq c1 (Seq c2 c3)) (Seq (Seq c4 c5) c6) = c1 == c4 && c2 == c5 && c3 == c6
          f (Seq c1 c2) (Seq c3 c4) = c1 == c3 && c2 == c4
          f (Par (Par c1 c2) c3) (Par c4 (Par c5 c6)) = (c1 == c4 && c2 == c5 && c3 == c6)
                                                        || (c1 == c4 && c2 == c6 && c3 == c5)
                                                        || (c1 == c5 && c2 == c5 && c3 == c4)
                                                        || (c1 == c5 && c2 == c4 && c3 == c5)
                                                        || (c1 == c6 && c2 == c4 && c3 == c5)
                                                        || (c1 == c6 && c2 == c5 && c3 == c4)
          f (Par c1 c2) (Par c3 c4) = (c1 == c3 && c2 == c4) || (c1 == c4 && c2 == c3)
          f t1 t2 = f t2 t1

data TreeOrHazard = Tree {t :: Tree
                         ,hs  :: Map.HashMap FilePath (ReadOrWrite, T, Cmd)}
                  | Hazard Hazard

instance Eq TreeOrHazard where
  Tree t1 f1 == Tree t2 f2 = t1 == t2
  Hazard h == Hazard h2 = h == h2
  x == x2 = False

  
reduce :: Tree -> Tree
reduce E = E
reduce l@L{} = l
reduce (Seq c1 E) = reduce c1
reduce (Seq E c2) = reduce c2
reduce (Par c1 E) = reduce c1
reduce (Par E c2) = reduce c2
reduce (Seq c1 c2) = Seq (reduce c1) (reduce c2)
reduce (Par c1 c2) = Par (reduce c1) (reduce c2)

