module StoreMain where

import StoreDataSource
import StoreHaxl
import NaideDataSource
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.Hashable
import Data.Typeable
import Text.Printf
import Haxl.Core


paring :: IO ()
paring = do
    connState <- initConnState
    myEnv <- initEnv (stateSet NoState $ stateSet connState $ stateEmpty) ()
    r <- runHaxl myEnv $ (getTrackAlbumGenre (getAlbumId (pure "For Those About To Rock We Salute You")) (getGenre (pure "Rock")))
    --r1 <- runHaxl myEnv $ (dataFetch Vanus)
    putStrLn ((show r) ++ "\n")
    --putStrLn ((show r) ++ "\n" ++ (show r1))
