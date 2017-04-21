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
    r <- runHaxl myEnv $ getTrackAlbumGenre (getAlbumId "For Those About To Rock We Salute You") (getGenre "Rock")
    putStrLn $ (show r) ++ "\n"
