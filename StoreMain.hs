module StoreMain where

import StoreDataSource
import StoreHaxl
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.Hashable
import Data.Typeable
import Text.Printf
import Haxl.Core

main :: IO ()
main = do
    connState <- initConnState
    --connState2 <- initConnState2
    myEnv <- initEnv (stateSet connState stateEmpty) ()
    --myEnv2 <- initEnv (stateSet connState2 stateEmpty) ()
    r <- runHaxl myEnv $ (getTrackAlbumGenre (getAlbumId (pure "For Those About To Rock We Salute You")) (getGenre (pure "Rock")))
        
    putStr ((show r) ++ "\n")
    --g <- runHaxl myEnv (getAlbum 1)
    --putStr r
    --putStr g
    {-f <- runHaxl myEnv (getArtistAlbums 1)
    l <- runHaxl myEnv (getTrack 1)
    k <- runHaxl myEnv (getTracks 1)
    m <- runHaxl myEnv (getTracksArtist 1)
    n <- runHaxl myEnv (getTrackType 1)
    putStr r
    putStr g
    putStr ((show f) ++ "\n")
    putStr l
    putStr ((show k) ++ "\n")
    putStr ((show m) ++ "\n")
    putStr n-}