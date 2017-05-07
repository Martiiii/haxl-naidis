{-# LANGUAGE DeriveDataTypeable, GADTs, MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, TypeFamilies #-}
module StoreDataSource where
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.Hashable
import Data.Typeable
import Text.Printf
import Haxl.Core

data Store a where
    GetTrackByAlbumGenre  :: Int -> Int -> Store [String]
    GetAlbumId            :: String -> Store Int
    deriving Typeable

deriving instance Show (Store a)
deriving instance Eq (Store a)

instance DataSourceName Store where
    dataSourceName _ = "TheStore1"

instance ShowP Store where
    showp = show

instance Hashable (Store a) where
    hashWithSalt salt (GetTrackByAlbumGenre a b) = 2 * (a + b + salt)
    hashWithSalt salt (GetAlbumId a) = 2 * ((length a) + salt) + 1

instance StateKey Store where
    data State Store = ConnState {connection :: Connection}

instance DataSource () Store where
    fetch (ConnState db) _ _ reqs = SyncFetch $ do
        forM_ reqs $ \(BlockedFetch req var) -> runQuery db req var

runQuery :: Connection -> Store a -> ResultVar a -> IO ()
runQuery db (GetTrackByAlbumGenre x y) var = getTrackByAlbumGenre x y db var
runQuery db (GetAlbumId x) var = getAlbumsId x db var

initConnState :: IO (State Store)
initConnState = do
    conn <- connectSqlite3 "test2.db"
    return ConnState {connection = conn}

getTrackByAlbumGenre :: Int -> Int -> Connection -> ResultVar [String] -> IO ()
getTrackByAlbumGenre albumId genre db var =
    do
        r <- quickQuery' db "SELECT Name FROM Track WHERE AlbumId = ? AND GenreId = ?;" [toSql albumId, toSql genre]
        let strings = map convRow r
        putSuccess var strings
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getAlbumsId :: String -> Connection -> ResultVar Int -> IO ()
getAlbumsId albumName db var =
    do
        r <- quickQuery' db "SELECT AlbumId FROM Album WHERE Title = ?;" [toSql albumName]
        let strings = map convRow r
        putSuccess var (head strings)
       
    where convRow :: [SqlValue] -> Int
          convRow [sqlName] = name
              where name = (fromSql sqlName)::Int
          convRow x = error $ "Unexpected result: " ++ show x
