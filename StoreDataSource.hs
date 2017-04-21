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
    GetGenreId            :: String -> Store Int
    {-GetArtistById         :: Int -> Store String
    GetArtistByName       :: String -> Store Int
    GetAlbumById          :: Int -> Store String
    GetAlbumsByArtistId   :: Int -> Store [String]
    GetTrackById          :: Int -> Store String
    GetAlbumTracks        :: Int -> Store [String]
    GetArtistTracks       :: Int -> Store [String]
    GetTrackMediatype     :: Int -> Store String
    GetPlaylistTracks     :: Int -> Store [String]
    GetPlaylist           :: Int -> Store String
    GetGenreTracks        :: Int -> Store [String]
    GetInvoice            :: Int -> Store String
    GetCustomerInvoices   :: Int -> Store [String]
    GetInvoicelines       :: Int -> Store [Int]
    GetInvoiceTracks      :: Int -> Store [String]
    GetCustomer           :: Int -> Store [String]
    GetEmployee           :: Int -> Store [String]
    GetEmployeeLowers     :: Int -> Store [String]
    GetEmployeeBoss       :: Int -> Store [String]
    GetSupportrepEmployee :: Int -> Store [String]-}
    deriving Typeable

deriving instance Show (Store a)
deriving instance Eq (Store a)

instance DataSourceName Store where
    dataSourceName _ = "TheStore1"

instance ShowP Store where
    showp = show

instance Hashable (Store a) where
    hashWithSalt salt (GetTrackByAlbumGenre a b) = hashWithSalt salt a
    hashWithSalt salt (GetAlbumId a) = hashWithSalt salt a
    hashWithSalt salt (GetGenreId a) = hashWithSalt salt a
    {-hashWithSalt salt (GetArtistById a) = hashWithSalt salt (a)
    hashWithSalt salt (GetArtistByName a) = hashWithSalt salt ()
    hashWithSalt salt (GetAlbumById a) = hashWithSalt salt (a)
    hashWithSalt salt (GetAlbumsByArtistId a) = hashWithSalt salt (a)
    hashWithSalt salt (GetTrackById a) = hashWithSalt salt (a)
    hashWithSalt salt (GetAlbumTracks a) = hashWithSalt salt (a)
    hashWithSalt salt (GetArtistTracks a) = hashWithSalt salt (a)
    hashWithSalt salt (GetTrackMediatype a) = hashWithSalt salt (a)
    hashWithSalt salt (GetPlaylistTracks a) = hashWithSalt salt ()
    hashWithSalt salt (GetPlaylist a) = hashWithSalt salt ()
    hashWithSalt salt (GetGenreTracks a) = hashWithSalt salt ()
    hashWithSalt salt (GetInvoice a) = hashWithSalt salt ()
    hashWithSalt salt (GetCustomerInvoices a) = hashWithSalt salt ()
    hashWithSalt salt (GetInvoicelines a) = hashWithSalt salt ()
    hashWithSalt salt (GetInvoiceTracks a) = hashWithSalt salt ()
    hashWithSalt salt (GetCustomer a) = hashWithSalt salt ()
    hashWithSalt salt (GetEmployee a) = hashWithSalt salt ()
    hashWithSalt salt (GetEmployeeLowers a) = hashWithSalt salt ()
    hashWithSalt salt (GetEmployeeBoss a) = hashWithSalt salt ()
    hashWithSalt salt (GetSupportrepEmployee a) = hashWithSalt salt ()-}

instance StateKey Store where
    data State Store = ConnState {connection :: Connection}

instance DataSource () Store where
    fetch (ConnState db) _ _ reqs = SyncFetch $ do
        traceRequests reqs
        forM_ reqs $ \(BlockedFetch req var) -> runQuery db req var

traceRequests :: ShowP r => [BlockedFetch r] -> IO ()
traceRequests reqs = printf "Computing %s\n" (show strs)
  where
    strs = fmap showRequest reqs
    showRequest (BlockedFetch req _) = showp req

runQuery :: Connection -> Store a -> ResultVar a -> IO ()
runQuery db (GetTrackByAlbumGenre x y) var = getTrackByAlbumGenre x y db var
runQuery db (GetAlbumId x) var = getAlbumsId x db var
runQuery db (GetGenreId x) var = getGenreId x db var
{-
runQuery db (GetArtistById x) var = getArtistById x db var
runQuery db (GetArtistByName x) var = getArtistByName x db var
runQuery db (GetAlbumById x) var = getAlbumById x db var
runQuery db (GetAlbumsByArtistId x) var = getAlbumsByArtistId x db var
runQuery db (GetTrackById x) var = getTrackById x db var
runQuery db (GetAlbumTracks x) var = getAlbumTracks x db var
runQuery db (GetArtistTracks x) var = getArtistTracks x db var
runQuery db (GetTrackMediatype x) var = getTrackMediatype x db var
runQuery _ _ _ = error "poolik"
-}

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

getGenreId :: String -> Connection -> ResultVar Int -> IO ()
getGenreId genreName db var =
    do
        r <- quickQuery' db "SELECT GenreId FROM Genre WHERE Name = ?;" [toSql genreName]
        let strings = map convRow r
        putSuccess var (head strings)
       
    where convRow :: [SqlValue] -> Int
          convRow [sqlId] = genreid
              where genreid = (fromSql sqlId)::Int
          convRow x = error $ "Unexpected result: " ++ show x

{-
getArtistById :: Int -> Connection -> ResultVar String -> IO ()
getArtistById artistId db var =
    do
        r <- quickQuery' db "SELECT Name FROM Artist WHERE ArtistId = ?;" [toSql artistId]
        let strings = map convRow r
        putSuccess var (unlines strings)
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getArtistByName :: String -> Connection -> ResultVar Int -> IO ()
getArtistByName artistName db var =
    do
        r <- quickQuery' db "SELECT ArtistId FROM Artist WHERE Name = ?;" [toSql artistName]
        let strings = map convRow r
        putSuccess var (head strings)
       
    where convRow :: [SqlValue] -> Int
          convRow [sqlId] = artistid
              where artistid = (fromSql sqlId)::Int
          convRow x = error $ "Unexpected result: " ++ show x

getAlbumById :: Int -> Connection -> ResultVar String -> IO ()
getAlbumById albumId db var =
    do
        r <- quickQuery' db "SELECT Title FROM Album WHERE AlbumId = ?;" [toSql albumId]
        let strings = map convRow r
        putSuccess var (unlines strings)
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getAlbumsByArtistId :: Int -> Connection -> ResultVar [String] -> IO ()
getAlbumsByArtistId artistId db var =
    do
        r <- quickQuery' db "SELECT Title FROM Album WHERE ArtistId = ?;" [toSql artistId]
        let strings = map convRow r
        putSuccess var strings
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getTrackById :: Int -> Connection -> ResultVar String -> IO ()
getTrackById trackId db var =
    do
        r <- quickQuery' db "SELECT Name FROM Track WHERE TrackId = ?;" [toSql trackId]
        let strings = map convRow r
        putSuccess var (unlines strings)
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getAlbumTracks :: Int -> Connection -> ResultVar [String] -> IO ()
getAlbumTracks albumId db var =
    do
        r <- quickQuery' db "SELECT Name FROM Track WHERE AlbumId = ?;" [toSql albumId]
        let strings = map convRow r
        putSuccess var strings
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getArtistTracks :: Int -> Connection -> ResultVar [String] -> IO ()
getArtistTracks artistId db var =
    do
        r <- quickQuery' db "SELECT name FROM track WHERE composer = (SELECT name FROM artist WHERE artistid = ?);" [toSql artistId]
        let strings = map convRow r
        putSuccess var strings
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x

getTrackMediatype :: Int -> Connection -> ResultVar String -> IO ()
getTrackMediatype trackId db var =
    do
        r <- quickQuery' db "SELECT Name FROM MediaType WHERE MediaTypeId = (SELECT MediaTypeId FROM Track WHERE TrackId = ?);" [toSql trackId]
        let strings = map convRow r
        putSuccess var (unlines strings)
       
    where convRow :: [SqlValue] -> String
          convRow [sqlName] = name
              where name = (fromSql sqlName)::String
          convRow x = error $ "Unexpected result: " ++ show x
-}
