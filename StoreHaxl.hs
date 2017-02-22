module StoreHaxl where

import Control.Monad
import StoreDataSource
import Haxl.Core

getArtist :: GenHaxl () Int -> GenHaxl () String
getArtist artistId = join (dataFetch <$> (GetArtistById <$> artistId))

getArtistId :: GenHaxl () String -> GenHaxl () Int
getArtistId artistName = join (dataFetch <$> (GetArtistByName <$> artistName))

getAlbum :: GenHaxl () Int -> GenHaxl () String
getAlbum albumId = join (dataFetch <$> (GetAlbumById <$> albumId))

getArtistAlbums :: Int -> GenHaxl () [String]
getArtistAlbums artistId = dataFetch (GetAlbumsByArtistId artistId)

getTrack :: Int -> GenHaxl () String
getTrack trackId = dataFetch (GetTrackById trackId)

getTracks :: Int -> GenHaxl () [String]
getTracks albumId = dataFetch (GetAlbumTracks albumId)

getTracksArtist :: Int -> GenHaxl () [String]
getTracksArtist artistId = dataFetch (GetArtistTracks artistId)

getTrackType :: Int -> GenHaxl () String
getTrackType trackId = dataFetch (GetTrackMediatype trackId)

getTrackAlbumGenre :: GenHaxl () Int -> GenHaxl () Int -> GenHaxl () [String]
getTrackAlbumGenre albumId artist = join (dataFetch <$> (GetTrackByAlbumGenre <$> albumId <*> artist))

getAlbumId :: GenHaxl () String -> GenHaxl () Int
getAlbumId albumName = join (dataFetch <$> (GetAlbumId <$> albumName))

getGenre :: GenHaxl () String -> GenHaxl () Int
getGenre genreName = join (dataFetch <$> (GetGenreId <$> genreName))

{-

getArtist2 :: Int -> GenHaxl () String
getArtist2 artistId = dataFetch (GetArtistById2 artistId)

getArtistId2 :: String -> GenHaxl () Int
getArtistId2 artistName = dataFetch (GetArtistByName2 artistName)

getAlbum2 :: Int -> GenHaxl () String
getAlbum2 albumId = dataFetch (GetAlbumById2 albumId)

getArtistAlbums2 :: Int -> GenHaxl () [String]
getArtistAlbums2 artistId = dataFetch (GetAlbumsByArtistId2 artistId)

getTrack2 :: Int -> GenHaxl () String
getTrack2 trackId = dataFetch (GetTrackById2 trackId)

getTracks2 :: Int -> GenHaxl () [String]
getTracks2 albumId = dataFetch (GetAlbumTracks2 albumId)

getTracksArtist2 :: Int -> GenHaxl () [String]
getTracksArtist2 artistId = dataFetch (GetArtistTracks2 artistId)

getTrackType2 :: Int -> GenHaxl () String
getTrackType2 trackId = dataFetch (GetTrackMediatype2 trackId)
-}