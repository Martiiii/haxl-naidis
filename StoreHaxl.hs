module StoreHaxl where

import Control.Monad
import StoreDataSource
import NaideDataSource
import Haxl.Core

getTrackAlbumGenre :: GenHaxl () Int -> GenHaxl () Int -> GenHaxl () [String]
getTrackAlbumGenre albumId artist = dataFetch =<< GetTrackByAlbumGenre <$> albumId <*> artist

getAlbumId :: String -> GenHaxl () Int
getAlbumId albumName = dataFetch =<< GetAlbumId <$> (pure albumName)

getVanusNr :: GenHaxl () Int
getVanusNr = dataFetch =<< (pure Vanus)
