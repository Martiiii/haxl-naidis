module NaideMain where

import NaideDataSource
import Haxl.Core

paring :: IO ()
paring = do
    myEnv <- initEnv initialState ()
    r <- runHaxl myEnv (dataFetch Vanus)
    r1 <- runHaxl myEnv (dataFetch Vanus)
    print r
    print r1

initialState :: StateStore
initialState = stateSet NoState stateEmpty
