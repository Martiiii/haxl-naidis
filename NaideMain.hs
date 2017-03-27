module NaideMain where

import NaideDataSource
import Haxl.Core

main :: IO ()
main = do
    myEnv <- initEnv initialState ()
    r <- runHaxl myEnv (dataFetch Vanus)
    print r

initialState :: StateStore
initialState = stateSet NoState stateEmpty