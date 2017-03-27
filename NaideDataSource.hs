{-# LANGUAGE DeriveDataTypeable, GADTs, MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, TypeFamilies #-}
module NaideDataSource where
import Control.Monad
import Data.Hashable
import Data.Typeable
import Haxl.Core
import Text.Printf

data Inimene a where
    Vanus :: Inimene Int
    deriving Typeable

deriving instance Show (Inimene a)
deriving instance Eq (Inimene a)

instance DataSourceName Inimene where
    dataSourceName _ = "Inimene"

instance Show1 Inimene where
    show1 = show

instance Hashable (Inimene a) where
    hashWithSalt salt Vanus = hashWithSalt salt ()

instance StateKey Inimene where
    data State Inimene = NoState

instance DataSource () Inimene where
    fetch _ _ _ reqs = SyncFetch $ do
        traceRequests reqs
        forM_ reqs $ \(BlockedFetch req var) -> getVanus req var

getVanus :: Inimene a -> ResultVar a -> IO ()
getVanus Vanus var = putSuccess var 21

traceRequests :: Show1 r => [BlockedFetch r] -> IO ()
traceRequests reqs = printf "Computing %s\n" (show strs)
  where
    strs = fmap showRequest reqs
    showRequest (BlockedFetch req _) = show1 req