{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Message where

import Data.Text (Text)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist
import Database.Persist.Sqlite (SqlPersist, withSqliteConn, runSqlConn)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistUpperCase)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Post
	content Text
|]

runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = runResourceT . withSqliteConn ":memory:" . runSqlConn $ query