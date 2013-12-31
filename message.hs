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

import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Database.Persist
import Database.Persist.Sqlite (SqlPersistT, withSqliteConn, runSqlConn)
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistUpperCase)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Post
	content ByteString
|]

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT . runResourceT . withSqliteConn "dev.idoor.sqlite3" . runSqlConn $ query

readMessage :: KeyBackend (PersistEntityBackend Post) Post -> IO (Maybe Post)
readMessage postID = runDb $ get postID