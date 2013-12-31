{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ScottyM, ActionM, get, post, file, redirect, setHeader, scotty, param, body, html)
import Control.Monad.IO.Class (liftIO)
import Message
import Data.Maybe (fromJust)
import Data.Int (Int64)
import Data.Text.Encoding (decodeASCII)
import Data.Text.Lazy (pack, fromStrict)
import Data.ByteString.Lazy (toStrict)
import Database.Persist (insert, KeyBackend(Key))
import Database.Persist.Types (PersistValue(PersistInt64))
import qualified Database.Persist as DB (get)
import Database.Persist.Sqlite (runMigration, insert)

import Debug.Trace

iDoor :: ScottyM ()
iDoor = do
	get "/" messages
	get "/messages" messages
	get "/messages/:id" $ do
		imageID <- param "id"
		imagecontent <- liftIO $ readMessage $ Key $ PersistInt64 $ fromIntegral $ read imageID
		case imagecontent of 
			Nothing -> do
				setHeader "Content-Type" "text/plain"
				html "Message not found!"
			Just content -> do
				setHeader "Content-Type" "image/jpeg"
				html $ fromStrict $ postContent content

	post "/messages" $ do
		image <- body
		liftIO $ runDb $ insert $ Post $ decodeASCII $ toStrict image
		redirect "/"

messages :: ActionM ()
messages = do
	setHeader "Content-Type" "text/html"
	file "test.html" --todo: show all the messages

main :: IO ()
main = do
	runDb $ runMigration migrateAll
	scotty 8003 iDoor