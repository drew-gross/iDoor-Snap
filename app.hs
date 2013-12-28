{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ScottyM, ActionM, get, post, file, redirect, setHeader, scotty, param, body)
import Control.Monad.IO.Class (liftIO)
import Message
import Data.Text.Encoding (decodeASCII)
import Data.ByteString.Lazy (toStrict)
import Database.Persist (insert)
import Database.Persist.Sqlite (runMigration, insert)
import Network.Email.Sendmail (sendmail)

iDoor :: ScottyM ()
iDoor = do
	get "/" messages
	get "/messages" messages
	get "/messages/:id" $ do
		imageID <- param "id"
		setHeader "Content-Type" "image/jpeg"
		file $ imageID ++ ".jpeg" --TODO: the actual image

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