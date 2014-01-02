{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ScottyM, ActionM, get, post, file, redirect, setHeader, scotty, param, body, html, raw)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Message
import Data.Maybe (fromJust)
import Data.Int (Int64)
import Data.Text (unpack)
import Data.Text.Lazy (pack)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Char8 (putStrLn)
import Database.Persist (insert, KeyBackend(Key), fromPersistValueText, unKey)
import Database.Persist.Types (PersistValue(PersistInt64))
import qualified Database.Persist as DB (get)
import Database.Persist.Sqlite (runMigration, insert)
import Network.Email.Sendmail (sendmail)
import Prelude hiding (putStrLn)

emailBodyFromID msgID = "Subject: New message from iDoor!\n" ++
						"Content-Type: text/html\n" ++
						"<img src=\"http://drewgross.com:8001/messages/" ++ msgID ++ "\">" ++
						"<br>" ++
						"<a href=\"http://drewgross.com:8001/messages/" ++ msgID ++ "\">"

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
				raw $ fromStrict $ postContent content

	post "/messages" $ do
		image <- body
		imageID' <- liftIO $ runDb $ insert $ Post $ toStrict image
		let imageID = fromPersistValueText $ unKey imageID'
		let messageBody = case imageID of
			Left errorMsg -> errorMsg
			Right imageID -> emailBodyFromID $ unpack imageID
		liftIO $ sendmail (Just "idoor@idoor.drewgross.com") ["drew.a.gross@gmail.com"] messageBody
		redirect "/"

messages :: ActionM ()
messages = do
	setHeader "Content-Type" "text/html"
	file "test.html" --todo: show all the messages

main :: IO ()
main = do
	runDb $ runMigration migrateAll
	scotty 8001 iDoor