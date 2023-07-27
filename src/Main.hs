{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Text hiding (replace)
import Database.Persist.Postgresql (ConnectionPool, Entity (..), createPostgresqlPool, runMigration, runSqlPool)
import Database.Persist.TH
import Control.Monad.Logger (runStdoutLoggingT)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (delete, get, insert, replace, selectFirst, selectList, toSqlKey, (==.))
import Data.Int (Int64)
import Servant
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Control.Monad (forM)
import Database.Persist.Sqlite

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User json
    name Text
    password Text
    UniqueUser name
    deriving Show Eq
    
UserSession json
    user UserId
    token Text
    UniqueToken token
    deriving Show Eq

Movie json
    name Text
    rating Int
    genre Text
    userId UserId
    deriving Show

Favorite json
    userid UserId 
    movieid MovieId
    deriving Show Eq
|]

type API =
  "register" :> ReqBody '[JSON] User :> PostCreated '[JSON] Text
    :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] Text
    :<|> "logout" :> Capture "username" Text :> Get '[JSON] Text
    :<|> "movie" :> ReqBody '[JSON] Movie :> PostCreated '[JSON] Text
    :<|> "movie" :> Capture "id" Text :> ReqBody '[JSON] Movie :> Put '[JSON] Text
    :<|> "movie" :> Capture "id" Text :> Delete '[JSON] Text
    :<|> "movies" :> Get '[JSON] [Entity Movie]
    :<|> "addFavourite" :> Capture "userid" Text :> ReqBody '[JSON] Movie :> Post '[JSON] Text
    :<|> "removeFavourite" :> Capture "userid" Text :> ReqBody '[JSON] Movie :> Post '[JSON] Text
    :<|> "getFavourites" :> Capture "userid" Text :> Get '[JSON] [Entity Movie]

server :: ConnectionPool -> Server API
server pool =
  registerUser pool
    :<|> loginUser pool
    :<|> logoutUser pool
    :<|> addMovie pool
    :<|> updateMovie pool
    :<|> deleteMovie pool
    :<|> listMovies pool
    :<|> addFavouriteMovie pool 
    :<|> deleteFavouriteMovie pool 
    :<|> listFavouriteMovie pool


registerUser :: ConnectionPool -> User -> Handler Text
registerUser pool user = do
  maybeUser <- liftIO $ runSqlPool (selectFirst [UserName ==. userName user] []) pool
  case maybeUser of
    Just _ -> throwError err409 {errBody = "Username is taken. Please choose a different username."}
    Nothing -> do
      _ <- liftIO $ runSqlPool (insert user) pool
      return "User Registered Successfully"

loginUser :: ConnectionPool -> User -> Handler Text
loginUser pool user = do
  maybeUser <- liftIO $ runSqlPool (selectFirst [UserName ==. userName user, UserPassword ==. userPassword user] []) pool
  case maybeUser of
    Nothing -> throwError err404 {errBody = "Invalid username or password."}
    Just (Entity userId _) -> do
      maybeUserSession <- liftIO $ runSqlPool (selectFirst [UserSessionUser ==. userId] []) pool
      case maybeUserSession of 
        Just _ -> throwError err409 {errBody = "User already logged in."}
        Nothing -> do 
          token <- liftIO $ toString <$> nextRandom
          _ <- liftIO $ runSqlPool (insert (UserSession userId (pack token))) pool
          return $ "Logged in successfully. Your token is " <> pack token

logoutUser :: ConnectionPool -> Text -> Handler Text
logoutUser pool username = do
  maybeUser <- liftIO $ runSqlPool (selectFirst [UserName ==. username] []) pool
  case maybeUser of
    Nothing -> throwError err404 {errBody = "No user with this username exists."}
    Just (Entity userId _) -> do
      maybeUserSession <- liftIO $ runSqlPool (selectFirst [UserSessionUser ==. userId] []) pool
      case maybeUserSession of
        Nothing -> throwError err401 {errBody = "This user is not currently logged in."}
        Just (Entity sessionId _) -> do
          liftIO $ runSqlPool (delete sessionId) pool
          return "You have been logged out!"

addMovie :: ConnectionPool -> Movie -> Handler Text
addMovie pool movie = do
  maybeMovie <- liftIO $ runSqlPool (selectFirst [MovieName ==. movieName movie] []) pool
  case maybeMovie of 
    Just _ -> throwError err409 {errBody = "Movie already in database."}
    Nothing -> do 
      _ <- liftIO $ runSqlPool (insert movie) pool
      return "Movie added to the database!"

updateMovie :: ConnectionPool -> Text -> Movie -> Handler Text
updateMovie pool movieId movie = do
  let movieKey = toSqlKey (read $ unpack movieId :: Int64) :: MovieId
  maybeMovie <- liftIO $ runSqlPool (get movieKey) pool
  case maybeMovie of
    Nothing -> throwError err404 {errBody =  "Movie not found in database. Try to add movie instead."}
    Just _ -> do
      _ <- liftIO $ runSqlPool (replace movieKey movie) pool
      return "Movie updated in the database!"

deleteMovie :: ConnectionPool -> Text -> Handler Text
deleteMovie pool movieId = do
  let movieKey = toSqlKey (read $ unpack movieId :: Int64) :: MovieId
  maybeMovie <- liftIO $ runSqlPool (get movieKey) pool
  case maybeMovie of
    Nothing -> throwError err404 {errBody = "Unable to delete movie. Movie not found in database."}
    Just _ -> do
      _ <- liftIO $ runSqlPool (delete movieKey) pool
      return "Movie has been deleted from the database!"

listMovies :: ConnectionPool -> Handler [Entity Movie]
listMovies pool = do liftIO $ runSqlPool (selectList [] []) pool

addFavouriteMovie :: ConnectionPool -> Text -> Movie -> Handler Text
addFavouriteMovie pool userid movie = do 
  let userkey = toSqlKey (read $ unpack userid :: Int64) :: UserId
  maybeUser <- liftIO $ runSqlPool (get userkey) pool 
  case maybeUser of 
    Nothing -> throwError err404 {errBody = "User does not exist."}
    Just _ -> do 
      maybeMovie <- liftIO $ runSqlPool (selectFirst [MovieName ==. movieName movie] []) pool
      case maybeMovie of 
        Nothing -> throwError err404 {errBody = "Movie does not exist in database. Try to add movie first."} -- return "Movie does not exist in database. Add movie first."
        Just (Entity movieId _) -> do 
          maybeFavorite <- liftIO $ runSqlPool (selectFirst [FavoriteUserid ==. userkey, FavoriteMovieid ==. movieId] []) pool
          case maybeFavorite of 
            Just _ -> throwError err409 { errBody = "Movie is already favourited by the user." }
            Nothing -> do 
              liftIO $ runSqlPool (insert (Favorite userkey movieId)) pool
              return "Movie added to favourites."

deleteFavouriteMovie :: ConnectionPool -> Text -> Movie -> Handler Text 
deleteFavouriteMovie pool userid movie = do 
  let userkey = toSqlKey (read $ unpack userid :: Int64) :: UserId 
  maybeUser <- liftIO $ runSqlPool (get userkey) pool 
  case maybeUser of 
    Nothing -> throwError err404 {errBody = "User does not exist."}
    Just _ -> do 
      maybeMovie <- liftIO $ runSqlPool (selectFirst [MovieName ==. movieName movie] []) pool 
      case maybeMovie of 
        Nothing -> throwError err404 {errBody = "Movie does not exist in database. Try to add movie first."}
        Just (Entity movieId _) -> do 
          maybeFavorite <- liftIO $ runSqlPool (selectFirst [FavoriteUserid ==. userkey, FavoriteMovieid ==. movieId] []) pool
          case maybeFavorite of 
            Nothing -> throwError err409 { errBody = "Movie is not favourited by the user." }
            Just (Entity favid _) -> do 
              liftIO $ runSqlPool (delete favid) pool
              return "Movie deleted from favourites."

listFavouriteMovie :: ConnectionPool -> Text -> Handler [Entity Movie] 
listFavouriteMovie pool userid = do
  let userkey = toSqlKey (read $ unpack userid :: Int64) :: UserId 
  maybeUser <- liftIO $ runSqlPool (get userkey) pool 
  case maybeUser of
    Nothing -> throwError err404 {errBody = "User does not exist."}
    Just _ -> do
      favoriteEntities <- liftIO $ runSqlPool (selectList [FavoriteUserid ==. userkey] []) pool
      let movieIds = Prelude.map (\(Entity _ favorite) -> favoriteMovieid favorite) favoriteEntities
      movies <- forM movieIds $ \movieId -> do
        maybeMovie <- liftIO $ runSqlPool (get movieId) pool
        case maybeMovie of
          Nothing -> throwError err404 {errBody = "Movie does not exist."}
          Just movie -> return (Entity movieId movie)
      return movies

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ server pool

main :: IO ()
main = runStdoutLoggingT $ do
  pool <- createPostgresqlPool "dbname=test host=localhost user=newuser password=password" 10
  liftIO $ flip runSqlPool pool $ do
    runMigration migrateAll
  liftIO $ run 8080 $ app pool