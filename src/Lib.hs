-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- module Lib
--   ( startApp,
--   )
-- where

-- import Codec.Binary.UTF8.Generic (fromString)
-- import Control.Monad.IO.Class (MonadIO (liftIO))
-- import Data.Aeson (ToJSON)
-- import Data.ByteString (ByteString)
-- import Data.Int (Int64)
-- import Data.Text (Text)
-- import Main
-- import Database (createMovie, deleteMovie, getAllMovie, readMovie, updateMovie)
-- import GHC.Generics (Generic)
-- import Network.Wai.Handler.Warp
-- import Servant hiding (Authorized, Unauthorized)
-- import Servant.Server.Experimental.Auth ()
-- import Servant.Server.Internal.BasicAuth
-- import System.Directory

-- type API = "movie" :> BasicAuth "admin" User :> ReqBody '[JSON] Movie :> Post '[JSON] Movie

-- hMovie :: User -> Movie -> Handler Movie
-- hMovie user movie = do
--   liftIO $ print "hello"
--   liftIO $ print movie
--   liftIO $ print "adding movie into db"
--   movieKey <- liftIO $ createMovie movie
--   liftIO $ print "Successfully added movie"
--   liftIO $ print movie
--   return movie

-- authUser :: ByteString -> ByteString -> Maybe User
-- authUser username password =
--   if username == "servant" && password == "server"
--     then Just (User "servant" "servant@gmail.com")
--     else Nothing

-- basicAuthServerContext :: Context '[BasicAuthCheck User]
-- basicAuthServerContext = BasicAuthCheck check :. EmptyContext
--   where
--     check :: BasicAuthData -> IO (BasicAuthResult User)
--     check (BasicAuthData username password) = do
--       case authUser username password of
--         Just user -> return (Authorized user)
--         Nothing -> return Unauthorized

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = hMovie

-- startApp :: IO ()
-- startApp =
--   Network.Wai.Handler.Warp.run
--     8080
--     ( serveWithContext
--         api
--         basicAuthServerContext
--         server
--     )
