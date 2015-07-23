{-# LANGUAGE OverloadedStrings #-}
module Database.Rivet.Adaptor.PostgreSQL (setup, ConnectInfo(..)) where

import           Control.Monad              (void)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (unpack)
import           Database.PostgreSQL.Simple
import           Database.Rivet

setup :: Monad m
      => (m () -> IO ())
      -> ConnectInfo
      -> IO (Adaptor m)
setup h info =
  do conn <- connect info
     execute_ conn "CREATE TABLE IF NOT EXISTS migrations (name text NOT NULL\
                   \ PRIMARY KEY, run_at timestamptz NOT NULL DEFAULT now())"
     return $ Adaptor h (sql conn) (check conn) (mark conn)
  where sql :: Connection -> Text -> IO ()
        sql conn = void . execute_ conn . fromString . T.unpack
        check :: Connection -> Text -> IO Bool
        check conn name =
          not . null <$> (query conn
                                "SELECT name FROM migrations WHERE name = ?"
                                (Only name) :: IO [Only Text])
        mark :: Connection -> Text -> Direction -> IO ()
        mark conn name dir =
          case dir of
            Up ->
              void $ execute conn
                             "INSERT INTO migrations (name) values (?)"
                             (Only name)
            Down ->
              void $ execute conn
                             "DELETE FROM migrations WHERE name = ?"
                             (Only name)
