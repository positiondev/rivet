{-# LANGUAGE OverloadedStrings #-}
module Database.Rivet.V0 (
    Migration
  , createTable
  , dropTable
  , renameColumn
  , addColumn
  , dropColumn
  , sql
  , ColumnSpec(..)
  )  where

import           Data.Maybe     (fromMaybe)
import           Data.Monoid
import           Data.Text      (Text)

import           Database.Rivet

data ColumnSpec = ColumnSpec { colName        :: Text
                             , colType        :: Text
                             , colDefault     :: Maybe Text
                             , colConstraints :: Maybe Text
                             }

swap :: Syntax m -> Syntax m
swap (SQL up down) = SQL down up
swap (App up down) = App down up

add :: Monad m => (Text, Text) -> Migration m ()
add (up, down) = Migration () [SQL up down]

invert :: Monad m => Migration m () -> Migration m ()
invert (Migration () ps) = Migration () (map swap ps)

stripDown :: Monad m => Migration m () -> Migration m ()
stripDown (Migration () ps) = Migration () (map elimDown ps)
  where elimDown (SQL up _) = SQL up ""
        elimDown (App up _) = App up (return ())

createTable :: Monad m => Text -> [ColumnSpec] -> Migration m ()
createTable tab cols =
  do add ("CREATE TABLE " <> tab <> "()", "DROP TABLE " <> tab)
     stripDown $ mapM_ (addColumn tab) cols

-- NOTE(dbp 2014-10-18): To make this invertable, you need to pass in
-- the spec for how the table should be recreated. Obviously this is
-- reasonably unsafe, as we aren't checking that it looks like that
-- currently (so up and down may not be inverses if you mess that up).
dropTable :: Monad m => Text -> [ColumnSpec] -> Migration m ()
dropTable tab = invert . createTable tab

renameColumn :: Monad m => Text -> Text -> Text -> Migration m ()
renameColumn tab old new =
  add ("ALTER TABLE " <> tab <> " RENAME COLUMN " <> old <> " TO " <> new
      ,"ALTER TABLE " <> tab <> " RENAME COLUMN " <> new <> " TO " <> old)

addColumn :: Monad m => Text -> ColumnSpec -> Migration m ()
addColumn tab (ColumnSpec nm ty def constr) =
  add ("ALTER TABLE " <> tab <> " ADD COLUMN " <> nm <>
       " " <> ty <> maybe "" (" DEFAULT " <>) def <> " " <>
       fromMaybe "" constr,
       "ALTER TABLE " <> tab <> " DROP COLUMN " <> nm)

-- NOTE(dbp 2014-10-18): Like with 'dropTable', we have to specify
-- what the column should look like when you re-add it in order to
-- build the inverse.
dropColumn :: Monad m => Text -> ColumnSpec -> Migration m ()
dropColumn tab = invert . addColumn tab

sql :: Monad m => Text -> Text -> Migration m ()
sql up down = Migration () [SQL up down]
