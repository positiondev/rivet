{-# LANGUAGE OverloadedStrings #-}

module Database.Rivet.Main where

import           Control.Monad
import           Data.Monoid
import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           Database.Rivet

data Mode = MigrateUp | MigrateDown | MigrateStatus

main :: Monad m =>
        Adaptor m ->
        Mode ->
        [(Text, Migration m ())] ->
        IO ()
main adaptor mode migrations = do
  let notRun m = fmap not $ checkMigration adaptor m
  case mode of
    MigrateUp ->
      do toRun <- filterM (notRun . fst) migrations
         mapM_ (\(name, m) -> do runMigration Up adaptor name m
                                 T.putStrLn ("Ran " <> name))
               toRun
    MigrateDown ->
      do toDown <- dropWhileM (notRun . fst)
                              (reverse migrations)
         case toDown of
           ((name, m) :_) -> do runMigration Down adaptor name m
                                T.putStrLn ("Reverted " <> name)
           [] -> putStrLn "No migrations remaining."
    MigrateStatus ->
      mapM_ (\(m,_) ->
        do r <- checkMigration adaptor m
           if r
             then T.putStrLn $ " APPLIED " <> m
             else T.putStrLn m)
        migrations
  where dropWhileM _ [] = return []
        dropWhileM f (x:xs) = do x' <- f x
                                 if x'
                                    then dropWhileM f xs
                                    else return (x:xs)
