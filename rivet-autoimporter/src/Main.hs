module Main where

import           Data.List          (intercalate, isPrefixOf, isSuffixOf, sort)
import           Data.Monoid        ((<>))
import           System.Directory   (getDirectoryContents)
import           System.Environment (getArgs)
import           System.FilePath    (dropExtension)

main :: IO ()
main =
  do (src:input:output:[]) <- getArgs
     ls <- lines <$> readFile input
     let (imports, rest) = splitImports ls
     ms' <- sort <$> getDirectoryContents "migrations"
     let ms = map dropExtension $
              filter (\m -> "M" `isPrefixOf` m && ".hs" `isSuffixOf` m) ms'
     let rivetImports = map (\m -> "import qualified " <> m) ms
         rivetMigrations = ["migrations = ["] <>
           ["  " <> intercalate "\n  ," (map (\m -> "(\"" <> m <> "\", " <> m <> ".migrate)") ms)] <> ["\n  ]"]
         rivetAdded = ["\n\n"] <>
                      rivetImports <> ["\n\n"] <>
                      rivetMigrations  <> ["\n\n"]
     writeFile output $ unlines $ ["{-# LINE 1 \"" <> src <> "\" #-}"] <>
                                  imports <>
                                  rivetAdded <>
                                  rest
  where splitImports ls = splitImports' [] (reverse ls)
        splitImports' imp [] = ([], imp)
        splitImports' imp (l:ls) = if "import" `isPrefixOf` l
                                      then (reverse (l:ls), imp)
                                      else splitImports' (l:imp) ls
