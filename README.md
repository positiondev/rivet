# Rivet - a database migration library for Haskell.

## How it works

You write SQL or haskell DSL database migrations, and they are
compiled into a `migrate` binary that knows how to run migrations,
roll them back, and inspect the state of the database. Since it is a
binary, it can be shipped around via whatever deployment mechanism you
use, and will be available in your production environment (where it
can talk to production databases), if you need that.

## Usage

All migrations will be Haskell source files in a `migrations`
directory. You should create that, and then add a target to your cabal
file.

```
Executable migrate
  hs-source-dirs: src migrations
  main-is: rivet.hs
  Build-depends: base,
                 ... -- same as your application
  default-language: Haskell2010
```

Then you should create `migrations/rivet.hs`. Currently, there is
still some copying / pasting, in particular, to figure out how to
connect to the database. The following will work, first loading
environment variables from a `.env` file if it exists, and then
looking for an `env.cfg` file in the `configurator` format where it
will find database connection info. You can change this to be however
your application manages database connections. The preprocessor line
(second in file) and the last two lines in the `main` function are
required; everything else could vary.

```
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF rivet-autoimporter #-}
module Main where

import qualified Configuration.Dotenv
import           Control.Monad                     (when)
import qualified Data.Configurator                 as C
import           Data.Monoid
import           Database.Rivet.Adaptor.PostgreSQL
import qualified Database.Rivet.Main               as Rivet
import           System.Directory                  (doesFileExist)
import           System.Environment


main :: IO ()
main = do e <- doesFileExist ".env"
          when e $ Configuration.Dotenv.loadFile False ".env"
          args <- getArgs
          let (env, mode) =
               case args of
                 [env', "up"] -> (env', Rivet.MigrateUp)
                 [env', "down"] -> (env', Rivet.MigrateDown)
                 [env', "status"] -> (env', Rivet.MigrateStatus)
                 _ -> error "Usage: [executable] [devel|prod|...] [up|down|status]"
          conf <- C.load [C.Required (env <> ".cfg")]
          host <- C.require conf "postgresql-simple.host"
          port <- C.require conf "postgresql-simple.port"
          user <- C.require conf "postgresql-simple.user"
          pass <- C.require conf "postgresql-simple.pass"
          db <- C.require conf "postgresql-simple.db"
          adaptor <- setup id (ConnectInfo host port user pass db)
          Rivet.main adaptor mode migrations
```

Once you've done that, build your project as normal, and then run the
produced `migrate` binary. The main file provided has usage
instructions, though if you customize it, obviously these might vary.
