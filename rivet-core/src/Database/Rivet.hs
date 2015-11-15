module Database.Rivet where

import           Data.Text (Text)

data Direction = Up | Down

data Syntax m = SQL Text Text | App (m ()) (m ())

data Migration m v = Migration { migValue :: v
                               , migSteps :: [Syntax m]}

instance Functor (Migration m) where
  fmap f m = m { migValue = f (migValue m) }

instance Applicative (Migration m) where
  pure v = Migration v []
  (<*>) (Migration f ss) (Migration v ss') = Migration (f v) (ss ++ ss')

instance Monad (Migration m) where
  (>>=) (Migration v ss) f = let (Migration v' ss') = f v
                             in Migration v' (ss ++ ss')
  return v = Migration v []

data Adaptor m = Adaptor { runHandler     :: m () -> IO ()
                         , runSQL         :: Text -> IO ()
                         , checkMigration :: Text -> IO Bool
                         , markMigration  :: Text -> Direction -> IO ()
                         }

runMigration :: Direction -> Adaptor m -> Text -> Migration m () -> IO ()
runMigration dir ad n m = do sequence_ (map (runStep dir ad) (migSteps m))
                             markMigration ad n dir

runStep :: Direction -> Adaptor m -> Syntax m -> IO ()
runStep Up   (Adaptor h _ _ _) (App up _)   = h up
runStep Down (Adaptor h _ _ _) (App _ down) = h down
runStep Up   (Adaptor _ s _ _) (SQL up _)   = s up
runStep Down (Adaptor _ s _ _) (SQL _ down) = s down
