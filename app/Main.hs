module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           PE                  (getFunc)

optProbId :: Parser Int
optProbId = argument auto (metavar "PROBLEM-ID" <> help "The id of the problem")

optVar :: Parser Int
optVar = argument auto (metavar "VARIABLE" <> help "The variable used in the problem")

data Option = Option
    { probId :: Int
    , var    :: Int
    }

optParser :: Parser Option
optParser = Option <$> optProbId <*> optVar

main :: IO ()
main = print . solve =<< execParser opts
  where
    opts = info (optParser <**> helper) (fullDesc <> progDesc "Solve Project Euler problems")

solve :: Option -> Int
solve opt =
    case f of
        Just g  -> g $ var opt
        Nothing -> error "Invalid problem id"
  where
    f = getFunc $ probId opt
