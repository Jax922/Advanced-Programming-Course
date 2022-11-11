import Definitions
import Parser
import Transformer
import Generator

import System.Exit (die)
import System.Environment (getArgs)
import Text.Read (readEither)
import Data.List (intercalate)

data Opts = Opts {outputFile :: String, dolre, dolfactor :: Bool}

opts0 :: Opts
opts0 = Opts {outputFile = "APpyParser.hs", dolre = True, dolfactor = True}

processOpts :: [String] -> Opts -> (Opts, [String])
processOpts ("-o" : fn : ss) o = processOpts ss (o {outputFile = fn})
processOpts ("--no-lre" : ss) o = processOpts ss (o {dolre = False})
processOpts ("--no-lfactor" : ss) o = processOpts ss (o {dolfactor = False})
processOpts ss o = (o, ss)

usage = "Usage: appy [-o FILE.hs] [--no-lre] [--no-lfactor] FILE.appy"

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

main :: IO ()
main =
  do ss <- getArgs
     let (opts, ss1) = processOpts ss opts0
     if length ss1 == 1 then return () else die usage
     s <- readFile (head ss1)
     (p,eg) <- fromEM "parsing" $ parseSpec s
     g <- fromEM "converting" $ convert eg
     lg <- if dolre opts then fromEM "lre" $ lre g else return  g
     flg <- if dolfactor opts then fromEM "factoring" $ lfactor lg else return lg
     let r = p ++ prelude ++ render flg
     putStrLn $ "Writing to " ++ outputFile opts
     writeFile (outputFile opts) r

