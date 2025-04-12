module Main (main) where

import Data.Function ((&))
import Lib (process)
import System.Environment (getArgs)
import Text.Libyaml

pipeline :: FilePath -> FilePath -> IO ()
pipeline inputFP outputFP = decodeFile inputFP & process & encodeFile outputFP

main :: IO ()
main = do
    args <- getArgs
    let [inputFP, outputFP] = args
    pipeline inputFP outputFP