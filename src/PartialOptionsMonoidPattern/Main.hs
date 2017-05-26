module PartialOptionsMonoidPattern.Main
    ( main
    ) where
import PartialOptionsMonoidPattern.Parser

main :: IO ()
main = run =<< parseOptions

-- | This is where one typically would do the work for the executable.
run :: Options -> IO ()
run options = putStrLn $ "Running with options = " ++ show options