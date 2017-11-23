import System.Environment

main :: IO ()
main = do
    input <- getArgs
    outputTime $ head input

outputTime :: String -> IO ()
outputTime [a, b, c, d] =
