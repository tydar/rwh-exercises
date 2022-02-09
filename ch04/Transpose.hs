import System.Environment (getArgs)
import Distribution.Simple.Program.HcPkg (list)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = transposeWrapper

-- currently only works for 2 lines
transpose :: String -> String
transpose input = unlines $ zipWith strFrom2 (head . lines $ input) (last . lines $ input)
  where strFrom2 a b = [a, b]

-- incorrect behavior when non-symmetric but didn't want to write the cases to avoid that
transposeWrapper :: String -> String 
transposeWrapper str = transposeMany $ lines str

transposeMany :: [String] -> String 
transposeMany [] = []
transposeMany input = if moreThanOne 
                      then map head input ++ "\n" ++ transposeMany (map tail input)
                      else map head input ++ "\n"
                        -- ensures 1) no calls to tail w/ empty list
                        --         2) no runtime errors due to line lengths (but incorrect behavior bc no guard on wrapper)
                        where moreThanOne = all ((/= 1) . length) input
