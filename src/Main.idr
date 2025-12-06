module Main

import Y2015.Day1
import Y2025.Day1
import Y2025.Day2
import Y2025.Day3
import Y2025.Day4
import Y2025.Day5

import System
import System.File

funcAndDataFile : List String -> (String, String -> IO ())
funcAndDataFile [_, _, "2015", "1", "1"] = ("data/2015/day1.txt", printLn . solve2015D1P1)
funcAndDataFile [_, _, "2015", "1", "2"] = ("data/2015/day1.txt", printLn . solve2015D1P2)
funcAndDataFile [_, _, "2025", "1", "1"] = ("data/2025/day1.txt", printLn . solve2025D1P1)
funcAndDataFile [_, _, "2025", "1", "2"] = ("data/2025/day1.txt", printLn . solve2025D1P2)
funcAndDataFile [_, _, "2025", "2", "1"] = ("data/2025/day2.txt", printLn . solve2025D2P1)
funcAndDataFile [_, _, "2025", "2", "2"] = ("data/2025/day2.txt", printLn . solve2025D2P2)
funcAndDataFile [_, _, "2025", "3", "1"] = ("data/2025/day3.txt", printLn . solve2025D3P1)
funcAndDataFile [_, _, "2025", "3", "2"] = ("data/2025/day3.txt", printLn . solve2025D3P2)
funcAndDataFile [_, _, "2025", "4", "1"] = ("data/2025/day4.txt", printLn . solve2025D4P1)
funcAndDataFile [_, _, "2025", "4", "2"] = ("data/2025/day4.txt", printLn . solve2025D4P2)
funcAndDataFile [_, _, "2025", "5", "1"] = ("data/2025/day5.txt", printLn . solve2025D5P1)
funcAndDataFile [_, _, "2025", "5", "2"] = ("data/2025/day5.txt", printLn . solve2025D5P2)
funcAndDataFile [_, _, _, _, _]          = ("src/Main.idr", const $ putStrLn "Not yet implemented")
funcAndDataFile _                        =
  ("src/Main.idr", const $ putStrLn "Wrong number of arguments: Usage is `pack run aoc -- <YEAR> <DAY> <PART>`")

main : IO ()
main = do
  args <- getArgs
  let (file, func) := funcAndDataFile args
  Right contents <- readFile file
    | Left err => print err
  func contents
