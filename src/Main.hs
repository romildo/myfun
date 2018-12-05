module Main where

import Parser (parse)
import MyFunParser (cmd)
import Interpreter (execute, initialMemory)

main :: IO ()
main =
  do -- read the input program
     input <- getContents
     -- do syntax analysis of the program
     case parse cmd input of
       -- it succeed: execute the program
       Just (program, "") ->
         do mem <- execute initialMemory program
            -- print the final memory for debugging purposes
            print mem
       -- it failed: show an error message
       _ -> putStrLn "syntax error"
