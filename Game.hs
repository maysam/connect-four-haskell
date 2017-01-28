
module Main where

import Prelude hiding (getContents)
import Board

--Core function, transform connecfour into I/O, that ask users for
--input and display the board.
--
play :: Board -> IO ()
play board | winner board == 1 = putStrLn red_wins
           | winner board == 2 = putStrLn yellow_wins
           | isDraw board == True = putStrLn draw
           | otherwise = do
             x <- readInteger (turn board)
             ok <- checkMove board x
             let board1 | ok  = makeMove board x (turn board)
                        | otherwise = board
             temp <- putStrLn ("\n" ++ (showBoard board1))
             play board1


--convert False to a String: Illegal Move!
--
convert :: Bool -> String
convert True = ""
convert False = "Illegal Move!\n"

--checkMove, check the move is valid or not, reture True or False
--
checkMove :: Board -> Column -> IO Bool
checkMove b c = do
                let x = isLegalMove b c
                putStr (convert x)
                return (x)

--Prompt for user imput for each turn
--
readInteger :: Player -> IO Int
readInteger player = do
                     putStr "\nPlease enter move for "
                     putStr (show player)
                     putStrLn ": "
                     x<-getLine
                     return (read x)

-- announce wins and draws with the following strings
--
draw        = "Game Over - Draw"
yellow_wins = "Yellow wins!"
red_wins    = "Red wins!"


main :: IO ()
main = play (newBoard (6, 7))

