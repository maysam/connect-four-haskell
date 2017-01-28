
module Board(
  Row, Column,
  Player(..),
  Board,
  newBoard,  -- :: (Row, Column) ->  Board
     -- returns an empty board with the
     -- specified number of rows and columns

  isWinningMove, -- :: Board -> Column -> Player -> Bool
     -- given an updated board and the most recent move,
     -- checks if player has won

  isDraw,  -- :: Board -> Bool
     -- checks if board is full

  showBoard,   -- :: Board -> String
     -- converts Board to String to show to user

  isLegalMove,   -- :: Board -> Column -> Bool
     -- checks if the move is legal (i.e., column not
     -- full yet, col <= no. of cols of board)

  makeMove,  -- :: Board -> Column -> Player -> Board
     -- make one move, raise runtime error if move is
     -- illegal

  getContents,   -- :: Board -> [[Player]]
     -- returns the board as a list of columns. The last
     -- element in each list corresponds to the checker
     -- inserted last

  getRows,   -- :: Board -> Row
     -- number of rows, as passed to newBoard


  getColumns,  -- :: Board -> Column
     -- number of columns, as passed to newBoard

  winner,  -- :: Board -> Int
     -- (helper function) find the winner and display as Int

  turn,    -- :: Board -> Player
     -- (helper function) find out whose turn it is
  )
where

import Data.List
import Prelude hiding (getContents)

-- given data types
-- ==================================
type Column = Int
type Row    = Int

data Player = Red | Yellow
        deriving (Eq, Show)

-- definition of abstract data types
-- ==================================
-- (define Board as an ADT)
data Board  = Con [[Player]] (Row, Column)
        deriving (Eq, Show)

-- creates new board
--
newBoard:: (Row, Column) -> Board
newBoard (r, c) = Con (replicate c []) (r, c)

-- given an updated board and most recent move,
-- checks if player has won
--
isWinningMove:: Board -> Column -> Player -> Bool
isWinningMove b c p | winner ( makeMove b c p ) == 1 = True
        | winner ( makeMove b c p ) == 2 = True
        | otherwise = False

--check the board for winner in the columns

--checks the column
--
checkColumn :: [Player] -> Player -> Bool
checkColumn [] p = False
checkColumn (x:y:z:q:t) p | x==y && x==z && x ==q && x == p = True
        | otherwise = checkColumn (y:z:q:t) p
checkColumn (x:t) p = False

--check vertical column win or not
--
checkVertical :: Board -> Player -> Bool
checkVertical (Con [] (r, c)) p = False
checkVertical (Con (x:t) (r, c)) p = (checkColumn x p) || (checkVertical  (Con t (r, c)) p)

-- generates other side, to put in empty spaces
--
otherSide :: Player -> Player
otherSide p | p == Red = Yellow
      | p == Yellow = Red

--gets item at position a b
--
getItem :: Board -> Int -> Int ->  Player ->  Player
getItem  (Con x (r, c)) a b p | b < length le = le !! b
            | otherwise = p
            where le | a < length x = x!!a
                            | otherwise = []

--checks right diagonals for winner
--
rightCheckItem :: Board -> Player -> Int -> Int -> Bool
rightCheckItem x p a b = q1 ==  q2 && q1 == q3 && q1 == q4 && q1 == p
      where
   p1 = otherSide p
   q1 = getItem x a b p1
   q2 = getItem x (a+1) (b+1) p1
   q3 = getItem x (a+2) (b+2) p1
   q4 = getItem x (a+3) (b+3) p1


rightCheckIter :: Board -> Player -> Int -> Int -> Bool
rightCheckIter x@(Con y (r, c)) p a b | a>c-4 && b > r - 5 = False
              | a>c-4 = rightCheckIter x p 0 (b+1)
              | otherwise = rightCheckItem x p a b || rightCheckIter x p (a+1) b

rightCheck :: Board -> Player -> Bool
rightCheck  x p = rightCheckIter  x p 0 0


--checks left diagonals for winner
--
leftCheckItem :: Board -> Player -> Int -> Int -> Bool
leftCheckItem x p a b = q1 ==  q2 && q1 == q3 && q1 == q4 && q1 == p
      where
   p1 = otherSide p
   q1 = getItem x a b p1
   q2 = getItem x (a-1) (b+1) p1
   q3 = getItem x (a-2) (b+2) p1
   q4 = getItem x (a-3) (b+3) p1

leftCheckIter :: Board -> Player -> Int -> Int -> Bool
leftCheckIter x@(Con y (r,c)) p a b
            | a >= c && b < (r-4) = leftCheckIter x p 3 (b+1)
            | a >= c && b >= (r-4) = False
            | otherwise = leftCheckItem x p a b || leftCheckIter x p (a+1) b

leftCheck :: Board -> Player -> Bool
leftCheck  x@(Con y (r, c))  p | c > 3 = leftCheckIter x p 3 0
           | otherwise = False

--checks diagonals and columns and columns of the transpose to see if Player is the winner
--
check :: Board -> Player -> Bool
check x p = (leftCheck x p) || (rightCheck x p) || (checkVertical x p) || (checkVertical (transboard x) p)

-- tranposes the board
--
transboard :: Board -> Board
transboard y@(Con x (r,c)) = p2b ( transpose ( b2p y)) r c

-- generates array of players from board
--
b2p :: Board -> [[Player]]
b2p (Con x (r, c)) = x

-- generates board from array of players
--
p2b :: [[Player]] -> Row -> Column -> Board
p2b x r c = (Con x (r,c))

--Finds the winner
--
winner :: Board -> Int
winner x | check x Red = 1
   | check x Yellow = 2
   | otherwise = 0

-- checks if board is full
--
isDraw:: Board -> Bool
isDraw (Con [] z) = True
isDraw (Con (x:xs) (r, c))
      | length x == r && isDraw (Con xs (r, c)) == True = True
      | otherwise = False

-- convert board to string
--
showBoard:: Board -> String
showBoard b@(Con x (r,c)) = (whr b 1) ++ trailer (c-1)

-- generates the trailing lines for the output
trailer :: Int -> String
trailer a | a == -1 = "+\n"
    | otherwise = "+-" ++ trailer (a-1) ++ " " ++ (show a)

--creates output for each cell
wh1 :: [Player] -> Int -> String
wh1 [] p = " |"
wh1 x 1 | head x ==Red = "#|"
  | head x ==Yellow = "o|"
  | otherwise = " |"
wh1 x p | p > (length x) = " |"
  | otherwise = (wh1 (tail x) (p-1))

--seperated the cells of one row
wh :: [[Player]] -> Int -> String
wh [x] p = wh1 x p
wh (x:t) p = wh1 x p ++ wh t p

-- joins the rows together, original table is created from columns !
whr :: Board -> Int -> String
whr (Con x (r, c)) p | p >= (c-1) = "|" ++ wh x p ++ "\n"
         | otherwise = whr (Con x (r, c)) (p+1) ++ "|"  ++ wh x p ++ "\n"

-- checks if the move is legal
--
isLegalMove:: Board -> Column -> Bool
isLegalMove (Con [] (r, c)) z = True
isLegalMove (Con x (r, c)) z | z >= c = False
           | otherwise = (kk < r)
              where
                kk = length k
                k  = (x !! z)

-- make one move, raise errors for illegal move
--
makeMove :: Board -> Column -> Player -> Board
makeMove y@(Con board (r, c)) column player = Con (a ++ b ++ c') (r, c)
          where a  = (firstHalf board column)
                b  = [((board !! column) ++ [player])]
                c' = (secondHalf board column)

--1st helper function to in makeMove, split to two part, first half
--
firstHalf:: [[Player]] -> Int -> [[Player]]
firstHalf board c = take c board

--second helper function to in makeMove, split to two part, second half
--
secondHalf:: [[Player]] -> Int -> [[Player]]
secondHalf board c = drop (c + 1) board

-- necessary for interfacing
--
getContents :: Board -> [[Player]]
getContents (Con [] z) = []
getContents (Con (x:xs) z) | null x == True = []: getContents (Con xs z)
         | null x == False = [last x]: getContents (Con xs z)

-- number of rows, as passed to newBoard
getRows :: Board -> Row
getRows (Con _ (r, _)) = r

-- number of columns, as passed to newBoard
getColumns :: Board -> Column
getColumns (Con _ (_, c)) = c

--Find out whose turn it is
turn :: Board -> Player
turn (Con x (r, c))  | rc > yc = Yellow
         | otherwise = Red
           where
      rc = length (filter (==Red) u1)
      yc = length (filter (==Yellow) u1)
      u1 = concat x
