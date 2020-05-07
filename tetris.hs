import Control.Concurrent
import System.Console.ANSI
import System.IO
import Data.Char
import Data.List
import Foreign.C.Types

width = 10
height = 24

-- Piece type, x, y, grid
data Piece = Piece Int Int Int [String] deriving (Show, Eq)
data Board = Board [String] deriving (Show, Eq)

getPiece :: Int -> Piece
getPiece x | x == 0 = Piece 0 3 (-1) ["####"]
           | x == 1 = Piece 1 3 (-1) [" # ", "###"]
           | x == 2 = Piece 2 2 (0) ["## ", " ##"]
           | x == 3 = Piece 3 3 (-1) ["#  ", "###"]
           | x == 4 = Piece 4 3 (-1) ["  #", "###"]
           | x == 5 = Piece 5 3 (-1) [" ##", "## "]
           | otherwise = Piece 6 3 (-1) ["##", "##"]

randPiece :: Int -> Piece
randPiece x | x < 0 = randPiece 0
            | x > 100 = randPiece 0
            | otherwise = getPiece ([2,3,2,6,4,2,1,3,3,5,0,4,3,0,1,3,3,0,1,3,3,2,3,5,2,0,0,0,2,1,0,4,4,1,3,6,3,0,4,5,1,2,4,6,1,4,4,6,1,1,2,5,3,0,3,1,5,5,0,3,2,4,4,2,1,3,5,6,2,0,5,1,2,1,1,3,1,2,2,0,5,2,5,2,1,5,2,5,1,0,2,6,3,1,3,4,6,0,4,5] !! x)

combineBoardPiece :: Board -> Piece -> Board
combineBoardPiece (Board board) piece = Board (combineBoardPiece' 0 board piece) 
    where combineBoardPiece' :: Int -> [String] -> Piece -> [String]
          combineBoardPiece' _ [] _ = []
          combineBoardPiece' curY (b:oard) piece = (combineBoardPiece'' 0 curY b piece) : (combineBoardPiece' (curY + 1) oard piece)

          combineBoardPiece'' :: Int -> Int -> String -> Piece -> String
          combineBoardPiece'' _ _ [] _ = []
          combineBoardPiece'' curX curY (b:oard) piece = (combineBoardPiece''' curX curY b piece) : (combineBoardPiece'' (curX + 1) curY oard piece)

          combineBoardPiece''' :: Int -> Int -> Char -> Piece -> Char
          combineBoardPiece''' curX curY board (Piece _ x y piece) = if (length piece) > (curY - y) && ((length (head piece)) > (curX - x)) && ((curY - y) >= 0) && ((curX - x) >= 0) then (if (piece !! (curY - y) !! (curX - x) == '#') && (board == ' ') then piece !! (curY - y) !! (curX - x) else board) else board

collisionBoardPiece :: Board -> Piece -> Bool
collisionBoardPiece (Board board) piece = collisionBoardPiece' 0 board piece
    where collisionBoardPiece' :: Int -> [String] -> Piece -> Bool
          collisionBoardPiece' _ [] _ = False
          collisionBoardPiece' curY (b:oard) piece = (collisionBoardPiece'' 0 curY b piece) || (collisionBoardPiece' (curY + 1) oard piece)

          collisionBoardPiece'' :: Int -> Int -> String -> Piece -> Bool
          collisionBoardPiece'' _ _ [] _ = False
          collisionBoardPiece'' curX curY (b:oard) piece = (collisionBoardPiece''' curX curY b piece) || (collisionBoardPiece'' (curX + 1) curY oard piece)

          collisionBoardPiece''' :: Int -> Int -> Char -> Piece -> Bool
          collisionBoardPiece''' curX curY board (Piece _ x y piece) = if (length piece) > (curY - y) && ((length (head piece)) > (curX - x)) && ((curY - y) >= 0) && ((curX - x) >= 0) then (if (piece !! (curY - y) !! (curX - x) == '#') && (board == '#') then True else False) else False

outsideBoardPiece :: Piece -> Bool
outsideBoardPiece (Piece _ x y piece) = (x < 0) || ((x + length (head piece)) > width) || (y < 0) || ((y + length (piece) > height))

lost :: Board -> Piece -> Bool
lost board piece = lost' (collisionBoardPiece board piece) piece
    where lost' :: Bool -> Piece -> Bool
          lost' comb (Piece _ x y piece) = comb && y == 0

render :: Board -> Piece -> IO()
render board piece = do clearScreen
                        render' (combineBoardPiece board piece)
    where render' :: Board -> IO()
          render' (Board board) = render'' board
          render'' :: [String] -> IO()
          render'' [] = putChar '\0'
          render'' (x:xs) =  do putChar '|'
                                putStr x
                                putStrLn "|"
                                render'' xs
           
drop :: Piece -> Piece
drop (Piece i x y piece) = Piece i x (y + 1) piece

left :: Piece -> Piece
left (Piece i x y piece) = Piece i (x - 1) y piece

right :: Piece -> Piece
right (Piece i x y piece) = Piece i (x + 1) y piece

rotate :: Piece -> Piece
rotate (Piece i x y piece) = Piece i x y ((transpose . map reverse) piece)

press :: Char -> Board -> Piece -> Piece
press x board piece | x == 'd' = if outsideBoardPiece (right piece) || collisionBoardPiece board (right piece) then piece else right piece
                    | x == 'a' = if outsideBoardPiece (left piece) || collisionBoardPiece board (left piece) then piece else left piece
                    | x == 'w' = if outsideBoardPiece (rotate piece) || collisionBoardPiece board (rotate piece) then piece else rotate piece
                    | otherwise = piece

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt

updateScore :: Board -> Int -> Int
updateScore board score = if board /= (updateBoard board) then score + 110 else score + 10

updateBoard :: Board -> Board
updateBoard (Board board) = Board (moveBoard (moveBoard (moveBoard (moveBoard (updateBoard' board)))))
    where updateBoard' :: [String] -> [String]
          updateBoard' [] = []
          updateBoard' (b:oard) = (updateBoard'' b) : (updateBoard' oard)

          updateBoard'' :: String -> String
          updateBoard'' b = if isFull b then replicate width ' ' else b

          isFull :: String -> Bool
          isFull [] = True
          isFull (x:xs) = x == '#' && isFull xs

moveBoard :: [String] -> [String]
moveBoard [] = []
moveBoard board | length board == 1 = [head board] 
                | otherwise = if isEmpty (last board) then (excludeLast2 board) ++ [last board] ++ [penultimate board] else moveBoard (excludeLast1 board) ++ [last board]
    where isEmpty :: String -> Bool
          isEmpty [] = True
          isEmpty (x:xs) = x == ' ' && isEmpty xs
          penultimate :: [String] -> String
          penultimate xs = if length xs == 1 then replicate width ' ' else last (init xs)
          excludeLast2 :: [a] -> [a]
          excludeLast2 (x:xs) | length xs <= 2 = [x]
                              | otherwise = x : (excludeLast2 xs)
          excludeLast1 :: [a] -> [a]
          excludeLast1 (x:xs) | length xs <= 1 = [x]
                              | otherwise = x : (excludeLast1 xs)


game :: Board -> Piece -> Int -> Int -> Int -> IO()
game _ _ 0 _ _ = putChar '\0'
game board piece seed count score = do let playBoard = board
                                       let fallingPiece = piece
                                       render playBoard fallingPiece
                                       threadDelay 30000
                                       putStrLn ("Score: " ++ (show score))
                                       hSetBuffering stdin NoBuffering
                                       keyPress <- getHiddenChar
                                       let nextPiece = (press keyPress playBoard (Main.drop fallingPiece))
                                       render playBoard nextPiece
                                       
                                       if lost playBoard nextPiece
                                           then putStrLn "Game Over"
                                           else if collisionBoardPiece playBoard nextPiece || outsideBoardPiece nextPiece 
                                                   then game (updateBoard (combineBoardPiece playBoard fallingPiece)) (randPiece seed) (seed + 1) (count - 1) (updateScore playBoard score)
                                                   else game playBoard nextPiece seed (count - 1) (updateScore playBoard score)

main = do putStrLn "Tetris (By Ethan Wood)"
          let playBoard = Board (replicate height (replicate width ' '))
          let fallingPiece = randPiece 0
          game playBoard fallingPiece 1 20 0
