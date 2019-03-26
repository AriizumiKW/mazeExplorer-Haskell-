import System.Environment
import System.IO

maze_path = "please overwrite"
-- overwrite this with your own path !!!

-- Useful code

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

---- Part A

-- Question 1

get_maze :: String -> IO [String]
get_maze path = do
    text <- readFile path
    let maze = lines text
    return maze

-- Question 2

print_maze :: [String] -> IO ()
print_maze text = do
    let maze = unlines text
    putStrLn maze

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall maze (x,y) = if (get maze x y) == '#'
                     then True
                     else False

-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player maze (x,y) = if (get maze x y) == '#'
                          then maze
                          else (set maze x y '@')


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) dir
 | dir == 'w' = (x,y-1)
 | dir == 's' = (x,y+1)
 | dir == 'a' = (x-1,y)
 | dir == 'd' = (x+1,y)
 | otherwise = (x,y)

-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze (x,y) dir = if is_wall maze (move (x,y) dir) == True
                          then False
                          else True

-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop maze (x,y) = do
    print_maze (place_player maze (x,y))
    str <- getLine
    let dir = head str
    case can_move maze (x,y) dir of False -> game_loop maze (x,y)
                                    True -> do let new_coor = move (x,y) dir
                                               game_loop maze new_coor
    



---- Part C

-- Question 8

is_x :: [String] -> (Int, Int) -> Bool
is_x maze (x,y) = if (get maze x y) == 'x'
                  then True
                  else False
-- if goes to one coordinate, mark it. So it will not go back.

strict_can_move :: [String] -> (Int, Int) -> Char -> Bool
strict_can_move maze (x,y) dir = if (is_wall maze (move (x,y) dir) == True) || (is_x maze (move (x,y) dir) == True)
                                 then False
                                 else True 
-- if the coordinate has been marked, it will not be selected.

write_path :: [String] -> (Int, Int) -> [(Int, Int)]
write_path [] (x,y) = []
write_path maze (x,y)
 | is_x maze (x,y-1) == True = (write_path (set maze x y '#') (x,y-1)) ++ [(x,y)]
 | is_x maze (x-1,y) == True = (write_path (set maze x y '#') (x-1,y)) ++ [(x,y)]
 | is_x maze (x+1,y) == True = (write_path (set maze x y '#') (x+1,y)) ++ [(x,y)]
 | is_x maze (x,y+1) == True = (write_path (set maze x y '#') (x,y+1)) ++ [(x,y)]
 | otherwise = [(x,y)]


get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze (x,y) (tx,ty)
 | (x,y) == (tx,ty) = write_path maze (tx,ty)
 | strict_can_move maze (x,y) 's' == True = get_path (set maze x y 'x') (x,y+1) (tx,ty)
 | strict_can_move maze (x,y) 'd' == True = get_path (set maze x y 'x') (x+1,y) (tx,ty)
 | strict_can_move maze (x,y) 'a' == True = get_path (set maze x y 'x') (x-1,y) (tx,ty)
 | strict_can_move maze (x,y) 'w' == True = get_path (set maze x y 'x') (x,y-1) (tx,ty)
 | can_move maze (x,y) 's' == True = get_path (set maze x y '#') (x,y+1) (tx,ty)
 | can_move maze (x,y) 'd' == True = get_path (set maze x y '#') (x+1,y) (tx,ty)
 | can_move maze (x,y) 'a' == True = get_path (set maze x y '#') (x-1,y) (tx,ty)
 | can_move maze (x,y) 'w' == True = get_path (set maze x y '#') (x,y-1) (tx,ty)



-- Question 9

get_target :: [String] -> (Int, Int)
get_target maze = ((length (head maze)) - 2, (length maze) - 2)
-- get the target coordinate.

write_road :: [String] -> [(Int, Int)] -> [String]
write_road maze [] = maze
write_road maze [(x,y)] = write_road (set maze x y '.') []
write_road maze ((x,y):xs) = write_road (set maze x y '.') xs
-- fill the path by '.'

main :: IO ()
main = do
    args <- getArgs
    text <- readFile (head args)
    let maze = lines text
    let path = get_path maze (1,1) (get_target maze)
    let new_maze = unlines (write_road maze path)
    putStrLn new_maze
