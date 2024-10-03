import Data.List (transpose)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import System.Random (randomRIO)
import Text.Printf

type Grid = [[Int]]
data Action = UP | DOWN | LEFT | RIGHT

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- nextGrid $ zeroGrid 4
    _ <- game grid
    putStrLn "Game Over!"
    return ()

game :: Grid -> IO Grid
game grid = do
    if continuable grid then play else return grid
  where
    play = do
        showGrid grid
        action <- getAction
        let grid2 = takeAction action grid
        if grid2 == grid
            then play
            else do
                newGrid <- nextGrid grid2
                game newGrid

zeroGrid :: Int -> Grid
zeroGrid n = replicate n $ replicate n 0

showGrid :: Grid -> IO ()
showGrid grid = do
    clearScreen
    setCursorPosition 0 0
    mapM_ (putStrLn . concatMap (printf "%5d")) grid

zeroes :: Grid -> [(Int, Int)]
zeroes grid = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length grid - 1], grid !! x !! y == 0]

pick :: [t] -> IO t
pick xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

nextGrid :: Grid -> IO Grid
nextGrid grid = do
    axis <- pick $ zeroes grid
    val <- pick (replicate 9 2 ++ [4])
    return $ setGrid axis val grid

setGrid :: (Int, Int) -> Int -> Grid -> Grid
setGrid (x, y) val grid = setCol x newRowX grid
  where
    setCol col v xs = take col xs ++ [v] ++ drop (col + 1) xs
    newRowX = setCol y val $ grid !! x

getAction :: IO Action
getAction = do
    c <- getChar
    maybe getAction return (lookup c keyMaps)
  where
    mapKeys keys = zip keys [UP, DOWN, LEFT, RIGHT]
    keyMaps = mapKeys "wsad" ++ mapKeys "kjhl"

takeAction :: Action -> Grid -> Grid
takeAction LEFT = map merged
  where
    merge (x : y : xs)
        | x == y = x + y : merge xs
        | otherwise = x : merge (y : xs)
    merge x = x
    merged xs = take size (merge (filter (/= 0) xs) ++ replicate size 0)
      where
        size = length xs
takeAction RIGHT = map reverse . takeAction LEFT . map reverse
takeAction UP = transpose . takeAction LEFT . transpose
takeAction DOWN = transpose . takeAction RIGHT . transpose

continuable :: Grid -> Bool
continuable grid = any (\x -> not (null $ zeroes (takeAction x grid))) [UP, DOWN, LEFT, RIGHT]
