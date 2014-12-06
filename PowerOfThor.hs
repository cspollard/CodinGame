import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position

    loop $ (lx, ly) `minus` (tx, ty)


type Dir = (Ordering, Ordering)

showDir :: Dir -> String
showDir d = case d of
            (GT, LT) -> "NE"
            (GT, EQ) -> "E"
            (GT, GT) -> "SE"
            (LT, LT) -> "NW"
            (LT, EQ) -> "W"
            (LT, GT) -> "SW"
            (EQ, LT) -> "N"
            (EQ, GT) -> "S"
            otherwise -> "N"


type Coord = (Int, Int)

plus :: Coord -> Coord -> Coord
plus (w, x) (y, z) = (w+y, x+z)

minus :: Coord -> Coord -> Coord
minus (w, x) (y, z) = (w-y, x-z)


toDir :: Coord -> Dir
toDir (dx, dy) = (,) (dx `compare` 0) (dy `compare` 0)

toCoord :: Dir -> Coord
toCoord (dx, dy) = (f dx, f dy)
                        where f x = case x of
                                EQ -> 0
                                GT -> 1
                                LT -> -1


loop :: Coord -> IO ()
loop c = do
    input_line <- getLine
    -- The level of Thor's remaining energy, representing the number of moves he can still make.
    let e = read input_line :: Int

    -- direction in which we're going to move
    let d = toDir c

    hPutStrLn stderr $ show c
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn $ showDir d

    loop $ c `minus` toCoord d
