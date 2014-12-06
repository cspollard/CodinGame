import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    loop


data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq, Ord)


alt :: Coord -> Coord -> Ordering
alt c1 c2 = y c2 `compare` y c1


loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let sx = read (input!!0) :: Int
    let sy = read (input!!1) :: Int

    -- read in and sort the mountains by height
    mountains <- sortBy alt `fmap` mapM (\i -> fmap (Coord i . read) getLine) [0..7]

    -- either:  FIRE (ship is firing its phase cannons) or HOLD (ship is not firing).
    if (x . head $ mountains) == (sx)
        then putStrLn "FIRE"
        else putStrLn "HOLD"
    
    loop
