import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- The code below will read all the game information for you.
    -- On each game turn, information will be available on the standard input, you will be sent:
    -- -> the total number of visible enemies
    -- -> for each enemy, its name and distance from you
    -- The system will wait for you to write an enemy name on the standard output.
    -- Once you have designated a target:
    -- -> the cannon will shoot
    -- -> the enemies will move
    -- -> new info will be available for you to read on the standard input.

    loop


type Enemy = (Int, String)


dist = fst
name = snd


enemy :: String -> Enemy
enemy line = let ws = words line in (read $ ws !! 1, ws !! 0)


loop :: IO ()
loop = do
    -- The number of current enemy ships within range
    count <- read `fmap` getLine :: IO Int

    -- a list of enemies
    enemies <- replicateM count $ enemy `fmap` getLine

    let e = head . sort $ enemies
    putStrLn $ name e

    loop
