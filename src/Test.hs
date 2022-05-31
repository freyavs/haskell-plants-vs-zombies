module Main where

import System.Environment
import System.IO
import System.FilePath
import System.Exit
import qualified Control.Exception as E
import Pvz
import Parser
import Data.Maybe (mapMaybe)

--------------------------------------
----- Set up start

startWorld :: [Defence] -> Level -> TestWorld
startWorld def Level{phases = p} = TestWorld (placeDefence def) (concatMap spawns p)

placeDefence :: [Defence] -> [Flower]
placeDefence = map (\(x, y, f) -> makeFlower f (x, y+1))

getFinishTime :: [Spawn] -> Float
getFinishTime s = makeTime (getFastestZombie (head s) s)

-----------------------------------
---- "Playing"

getFlowersOnLane :: Int -> [Flower] -> [Flower]
getFlowersOnLane l = filter (\flower -> let (x,y) = fcell flower in y == l)

getZombiesOnLane :: Int -> [Spawn] -> [Spawn]
getZombiesOnLane l = filter (\s -> let y = lane s in y == l)

processLane:: [Flower] -> [Spawn] -> [Spawn]
processLane [] s = s
processLane (f:_) s = processSpawn f s

processSpawn :: Flower -> [Spawn] -> [Spawn]
processSpawn f [] = []
processSpawn f s@(Spawn z b l st r : xs) | ftype f == Walnut = Spawn z b l (st + fromIntegral attackTimeNeeded) r : xs -- als het een walnoot is wordt eerste zombie (eventueel meer) tegengehouden
                                         | ftype f == Peashooter = killZombies f s --als het er een peashooter is verwijder eventuele zombies die doodgaan
                                         | otherwise = s --Sunflower beinvloedt zombies niet
    where attackTimeNeeded = getSpeed z * ceiling (5 / fromIntegral (getDamage z))

killZombies :: Flower -> [Spawn] -> [Spawn]
killZombies f = mapMaybe (killZombie f)

killZombie :: Flower -> Spawn -> Maybe Spawn
killZombie f s | getLives (zombie s) (bucket s) - getHitsByPeas f s < 0 = Nothing
               | otherwise = Just s

getHitsByPeas :: Flower -> Spawn -> Int
getHitsByPeas p s = let (x,_) = fcell p in (columns - round x) * getSpeed (zombie s)     

getSpeed :: ZombieType -> Int
getSpeed z | z == Dog = 1 -- 1 seconde per cel
           | otherwise = 3

getDamage :: ZombieType -> Int
getDamage z | z == Citizen = 2
            | z == Dog = 3
            | otherwise = 4

getLives :: ZombieType -> Bool -> Int
getLives z b | z == Dog = 2 + l
             | otherwise = 3 + l
        where l | b = 2
                | otherwise = 0

play :: TestWorld -> TestWorld
play t@TestWorld{flows = f, zombs = s} = t{zombs = concat updatedSpawns}
    where updatedSpawns = [processLane (getFlowersOnLane lane f) (getZombiesOnLane lane s) | lane <- [1..rows]]

-----------------------------------
---- End of game

-- bij verlies: kijk welke zombie als eerste is aangekomen
getFastestZombie :: Spawn -> [Spawn] -> Spawn
getFastestZombie f [] = f
getFastestZombie f (s:sr) = getFastestZombie newFastest sr
    where newFastest | makeTime s > makeTime f = f
                     | otherwise = s

makeTime :: Spawn -> Float
makeTime Spawn{zombie = z, spawntime = t} = t + fromIntegral columns * speed
    where speed | z == Dog = 1
                | otherwise = 3

checkEnd :: TestWorld -> IO ()
checkEnd w = do case zombs w of
                 [] -> print "Victory"
                 _ -> print $ "Loss after " ++ show (getFinishTime (zombs w)) ++ " seconds"
                exitSuccess

--------------------------------------
---- Main

main :: IO ()
main = do (lvl:def) <- getArgs
          level <- getLevel lvl
          def <- getDefence $ head def
          let w = play $ startWorld def level
          checkEnd w

getLevel :: FilePath -> IO Level
getLevel f = do c <- readFile f
                E.catch (E.evaluate (parse parseLevel c)) levelHandler

getDefence :: FilePath -> IO [Defence]
getDefence f = do c <- readFile f
                  E.catch (E.evaluate (parse parseDefence c)) defenceHandler

levelHandler :: E.ErrorCall -> IO Level
levelHandler _ = do error "Error parsing level"
                    exitWith (ExitFailure 1)

defenceHandler :: E.ErrorCall -> IO [Defence]
defenceHandler _ = do error "Error parsing defence"
                      exitWith (ExitFailure 2)