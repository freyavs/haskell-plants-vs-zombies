module Main where


---------------------------------------
---- Imports

import System.Environment
import System.IO
import System.FilePath
import System.Exit
import Data.List
import System.Directory
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Bitmap 
import Control.Monad.State
import Data.Maybe (mapMaybe, maybeToList,fromJust)
import Pvz
import Parser

---------------------------------------
---- Graphics 

scale  :: Int
scale = 140

shoplength :: Float
shoplength = 4

timebarlength :: Float
timebarlength = fromIntegral columns - shoplength
 
square :: G.Color -> G.Picture 
square c = G.color c (G.rectangleSolid h h)
        where h = fromIntegral (scale - 20)

rect :: G.Color -> G.Picture 
rect c = G.color c (G.rectangleSolid h (h-30))
        where h = fromIntegral scale

window :: G.Display
window = G.InWindow "UGent Functioneel Programmeren Opdracht 4" 
                                    (columns*scale+(scale `div` 2), (rows+1)*scale)
                                    (0,0)

cellToPic :: Cell -> G.Picture -> G.Picture
cellToPic (x,y) = G.translate xn (-yn)
    where xn = x * fromIntegral scale - fromIntegral ((columns * scale) `div` 2) + fromIntegral (scale `div` 2)
          yn = fromIntegral $ (y*scale - (((rows+1)*scale) `div` 2)) + scale `div` 2

barToPic :: Cell -> Float -> G.Picture -> G.Picture
barToPic (x,y) b = G.translate xn (-yn)
    where xn = x * fromIntegral scale - fromIntegral ((columns * scale) `div` 2) +  ((b * fromIntegral scale) / 2)
          yn = fromIntegral $ (y*scale - (((rows+1)*scale) `div` 2)) + scale `div` 2

worldToPic :: World -> IO G.Picture
worldToPic w@World{level = (Selecting l)} = return $ selectingWorldToPic w l
worldToPic w | status w == Ongoing = return $ playingWorldToPic w
             | otherwise = return $ endedWorldToPic w

levelToPic :: Level -> G.Picture
levelToPic Level{name = n, difficulty = dif, duration = d} = textToPic 0.15 (n ++ " - difficulty: " ++ show dif ++ " - duration: " ++ show d ++ " seconds")

textToPic :: Float -> String -> G.Picture
textToPic s str = G.scale s s $ G.text str

statusToPic :: World -> G.Picture
statusToPic World{status = s} | s == Won = cellToPic (0,0) (textToPic 0.4 "You defeated the zombies!")
                              | otherwise = cellToPic (0,0) (textToPic 0.4 "The zombies reached you...")

selectingWorldToPic :: World -> [Level] -> G.Picture
selectingWorldToPic World{pics = ps} l = G.pictures ([cellToPic (fromIntegral ((columns - 1) `div` 2),rows `div` 2) (screen ps)]
                                        ++ [cellToPic (fromIntegral i,j) (rect G.white) | i <- [0..(columns-1)], j <- [0..rows]]
                                        ++ [cellToPic (0,0) (textToPic 0.35 "Ready to slay some zombies? Select a level:")]
                                        ++ [cellToPic (0,i+1) $ levelToPic (l !! i) | i <- [0..(minimum [rows-1, length l - 1])]] --toont max 5 levels op het scherm
                                        ++ [cellToPic (0,rows) (textToPic 0.3 "Exit game")]
                                        )

playingWorldToPic :: World -> G.Picture
playingWorldToPic w@World{flowers = f, zombies = z, peas = p, saldo = s, level = l, time = t, pics = ps } = G.pictures ( [cellToPic  (fromIntegral ((columns - 1) `div` 2),rows `div` 2) (lawn ps)]
                                                                                                        ++ map  (flowerToPic w) f
                                                                                                        ++ map (zombieToPic w) z
                                                                                                        ++ map peaToPic p
                                                                                                        ++ [cellToPic (fromIntegral x,0) (square G.yellow) | x <- [0..length $ seeds l]]
                                                                                                        ++ [saldoToPic s] 
                                                                                                        ++ shopToPic w
                                                                                                        ++ phasesToPic (duration l) (phases l)
                                                                                                        ++ [timeToPic (duration l) t])

endedWorldToPic :: World -> G.Picture
endedWorldToPic w@World{pics = ps} = G.pictures ( [cellToPic (fromIntegral ((columns - 1) `div` 2),rows `div` 2) (screen ps)]
                                                ++ [cellToPic (fromIntegral i,j) (rect G.white) | i <- [0..(columns-1)], j <- [0,rows,rows-1,rows-2]]
                                                ++ [statusToPic w]
                                                ++ [cellToPic (0,rows-2) (textToPic 0.2 "Restart level")]
                                                ++ [cellToPic (0,rows-1) (textToPic 0.2 "Select another level")]
                                                ++ [cellToPic (0,rows) (textToPic 0.2 "Exit game")]
                                                )
                                                
flowerToPic :: World -> Flower -> G.Picture
flowerToPic World{pics = p} Flower{ftype = t, fcell = c} = cellToPic c (getFlowerPic p t)

zombieToPic :: World -> Zombie -> G.Picture
zombieToPic World{pics = p} Zombie{ztype = t, zcell = c} = cellToPic c (G.scale 0.6 0.6 pic)
        where pic | t == Citizen = citizen p
                  | t == Dog = dog p
                  | otherwise = farmer p

getFlowerPic :: Pictures -> FlowerType -> G.Picture
getFlowerPic p t  = G.scale 0.6 0.6 pic
        where pic | t == Sunflower = sunflower p
                  | t == Peashooter = peashooter p
                  | otherwise = walnut p

peaToPic :: Pea -> G.Picture
peaToPic Pea{pcell = c} = cellToPic c (G.color G.green (G.circleSolid 20)) 

saldoToPic :: Int -> G.Picture
saldoToPic s = cellToPic (-0.25,0) (G.scale 0.4 0.4 $ G.text (show s))

shopToPic :: World -> [G.Picture]
shopToPic w@World{level = l} = let n = length (seeds l)
                                in [ cellToPic (fromIntegral $ x+1, 0) (getFlowerPic (pics w) (seeds l !! x)) | x <- [0..n-1] ] ++ [cellToPic (fromIntegral x + 0.65, 0) (costToPic (seeds l !! x)) | x <- [0..n-1]]

costToPic :: FlowerType -> G.Picture
costToPic f = textToPic 0.2 ("COST=" ++ show c)
        where c | f == Sunflower = 3
                | otherwise = 6

phasesToPic :: Float -> [Phase] -> [G.Picture]
phasesToPic t = map (phaseToPic t)

phaseToPic :: Float -> Phase -> G.Picture 
phaseToPic t p = barToPic (shoplength+b,0) (timebarlength-b) (G.color (getColor (ptype p)) (G.rectangleSolid ((timebarlength-b)*h) 20))
        where h = fromIntegral scale
              getColor t | t == BuildingPhase = G.green
                         | otherwise = G.red
              part = starttime p / t
              b = part * timebarlength

timeToPic :: Float -> Float -> G.Picture
timeToPic t now = cellToPic (shoplength+(part*timebarlength)-0.5,0) (G.color G.black (G.circleSolid 10))
        where part | t > now = now / t
                   | otherwise = 1

----------------------------------------
---- Flower mechanics

updateFlower :: Float -> Flower -> Flower
updateFlower t f@Flower{ftimer = timer, acttime = a} | (timer + t) >= a = f{ftimer = 0}
                                                                       | otherwise = f{ftimer = timer + t}
                                                                                                        
updatePeaHit :: Bool -> Pea -> Maybe Pea
updatePeaHit wasHit p | wasHit = Nothing
                      | otherwise = Just p

movePea :: Float -> Pea -> Maybe Pea
movePea t p@Pea{pcell = (x,y), pspeed = s} | round newx < columns = Just p{pcell = (newx, y)}
                                           | otherwise = Nothing
        where newx = x + (1.0 / s)*t

spawnPeas :: [Flower] -> [Pea]
spawnPeas = mapMaybe spawnPea

spawnPea :: Flower -> Maybe Pea
spawnPea Flower{ftimer = timer, fcell = c, ftype = t} | t == Peashooter && timer == 0 = Just $ makePea c
                                                      | otherwise = Nothing

peasGotHit :: [Zombie] -> [Pea] -> [Pea]
peasGotHit z = mapMaybe (\x -> updatePeaHit (peaHit z x) x)
        where peaHit z' p' = any (isHit (pcell p') . zcell) z'

flowersGotHit :: [Zombie] -> [Flower] -> [Flower]
flowersGotHit z = mapMaybe (\x -> updateFlowerLives (flowerHit z x) x)
        where flowerHit z' f' = filter (isHit (fcell f') . zcell) z'

updateFlowerLives :: [Zombie] -> Flower -> Maybe Flower
updateFlowerLives [] f = Just f
updateFlowerLives z f | newlives >= 0 = Just f{flives = newlives}
                      | otherwise = Nothing
        where newlives = flives f - getLivesToTake z

getLivesToTake :: [Zombie] -> Int
getLivesToTake [] = 0
getLivesToTake (z:zr) = dam z + getLivesToTake zr
        where dam x | ztimer x == 0 = damage x
                    | otherwise = 0

------------------------------
---- Zombie mechanics

updateZombie :: World -> Float -> Zombie -> Zombie 
updateZombie w t z@Zombie{zcell = (x,y), speed = s, zlives = l, ztimer = tim} | cellContainsFlower (x,y) w && tim >= s = z{ztimer = 0} -- als op 0 staat kan die een bloem aanvallen, redenering: zombie kan maar per cel verplaatsing 1x aanvallen
                                                                              | cellContainsFlower (x,y) w = z{ztimer = tim+t}
                                                                              | otherwise =  z{zcell = (newx, y), ztimer = tim+t}
        where newx = x - (1.0 / s)*t

updateZombieLife :: Bool -> Zombie -> Maybe Zombie
updateZombieLife wasHit z@Zombie{zlives = l} | wasHit && l - 1 >= 0 = Just $ z{zlives = l - 1}
                                             | l - 1 < 0 = Nothing
                                             | otherwise = Just z

zombiesGotHit :: [Pea] -> [Zombie] -> [Zombie]
zombiesGotHit p = mapMaybe (\x -> updateZombieLife (zombieHit p x) x)
        where zombieHit p' z' = any (isHit (zcell z') . pcell) p'

updateDamage :: World -> World 
updateDamage w@World{flowers = f, peas = p, zombies = z} = w {zombies = zombiesGotHit p z, peas = peasGotHit z p, flowers = flowersGotHit z f}

spawnZombies :: Float -> Level -> [Zombie]
spawnZombies time Level{phases = p} = concatMap (mapMaybe (makeSpawn time) . spawns) p

makeSpawn :: Float -> Spawn -> Maybe Zombie
makeSpawn time s  | spawntime s <= time = Just $ makeZombie (zombie s) (fromIntegral columns, fromIntegral $ lane s) (bucket s) 
                  | otherwise = Nothing

removeSpawnsFromPhase :: Float -> Phase -> Phase
removeSpawnsFromPhase time p@Phase{spawns = s} = p{spawns = mapMaybe (removeSpawn time) s}

removeSpawn :: Float -> Spawn -> Maybe Spawn
removeSpawn time s  | spawntime s <= time = Nothing
                    | otherwise = Just s

updateSpawns :: Float -> Level -> Level
updateSpawns _ l@(Selecting _) = l
updateSpawns time l@Level{phases = p} = l{phases = map (removeSpawnsFromPhase time) p}

-------------------------------
----- General mechanics

cellContainsFlower :: Cell -> World -> Bool
cellContainsFlower c World{flowers = f} = any (isHit c . fcell) f

isHit :: Cell -> Cell -> Bool
isHit (a,b) (x,y) = round x == round a && b == y

updateStatus :: Float -> Float -> [Zombie] -> Status
updateStatus now dur z | null z && now > dur = Won
                       | any (\x -> fst (zcell x) <= -0.5) z = Lost -- -0.5 voor esthetische redenen
                       | otherwise = Ongoing

updateWorld :: Float -> State World ()
updateWorld t = do world <- get
                   let newtime = t + time world
                   let flowersUpdated = map (updateFlower t) (flowers world) -- update flower times
                   let peasUpdated = mapMaybe (movePea t) (peas world) ++ spawnPeas flowersUpdated --update pea locations en spawn er meer
                   let saldoUpdated = updateSaldo flowersUpdated (saldo world) --update hoeveelheid saldo
                   let zombiesUpdated = map (updateZombie world t) (zombies world) ++ spawnZombies newtime (level world) --update zombie times
                   let updatedLevel = updateSpawns newtime (level world) --verwijder net gespawnde zombies
                   let updatedStatus = updateStatus newtime (duration $ level world) zombiesUpdated 
                   put world{flowers = flowersUpdated, peas = peasUpdated, zombies = zombiesUpdated, saldo = saldoUpdated, time = newtime, level = updatedLevel, status = updatedStatus} 
       
--------------------------------------
---- Shop and building mechanics  
                                                  
updateSaldo :: [Flower] -> Int -> Int
updateSaldo f saldo = saldo + length (filter (\x -> ftype x == Sunflower && ftimer x == 0) f)

clickedOnCell :: Cell -> State World ()
clickedOnCell c = do world <- get
                     let maybeFlower = getFlowerType (seeds $ level world) c
                     let s = changeSelected (selected world) maybeFlower 
                     let (f, newsaldo) = placeFlower c s world
                     let addedFlowers = flowers world ++ maybeToList f 
                     put world{flowers = addedFlowers, selected = s, saldo = newsaldo}

placeFlower :: Cell -> FlowerType -> World -> (Maybe Flower, Int)
placeFlower (x,y) f w@World{saldo = s} | y > 0 && x >= 0 && round x < columns && s - cost nf >= 0 && not (cellContainsFlower (x,y) w) = (Just nf, s - cost nf)
                                       | otherwise = (Nothing, s)
        where nf = makeFlower f (x,y)

changeSelected :: FlowerType -> Maybe FlowerType -> FlowerType
changeSelected f Nothing = f
changeSelected _ (Just f) = f

getFlowerType :: [FlowerType] -> Cell -> Maybe FlowerType
getFlowerType f (x,0) | x == 1 && b =  Just $ head f
                      | x == 2 && b =  Just $ f !! 1
                      | x == 3 && b =  Just $ f !! 2
                      | otherwise = Nothing
        where b = round x <= length f
getFlowerType _ _ = Nothing


-------------------------------
---- Handle input

selectedLevel :: Cell -> World -> IO World
selectedLevel (x,y) w | y == 0 = return w
                      | y  == rows = exitSuccess
                      | otherwise = do let (Selecting lvl) = level w 
                                       let l = lvl !! (y-1)
                                       return $ w{level = l, selected = head $ seeds l, levels = (l, lvl)}

selectedAction :: Cell -> World -> IO World
selectedAction (x,y) w | y == rows = exitSuccess
                       | y == rows - 1 = return $ startWorld (pics w) (snd $ levels w)
                       | y == rows - 2 = return $ restartWorld w
                       | otherwise = return w

getClickedCell :: (Float, Float) -> (Int, Int) -> Cell
getClickedCell (xn, yn) (b, h) = (fromIntegral $ round x, round (-y))
    where x = xn / fromIntegral scale + fromIntegral b /  2 - 0.5
          y = yn / fromIntegral scale - fromIntegral h /  2 + 0.5

handleClick :: G.Event -> World -> IO World 
handleClick (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (x', y')) w@World{level = Selecting _} = selectedLevel (getClickedCell (x', y') (columns, rows+1)) w
handleClick (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (x', y')) w@World{status = s} | s == Ongoing = return $ execState (clickedOnCell (getClickedCell (x', y') (columns, rows+1))) w
                                                                                            | otherwise = selectedAction (getClickedCell (x', y') (columns, rows+1)) w
handleClick _ w = return w     

update :: Float -> World -> IO World
update f w@World{level = Selecting _} = return w
update f w = return $ updateDamage (execState (updateWorld f) w)

----------------------------------------
---- Game

startWorld :: Pictures -> [Level] -> World
startWorld pics f = World [] [] [] Ongoing 0 20 Sunflower pics (Selecting f) (Selecting f, f)

restartWorld :: World -> World
restartWorld w = let (l,ls) = levels w in World [] [] [] Ongoing 0 20 (head $ seeds l) (pics w) l (l,ls)

-------------------------------
----- Main 

loadPictures :: IO Pictures
loadPictures = do sunflower <- loadBMP "img/sunflower.bmp"
                  peashooter <- loadBMP "img/peashooter.bmp"
                  walnut <- loadBMP "img/walnut.bmp"
                  citizen <- loadBMP "img/citizen.bmp"
                  farmer <- loadBMP "img/farmer.bmp"
                  dog <- loadBMP "img/dog.bmp"
                  lawn <- loadBMP "img/lawn.bmp"
                  screen <- loadBMP "img/screen.bmp"
                  return $ Pictures sunflower peashooter walnut citizen farmer dog lawn screen

getLevel :: FilePath -> IO Level
getLevel f = do c <- readFile f
                return $ parse parseLevel c

main :: IO ()
main = do pics <- loadPictures
          (d:_) <- getArgs
          files <- map (d </>) . filter (`notElem` ["..",".","/"]) <$> getDirectoryContents d
          levels <- mapM getLevel files
          G.playIO window G.black 60 (startWorld pics levels) worldToPic handleClick update
