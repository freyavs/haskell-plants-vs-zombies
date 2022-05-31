module Pvz where
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Control.Monad

----------------------------------------------------
---- Data

type Cell = (X, Y) 
type X = Float --float want moet continu bewegen 
type Y = Int
type LastZombieSpawned = Bool
data Status = Won | Lost | Ongoing 
                deriving(Eq)

rows :: Int
rows = 6

columns :: Int
columns = 9

data World = World
        { flowers :: [Flower]
        , zombies :: [Zombie]
        , peas :: [Pea]    
        , status :: Status
        , time  :: Float 
        , saldo :: Int  --zal starten op getal >0 zodat er al geplant kan worden
        , selected :: FlowerType 
        , pics  :: Pictures  
        , level :: Level 
        , levels ::(Level, [Level])-- gekozen level en mogelijke levels
        }

data TestWorld = TestWorld 
        { flows :: [Flower]
        , zombs :: [Spawn]
        }  
        deriving (Show)  

data Pictures = Pictures
    { sunflower   :: G.Picture
    , peashooter  :: G.Picture
    , walnut      :: G.Picture
    , citizen     :: G.Picture
    , farmer      :: G.Picture
    , dog         :: G.Picture 
    , lawn        :: G.Picture 
    , screen      :: G.Picture
    }

data Pea = Pea 
        { pcell :: Cell
        , pspeed :: Float
        , hit :: Bool 
        }

data FlowerType = Sunflower | Peashooter | Walnut
                  deriving (Read, Show, Eq)
data Flower = Flower
        { fcell :: Cell
        , flives :: Int
        , cost :: Int
        , acttime :: Float --om de hoeveel tijd die een action doet
        , ftimer :: Float --als deze terug op 0 is gezet, kan actie worden uitgevoerd
        , ftype :: FlowerType
        }
        deriving (Show) 

data ZombieType = Citizen | Dog | Farmer | NoType
                  deriving (Read, Show, Eq) 
data Zombie = Zombie
        { zcell :: Cell
        , zlives :: Int
        , damage :: Int
        , hasBucket :: Bool
        , speed :: Float -- in seconden per cel
        , ztimer :: Float
        , ztype :: ZombieType
        }
        deriving (Show) 

----------------------------------------------
---- Parser Data

data Level = Level 
        { name :: String
        , difficulty :: Float
        , seeds :: [FlowerType]
        , phases :: [Phase]
        , duration :: Float
        } | Selecting [Level]
        deriving (Read, Show, Eq)


data PhaseType = ZombiePhase | BuildingPhase
                deriving (Read, Show, Eq)

data Phase = Phase
        { starttime :: Float
        , spawns :: [Spawn]
        , ptype :: PhaseType
        }
        deriving (Read, Show, Eq)

type RepeatPair = (Every, Times)
type Every = Float
type Times = Int

data Spawn = Spawn 
        { zombie :: ZombieType
        , bucket :: Bool
        , lane :: Int
        , spawntime :: Float -- uitgedrukt in seconden sinds begin van het spel
        , repeats :: [(Every, Times)]
        }
        deriving (Read, Show, Eq)

type Defence = (X,Y, FlowerType)

----------------------------------
------ data aanmaken

makeFlower :: FlowerType -> Cell -> Flower
makeFlower t c | t == Sunflower = Flower c 1 3 3 0 t
               | t == Peashooter = Flower c 1 6 2 0 t
               | otherwise = Flower c 5 6 0 0 t

makeZombie :: ZombieType -> Cell -> Bool -> Zombie
makeZombie t c b | t == Citizen = Zombie c (3+extra) 2 b 3 0 t
                 | t == Farmer = Zombie c (3+extra) 4 b 3 0 t
                 | otherwise = Zombie c (2+extra) 3 b 1 0 t
        where extra | b = 2 -- als een bucket heeft, dan 2 extra levens
                    | otherwise = 0

makePea :: Cell -> Pea
makePea c = Pea c 0.2 False
