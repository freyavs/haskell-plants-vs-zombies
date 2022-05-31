module Parser 
( parse
, parseLevel
, parseDefence
)where

import Data.Char
import Control.Applicative
import Control.Monad
import Pvz

----------- parser implementation ----------------
newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
        where one [] = error "no parse"
              one [x] = x
              one xs | length xs > 1 = error "ambigious parse" 

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

instance Monad Parser where
        return t = Parser (\s -> [(t,s)])
        m >>= k = Parser (\s -> 
                        [ (y, u) |
                          (x, t) <- apply m s,
                          (y, u) <- apply (k x) t])

instance Functor Parser where
        fmap = liftM

instance Applicative Parser where
       pure = return
       (<*>) = ap

instance Alternative Parser where 
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
        mzero = Parser (const [])
        mplus m n = Parser (\s -> apply m s ++ apply n s)


-- parse character
char :: Parser Char
char = Parser f
        where f [] = []
              f (c:s) = [(c,s)]

-- parse character satisfying a predicate             
check :: (Char -> Bool) -> Parser Char
check p = do c <- char
             guard (p c)
             return c

------------ matching methods -----------------------

-- match a given character             
token :: Char -> Parser Char
token c  = check (== c)

-- match a string
match :: String -> Parser String
match = mapM token

-- match zero or more occurences
star :: Parser a -> Parser [a] 
star p = plus p <|> return []

-- match a sequence
plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

------------- parsers ----------------------------------   

parseDifficulty :: Parser Float              
parseDifficulty = do d <- parseBrackets' parseFloat
                     if d >= 0 && d <= 1 then return d else error "Parse error: difficulty not in interval [0,1]"
      
parseTitle :: Parser String
parseTitle = do s1 <- parseTitleChars
                r <- star parseRest
                return $ unwords (s1 : r)
        where parseRest = do token ' '
                             parseTitleChars

parseSeeds :: Parser [FlowerType]
parseSeeds = parseList parseSeed  

parseDuration :: Parser Float
parseDuration = do min <- parseTimePair
                   parseColon
                   sec <- parseTimePair
                   let secs = read sec
                   if secs <60 then 
                        return $ read min * 60 + secs
                   else error "Parse error: invalid duration."
                   -- controleren op mins <= 100 moet niet want zal sowieso een no parse error geven vanaf m 3 getallen heeft, 
                   -- ook >= 0 moet niet gecontroleerd worden want negatieve getallen worden hier ook niet geparsed
                   
parsePhases :: Parser [Phase]                  
parsePhases  =  plus parsePhase

parsePhase :: Parser Phase
parsePhase = do dur <- parseDuration
                parseWS 
                parseDash 
                parseWS
                t <- parsePhaseType
                case t of
                        ZombiePhase -> parseZombiePhase dur
                        _ -> parseBuildingPhase dur

parseBuildingPhase :: Float -> Parser Phase
parseBuildingPhase dur = do parseEndOfLine
                            return $ Phase dur [] BuildingPhase

parseZombiePhase :: Float -> Parser Phase
parseZombiePhase dur = do parseEndOfLine
                          sp <- parseSpawnExps (-2) dur [] 
                          parseEndOfLine
                          return $ Phase dur sp ZombiePhase
                        

parseSpawnExps :: Int -> Float -> [RepeatPair] -> Parser [Spawn]
parseSpawnExps l t r = do f <- parseSpawnExp l t r
                          rest <- star parseRest
                          return $ f ++ concat rest
        where parseRest = do parseEndOfLine
                             parseOWS
                             parseSpawnExp l t r

parseSpawnExp :: Int -> Float -> [RepeatPair] -> Parser [Spawn] --geef lane en time mee
parseSpawnExp l t r = parseSpawnOn t r <|> parseZombie l t r <|> parseSpawnAfter l t r <|> parseSpawnEvery l t r

parseSpawnOn :: Float -> [RepeatPair] -> Parser [Spawn] --geef geen lane door want moet met binnenste lane rekening houden
parseSpawnOn t r =  do match "on" 
                       parseWS
                       l <- parseLane
                       parseWS
                       exp <- parseBrackets $ plus (parseSpawnExps l t r)
                       return $ concat exp

parseSpawnAfter :: Int -> Float -> [RepeatPair] -> Parser [Spawn]
parseSpawnAfter l t r = do match "after"
                           parseWS
                           s <- parseFloat
                           parseWS
                           match "seconds"
                           parseWS
                           let nt = s + t
                           exp <- parseBrackets $ plus (parseSpawnExps l nt r)
                           return $ concat exp

parseSpawnEvery :: Int -> Float -> [RepeatPair] -> Parser [Spawn]
parseSpawnEvery l t r = do match "every" 
                           parseWS
                           s <- parseFloat
                           parseWS
                           match "seconds"
                           parseWS
                           match "for"
                           parseWS
                           x <- parseNat
                           parseWS
                           match "times"
                           parseWS
                           let nr = r ++ [(s,x)]
                           exp <- parseBrackets $ plus (parseSpawnExps l t nr)
                           return $ expandRepeats (concat exp)

parseZombie :: Int -> Float -> [RepeatPair] -> Parser [Spawn]
parseZombie l t r = do z <- parseBucket l t r <|> parseNoBucket l t r
                       case l of
                        (-2) -> error "Parse error: zombie doesn't have lane."
                        _ -> return [z]

----------- smol parsers ----------------------
               
parseTitleChars :: Parser String
parseTitleChars = plus parseTitleChar

parseOWS :: Parser String
parseOWS = star parseWSChar

parseWS :: Parser String
parseWS = plus parseWSChar

parseOptEOL :: Parser String
parseOptEOL = star $ token '\n' <|> token '\r'

parseEndOfLine :: Parser String
parseEndOfLine = plus $ token '\n' <|> token '\r'

parseSeed :: Parser FlowerType
parseSeed =  do s <- match "Sunflower" <|> match "Peashooter" <|> match "Walnut" 
                return $ read s

parseTimePair :: Parser String
parseTimePair = do x1 <- parseDigit
                   x2 <- parseDigit
                   return [x1, x2]

parsePhaseType :: Parser PhaseType
parsePhaseType = do s <- match "Building Phase" <|> match "Zombie Phase"
                    return $ makePhaseType s
                    
parseZombieType ::  Parser ZombieType
parseZombieType = do s <- match "Citizen" <|> match "Dog" <|> match "Farmer"
                     return $ read s

parseLane :: Parser Int
parseLane = do l <- parseOneLane <|> parseEveryLane
               if l `elem` [1..rows] || l == -1 then return l else error ("Parse error: lane number " ++ show l ++ " doesn't exist.")
         
parseOneLane :: Parser Int
parseOneLane = do match "lane"
                  parseWS
                  n <- parseDigit
                  return $ read [n]

parseEveryLane :: Parser Int
parseEveryLane = do match "every"
                    parseWS
                    match "lane"
                    return (-1)

-- parse zombie met bucket
parseBucket :: Int -> Float -> [RepeatPair] -> Parser Spawn
parseBucket l t r = do match "Bucket" 
                       parseWS
                       b <- parseZombieType
                       return $ Spawn b True l t r

-- parse zombie zonder bucket
parseNoBucket :: Int -> Float -> [RepeatPair] -> Parser Spawn
parseNoBucket l t r = do b <- parseZombieType
                         return $ Spawn b False l t r

parseFloat :: Parser Float
parseFloat = do x <- plus $ parseDigit <|> token '.'
                return $ read x

parseNat :: Parser Int
parseNat = do d <- plus parseDigit
              return $ read d

parseDefenceLine :: Parser Defence
parseDefenceLine = do x <- parseFloat
                      parseComma
                      parseOWS
                      y <- parseNat
                      parseComma
                      parseOWS
                      f <- parseFlowerType     
                      return (x,y,f) 

parseFlowerType ::  Parser FlowerType
parseFlowerType = do s <- match "Sunflower" <|> match "Walnut" <|> match "Peashooter"
                     return $ read s


---------------- char parsers -------------------------

parseTitleChar :: Parser Char
parseTitleChar = parseTitleSymbol <|> parseDigit <|> parseLetter 

parseWSChar :: Parser Char
parseWSChar = token '\t' <|> token ' ' 

parseTitleSymbol :: Parser Char
parseTitleSymbol = token '!' <|> token '&' <|> token '?'

parseDigit :: Parser Char
parseDigit = check isDigit

parseLetter :: Parser Char
parseLetter = check isAlpha

parseComma :: Parser Char
parseComma = token ','

parseColon :: Parser Char
parseColon = token ':'

parseDash :: Parser Char
parseDash = token '-' 

------------------- parser helpers ------------------------

parseList :: Parser a -> Parser [a]
parseList parser = do s1 <- parser
                      elements <- star parseRest
                      return (s1:elements)
        where parseRest = do parseComma 
                             parseWS
                             parser

parseBrackets :: Parser a -> Parser a
parseBrackets parser = do token '{'
                          parseOptEOL
                          parseOWS
                          s <- parser
                          parseOptEOL 
                          parseOWS
                          token '}'
                          return s      
                          
parseBrackets' :: Parser a -> Parser a             
parseBrackets' parser = do token '('
                           s <- parser
                           token ')'
                           return s

makePhaseType :: String -> PhaseType
makePhaseType "Zombie Phase" = ZombiePhase
makePhaseType _ = BuildingPhase

repeatsToList :: RepeatPair -> [Float]
repeatsToList (e, t) = [ e * fromIntegral x | x <- [0..t-1] ]

expandRepeats :: [Spawn] -> [Spawn]
expandRepeats = concatMap expandRepeat

expandRepeat :: Spawn -> [Spawn]
expandRepeat s@(Spawn _ _ _ _ []) = [s]
expandRepeat (Spawn z b l s (r:xs))  = [ Spawn z b l (s+x) xs | x <- repeatsToList r] 

-- zal ook de lanes die op -1 staan vervangen door alle lanes
filterDurations :: [Phase] -> Float -> [Phase]
filterDurations [p] gamedur = let o = spawns p in [p{spawns = filterSpawns o gamedur}] -- als het de laatste is filter met de tijd van het spel
filterDurations (p:ps) t = let o = spawns p
                               maxtime = starttime $ head ps
                           in p{spawns = filterSpawns o maxtime} : filterDurations ps t 

filterSpawns :: [Spawn] -> Float -> [Spawn]
filterSpawns s maxtime = concatMap expandAllLanes (filter (\x -> spawntime x <= maxtime ) s)

expandAllLanes :: Spawn -> [Spawn]
expandAllLanes s@(Spawn z b l r sp) | l == -1 = [ Spawn z b x r sp | x <- [1..6] ]
                                    | otherwise = [s]

--------------- main parsers -------------------------
parseLevel :: Parser Level
parseLevel = do title <- parseTitle
                parseOWS
                diff <- parseDifficulty
                parseEndOfLine
                seeds <- parseSeeds
                parseEndOfLine
                p <- parsePhases 
                dur <- parseDuration
                parseEndOfLine
                return $ Level title diff seeds (filterDurations p dur) dur

parseDefence :: Parser [Defence]
parseDefence = do parseOptEOL
                  f <- star $ parseBrackets' parseDefenceLine
                  rest <- star parseRest
                  return $ f ++ rest
        where parseRest = do parseEndOfLine
                             parseBrackets' parseDefenceLine

