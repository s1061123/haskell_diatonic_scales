{-
  chord: Showing Diatonic Chords Tool

 Description
  Chord is very simple haskell program which shows diatonic chords
 from given key.

 Requirements:
 The following program/library are required.
  - Haskell (hugs are tested but ghc also works)
  - Parsec module

 How to use:
  1) run chord.hs
  2) 'cc <key>' to select key
  3) 'dia' shows diatonic chords
  4) When finished, 'exit'.

 Please see following

 Example:
  $ hugs chord.hs
   (snip)
  Chord> main:
  C>cc Cm
  Cm>dia
  Cm7, Dm7(b5), D#M7, Fm7, Gm7, G#M7, A#7
  Cm>cc D#m
  D#m>dia
  D#m7, Fm7(b5), F#M7, G#m7, A#m7, BM7, C#7
  D#m>cc F#
  F#>dia
  F#M7, G#m7, A#m7, BM7, C#7, D#m7, Fm7(b5)
  F#>exit
 -}
module Chord
    where
import Prelude
import System.IO
import System.Exit
import Text.Printf
import Text.ParserCombinators.Parsec
import Control.Exception as Exp hiding (try, catch)
import Data.Char
import Data.Maybe (fromJust)
import Data.List
-- for hugs98
-- import Char(isSpace, isAlpha, toUpper)
-- import Maybe (fromJust)
-- import List


{- for escaping "A" for string -}
newtype KeyType = Key String
    deriving (Eq)
instance Show KeyType where
    show (Key x) = x
{-
instance Read KeyType where
    readsPrec _ [] = []
    readsPrec _ s = [(Key s, [])]
-}

{- Chord structure -}
data Chord x = Maj x
             | Min x
             | Seventh x
             | Maj7 x
             | Min7 x
             | HalfDim x
               deriving (Eq)

instance (Show x) => Show (Chord x) where
    show (Maj x) = show x
    show (Min x) = show x ++ "m"
    show (Seventh x) = show x ++  "7"
    show (Maj7 x) = show x ++ "M7"
    show (Min7 x) = show x ++ "m7"
    show (HalfDim x) = show x ++ "m7(b5)"

{-
instance (Read x) => Read (Chord x) where
    readsPrec _ [] = []
    readsPrec _ s = case (parse chordParse "" s) of
                      Left err -> []
                      Right xs -> [(xs, [])]
-}

getKey (Maj x) = x
getKey (Min x) = x
getKey (Seventh x) = x
getKey (Maj7 x) = x
getKey (Min7 x) = x
getKey (HalfDim x) = x

numToRoman :: Int -> String
numToRoman n =
    case n of
      1 -> "I"
      2 -> "II"
      3 -> "III"
      4 -> "IV"
      5 -> "V"
      6 -> "VI"
      7 -> "VII"
      8 -> "IIX"
      -1 -> "???"
      n  -> "(" ++ (printf "%d"n) ++ ")"

replaceKey k (Maj x) = (Maj k)
replaceKey k (Min x) = (Min k)
replaceKey k (Seventh x) = (Seventh k)
replaceKey k (Maj7 x) = (Maj7 k)
replaceKey k (Min7 x) = (Min7 k)
replaceKey k (HalfDim x) = (HalfDim k)

-- Relative one. 1 => 0
-- maj_diatonic = [ Maj7 1, Min7 2, Min7 3, Maj7 4, Seventh 5, Min7 6, HalfDim 7]
maj_diatonic = [ Maj7 0, Min7 2, Min7 4, Maj7 5, Seventh 7,
                     Min7 9, HalfDim 11]

-- In Minor 3 and 6 and 7 are flatted.
-- min_diatonic4 = [ Min7 1, HalfDim 2, Maj7 3, Min7 4, Min7 5, Maj7 6, Seventh 7]
min_diatonic4 = [ Min7 0, HalfDim 2, Maj7 3, Min7 5,
                       Min7 7, Maj7 8, Seventh 10]

replaceChord (x, Just n) = (x, Just (replaceKey (Key (numToRoman n)) x))
replaceChord (x, Nothing) = (x, Nothing)

keylist_str = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
               "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
keylist = map (\x -> Key x) keylist_str

getBegin (h:t) key = if key == h then (h:t)
                     else getBegin t key
getBegin [] key = []

-- get index of "key" from "keyl"
findNth keyl key =
    let listNth n (h:t) = if h == key then Just n
                          else listNth (n+1) t
        listNth _ [] = Nothing
    in listNth 1 keyl
{- or
findNth keyl key = (elemIndex keyl key) + 1
-}

-- Parser (ID function)
checkKey (h:t) k = (h == k) || checkKey t k
checkKey [] k = False

-- parse chordListParse "" "Am Dm7 D#M7"
chordListParse :: Parser [Chord KeyType]
chordListParse = do
  chords <- sepBy1 chordParse (skipMany1 (space <|> char ','))
  return chords

-- parse chordParse "" "Am"
chordParse :: Parser (Chord KeyType)
chordParse = do c <- oneOf ['A', 'B', 'C', 'D', 'E', 'F', 'G',
                            'a', 'b', 'c', 'd', 'e', 'f', 'g'];
                do { char '#';
                     c' <- chordParse' (Key [toUpper(c), '#']);
                     if (checkKey keylist (Key [toUpper(c), '#'])) then
                         return c'
                     else unexpected "Not valid key (sharp/flat)"}
                   <|> do { c' <- chordParse' (Key [toUpper(c)]);
                            return c'}
             <?> "unknown Keycode"
-- chordParse calls chordParse'
chordParse' :: KeyType -> Parser (Chord KeyType)
chordParse' x = do try (string  "m7")
                   return (Min7 x)
                <|> do string "M7"
                       return (Maj7 x)
                <|> do string "m"
                       return (Min x)
                <|> do string "7"
                       return (Seventh x)
                <|> return (Maj x)
                <?> "Chord adv"

-- getChordProgress (parseChordStr "C E7 F Gm7") "C"; <- missing

-- Parse Chord list, such as "C E7 F Gm7"
-- chordLParse "C E7 F Gm7"
chordLParse x = case (parse chordListParse ""  x) of
                  Left err -> throw (ErrorCall "Parse failed.")
                  Right xs -> Just xs


-- get scale list from given chord
getScale :: Chord KeyType -> [KeyType]
getScale (Maj k) = foldr (\x y -> (keys !! x):y) [] nth
    where nth = [0, 2, 4, 5, 7, 9, 11]
          keys = getBegin keylist k
getScale (Min k) = foldr (\x y -> (keys !! x):y) [] nth
    where nth = [0, 2, 3, 5, 7, 8, 10]
          keys = getBegin keylist k
getScale _ = throw (ErrorCall "Illegal scale")

-- get diatonic scale list from given chord
getDiatonic (Maj x) = map repkey maj_diatonic
    where keys = getBegin keylist x
          repkey = (\x -> replaceKey (keys !! (fromInteger . getKey) x) x)
getDiatonic (Min x) = map repkey min_diatonic4
    where keys = getBegin keylist x
          repkey = (\x -> replaceKey (keys !! (fromInteger . getKey) x) x)
getDiatonic _ = throw (ErrorCall "Illegal scale")

-- chordProgress "Cm" "C E7 F Gm7"
chordProgress x str = map f chords
    where chords = fromJust (chordLParse str)
          x' = case (parse chordParse "" x) of
                 Left err -> throw (ErrorCall "cannot parse")
                 Right x -> x
          l = getScale  x'
          f = replaceChord . (\x -> (x, findNth l (getKey x)))

-- 4 degree 
-- div 12
-- '5' is the index for perfect 4th
showFourDegree :: KeyType -> KeyType
showFourDegree x = (keys !! 5)
               where keys = getBegin keylist x
--showFourDegree:: Chord KeyType -> [KeyType]
--showFourDegree (Maj x) = 

-- getDegree chord nth (missing


data Command = Exit
             | ChangeChord (Chord KeyType)
             | GetScale
             | GetDiatonic
             | ShowHelp

instance Show Command where
    show Exit = "exit"
    show (ChangeChord s) = "changeChord "++(show s)
    show GetScale = "getScale"
    show GetDiatonic = "GetDiatonic"

printHelp = do
  putStrLn "cc \"chord\""
  putStrLn "scale"
  putStrLn "dia"
  putStrLn "exit"
  putStrLn "help"
  putStrLn "?"

commandParse :: Parser (Command)
commandParse = do try (string "exit")
                  return Exit
               <|> do try (string "?")
                      return ShowHelp
               <|> do try (string "help")
                      return ShowHelp
               <|> do try (string "cc")
                      skipMany1 space
                      k <- chordParse
                      return (ChangeChord k)
               <|> do try (string "scale")
                      return GetScale
               <|> do try (string "dia")
                      return GetDiatonic

mainLoop :: (Chord KeyType) -> IO ()
mainLoop key = do
  putStr ((show key)++">")
  line <- getLine
  case (parse commandParse "" line) of
    Right Exit -> exitWith ExitSuccess
    Right (ChangeChord x) -> mainLoop x
    Right GetScale -> do { let keylist = map (\x -> show x) (getScale key)
                           in (putStrLn . concat . (intersperse ", "))
                              keylist;
                           mainLoop key
                         }
    Right GetDiatonic -> do { let keylist = map (\x -> show x) (getDiatonic key)
                              in (putStrLn . concat . (intersperse ", "))
                                 keylist;
                              mainLoop key
                            }
    Right ShowHelp -> do { printHelp;
                           mainLoop key
                         }
    Left err -> do { putStr "Parse error: " ;
                     print err;
                     mainLoop key
                   }
    _ -> do putStrLn ("Unknown command: " ++ line)
            mainLoop key

main = do
  mainLoop (Maj (keylist !! 0))