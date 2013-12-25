import System.IO

import Data.Char
import Data.List
import Data.Either
import Data.Array as A
import Data.Array.IO as IOA
import Data.Word
import Data.Ord
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Error
import Control.Monad.Cont

type Pos = (Int, Int)

getX :: Pos -> Int
getX (x, _) = x

getY :: Pos -> Int
getY (_, y) = y

type ParseError a = Either String a

bail :: Int -> Int -> String -> ParseError a
bail row col str = Left $ "Error: " ++ str ++ " at (" ++
                          (show row) ++ ", " ++ (show col) ++ ")"

parseComment :: String -> ParseError [(Pos, Char)]
parseComment xs = helper xs 1 1
    where helper :: String -> Int -> Int -> 
                    ParseError [(Pos, Char)]
          helper [] r c = Right []
          helper (x:xs) r c = 
            case x of 
              '#' -> let ys = dropWhile (/='\n') xs
                         zs = dropWhile (== '\n') ys
                         r' = length ys - length zs
                     in helper zs (r + r') 1
              ' ' -> helper xs r (c + 1)
              '\n' -> helper xs (r + 1) c
              otherwise -> if x `elem` "><+-.,[]"
                           then do remain <- helper xs r (c + 1)
                                   let pos = (r, c)
                                   return ((pos, x):remain)
                           else bail r c $ "undefined symbol " ++ 
                                           [x]

{- Parse bracket pair in no comment content. This function 
   returns a list, which elems are tuple. Each tuple represents a
   matched pair in bf. Left elem in a tuple is left bracket, 
   right elem in that tuple is the corresponding right bracket.
-}
parseBracket :: [(Pos, Char)] -> ParseError [(Pos, Pos)]
parseBracket cs = helper cs [] []
    where helper :: [(Pos, Char)] -> [Pos] ->
                    [(Pos, Pos)] -> ParseError [(Pos, Pos)]
          helper [] s acc = 
            if null s then return acc
                      else let h = head s
                               r = getX h
                               c = getY h
                           in bail r c "unmatch left pair"
          helper (x:xs) s acc =
            case x of
              (pos, '[') -> helper xs (pos : s) acc
              (pos, ']') -> if not . null $ s
                            then helper xs (tail s) 
                                        ((head s, pos) : acc)
                            else bail (getX pos) (getY pos)
                                      "unmatch right pair"
              otherwise -> helper xs s acc

data UniMap a b = UniMap { kMap :: M.Map a b,
                           vMap :: M.Map b a }

unimap :: (Ord a, Ord b) => [(a, b)] -> UniMap a b
unimap xs = let ys = map (\(x, y) -> (y, x)) xs
            in UniMap { kMap = M.fromList xs,
                        vMap = M.fromList ys }

nextBracket :: Int -> UniMap Int Int -> Int
nextBracket pos umap = (kMap umap) M.! pos

preBracket :: Int -> UniMap Int Int -> Int
preBracket pos umap = (vMap umap) M.! pos

type IR = Int
type IRSize = Int
type AR = Int
type ARSize = Int
type Program = A.Array Int (Pos, Char)
type Memory = IOA.IOArray Int Word8
type JmpTable = UniMap Int Int
data RunEnv = RunEnv { getIR :: IR, getIRSize :: IRSize,
                       getAR :: AR, getARSize :: ARSize,
                       getPro :: Program, getMem :: Memory,
                       getJmp :: JmpTable }

program :: [(Pos, Char)] -> Program
program cnt = let ns = take (length cnt) [0..]
                  xs = zip ns cnt
              in array (0, length cnt - 1) xs

jmpTable :: [(Pos, Char)] -> [(Pos, Pos)] -> JmpTable
jmpTable cnt pair = unimap idxPair
    where cntMap = M.fromList cnt
          idxPair = (flip map) pair $ \(lpos, rpos) ->
                    ( M.findIndex lpos cntMap, 
                      M.findIndex rpos cntMap)


initEnv :: [(Pos, Char)] -> [(Pos, Pos)] -> IO RunEnv
initEnv cnt pair = do 
          mem <- IOA.newArray (0, msize - 1) 
                 (fromInteger 0) :: IO (IOA.IOArray Int Word8)
          return $ RunEnv ir irsize ar msize p mem j
    where ir = 0
          irsize = length cnt
          ar = 0
          p = program cnt
          j = jmpTable cnt pair
          msize = 8024

run :: RunEnv -> IO ()
run env@(RunEnv { getIR = ir, getIRSize = irsize,
                  getAR = ar, getARSize = arsize,
                  getPro = pro, getMem = mem,
                  getJmp = jmp } )=
    if ir >= irsize
    then do putStrLn ""
            putStrLn "program reach end, finish"
    else let (pos, c) = pro A.! ir
         in case c of
              '>' -> if ar + 1 >= arsize
                     then putStrLn $ "data pointer out of " ++
                                  "range (>= 8024) at " ++ 
                                  (show pos)
                     else run $ env { getIR = ir + 1,
                                      getAR = ar + 1 }
              '<' -> if ar - 1 < 0
                     then putStrLn $ "data pointer out of " ++
                                  " range (< 0) at " ++ 
                                  (show pos)
                     else run $ env { getIR = ir + 1,
                                      getAR = ar - 1 }
              '+' -> do a <- readArray mem ar
                        writeArray mem ar
                                   (a + (fromInteger 1) :: Word8)
                        run $ env { getIR = ir + 1 }
              '-' -> do a <- readArray mem ar
                        writeArray mem ar
                                   (a - (fromInteger 1) :: Word8)
                        run $ env { getIR = ir + 1 }
              '.' -> do a <- readArray mem ar
                        putChar $ (chr . fromIntegral) a
                        run $ env { getIR = ir + 1 }
              ',' -> do a <- getChar
                        writeArray mem ar (fromIntegral $ ord a)
                        run $ env { getIR = ir + 1 }
              '[' -> do a <- readArray mem ar
                        let nIR = if a == 0 
                                  then 1 + nextBracket ir jmp
                                  else ir + 1
                        run $ env { getIR = nIR }
              ']' -> do a <- readArray mem ar
                        let nIR = if a /= 0
                                  then 1 + preBracket ir jmp
                                  else ir + 1
                        run $ env { getIR = nIR }


main :: IO ()
main = do
  str <- readFile "./helloworld.bf"
  case parseComment str of 
       Left err -> putStrLn err
       Right x -> case parseBracket x of
                       Left err' -> putStrLn err'
                       Right y -> do env <- initEnv x y
                                     run env
