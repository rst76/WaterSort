import Data.List (group, mapAccumR, sort, unfoldr)
import Data.Set (Set, empty, insert)
import Data.Tuple (swap)
import System.IO (hFlush, stdout)

data Color = Blank | Red | Lime | Yellow | Blue | Purple | Aqua | Gray | Green | Olive | Brown | Pink | Orange | Violet | Cream deriving (Enum)

instance Show Color where
  show Blank  = "  "
  show Red    = "\x1b[48;5;196mRd\x1b[0m"
  show Lime   = "\x1b[48;5;040mLm\x1b[0m"
  show Yellow = "\x1b[48;5;184mYl\x1b[0m"
  show Blue   = "\x1b[48;5;021mBl\x1b[0m"
  show Purple = "\x1b[48;5;054mPl\x1b[0m"
  show Aqua   = "\x1b[48;5;044mAq\x1b[0m"
  show Gray   = "\x1b[48;5;248mGy\x1b[0m"
  show Green  = "\x1b[48;5;022mGn\x1b[0m"
  show Olive  = "\x1b[48;5;058mOv\x1b[0m"
  show Brown  = "\x1b[48;5;130mBn\x1b[0m"
  show Pink   = "\x1b[48;5;201mPk\x1b[0m"
  show Orange = "\x1b[48;5;208mOg\x1b[0m"
  show Violet = "\x1b[48;5;097mVt\x1b[0m"
  show Cream  = "\x1b[48;5;222mCm\x1b[0m"

type Bottle = Int
type State = [Bottle]
type Id = Integer
type Path = [State]
type Path' = (Path, Set Path)
type Possibilities = ([Path], Set State)

replace :: Ord a => [a] -> [(Int, a)] -> [a]
replace list elements = concat $ a : as
  where
  (a, as) = mapAccumR replace list $ sort elements
  replace bs (i, b) = (bs0, b : bs1)
    where
    (bs0, _:bs1) = splitAt i bs

readBottle :: [Color] -> Bottle
readBottle = foldr (\c b -> b * 0x10 + fromEnum c) 0

split :: Bottle -> [Int]
split = take 4 . unfoldr (Just . swap . (`quotRem` 0x10))

showBottle :: Bottle -> String
showBottle bottle = concatMap (show . (toEnum :: Int -> Color)) $ reverse $ take 4 $ dropWhile (== 0) (reverse $ split bottle) ++ repeat 0

showState :: State -> String
showState = unwords . map showBottle

sealed :: Bottle -> Bool
sealed b = b0 == b1 && b1 == b2 && b2 == b3
  where
  [b0, b1, b2, b3] = split b

still :: Bottle -> Bool
still b = b == 0 || sealed b

pour :: (Bottle, Bottle) -> [(Bottle, Bottle)]
pour (a, b) | b < 0x1000 && not (still a) && (b == 0 || a0 == b0) = [(a', b')]
  where
  (a', a0) = a `quotRem` 0x10
  b0 = b `rem` 0x10
  b' = b * 0x10 + a0
pour _ = []

step :: State -> [State]
step state = [replace state [(from, fromBottle'), (to, toBottle')] |
  let l = length state - 1, from <- [0 .. l], to <- [0 .. l], from /= to,
  (fromBottle', toBottle') <- pour (state !! from, state !! to)]

search :: [Path] -> Set Id -> Int -> (Path, Set Id, Int)
search (path@(state : _) : paths) visited count
  | elem sorted visited = search paths visited (count + 1)
  | cleared state = (path, visited, count)
  | otherwise = search (paths ++ [state' : path | state' <- step state]) (insert sorted visited) (count + 1)
  where
  sorted = foldr (\c b -> b * 0x10000 + toInteger c) 0 $ sort state

cleared :: State -> Bool
cleared state = all still state

valid :: State-> Bool
valid state = all ((== 4) . length) $ group $ sort $ filter (> 0) $ concatMap split state

states :: [State]
states = init $ map (map readBottle) [
  [[Blue, Blue, Orange, Green], [Orange, Orange, Blue, Orange], [Green, Green, Green, Blue], [], []],
  [[Orange, Green, Blue, Pink], [Brown, Brown, Pink, Pink], [Green, Orange, Green, Pink], [Brown, Orange, Brown, Green], [Orange, Blue, Blue, Blue], [], []],
  [[Violet, Pink, Pink, Yellow], [Red, Red, Violet, Aqua], [Pink, Violet, Yellow, Aqua], [Yellow, Violet, Yellow, Pink], [Red, Red, Aqua, Aqua], [], []],
  [[Yellow, Gray, Red, Violet], [Purple, Blue, Gray, Violet], [Violet, Orange, Orange, Gray], [Lime, Aqua, Aqua, Lime], [Red, Olive, Aqua, Red], [Violet, Purple, Yellow, Red], [Yellow, Blue, Blue, Purple], [Olive, Lime, Gray, Blue], [Orange, Olive, Yellow, Orange], [Lime, Aqua, Purple, Olive], [], []],
  [[Blue, Green, Aqua, Aqua], [Purple, Pink, Olive, Gray], [Orange, Purple, Red, Brown], [Orange, Pink, Red, Orange], [Green, Red, Yellow, Blue], [Yellow, Green, Brown, Green], [Brown, Purple, Red, Lime], [Lime, Purple, Pink, Lime], [Olive, Gray, Aqua, Blue], [Brown, Yellow, Gray, Olive], [Gray, Yellow, Lime, Blue], [Olive, Aqua, Pink, Orange], [], []],
  [[Purple, Pink, Orange, Aqua], [Orange, Lime, Yellow, Aqua], [Red, Green, Green, Orange], [Green, Blue, Blue, Yellow], [Violet, Purple, Green, Lime], [Olive, Pink, Gray, Violet], [Olive, Purple, Yellow, Lime], [Purple, Gray, Gray, Olive], [Red, Pink, Aqua, Lime], [Olive, Violet, Violet, Pink], [Aqua, Blue, Red, Yellow], [Red, Gray, Orange, Blue], [], []],
  [[Red, Green, Pink, Yellow], [Gray, Yellow, Yellow, Olive], [Aqua, Olive, Aqua, Blue], [Lime, Purple, Green, Blue], [Aqua, Olive, Violet, Red], [Lime, Yellow, Purple, Pink], [Violet, Lime, Red, Olive], [Orange, Pink, Red, Orange], [Pink, Gray, Gray, Green], [Orange, Orange, Purple, Blue], [Gray, Blue, Lime, Violet], [Purple, Green, Violet, Aqua], [], []],
  [[Purple, Green, Lime, Violet], [Red, Violet, Aqua, Blue], [Blue, Red, Olive, Purple], [Green, Orange, Pink, Violet], [Green, Pink, Yellow, Red], [Gray, Purple, Lime, Orange], [Red, Blue, Aqua, Yellow], [Gray, Lime, Olive, Violet], [Gray, Lime, Purple, Yellow], [Pink, Aqua, Olive, Orange], [Orange, Olive, Pink, Yellow], [Aqua, Blue, Gray, Green], [], []],
  [[Yellow, Yellow, Green, Olive], [Blue, Red, Lime, Olive], [Aqua, Aqua, Green, Green], [Aqua, Brown, Blue, Lime], [Lime, Brown, Yellow, Olive], [Blue, Aqua, Orange, Gray], [Orange, Yellow, Green, Red], [Orange, Brown, Brown, Lime], [Gray, Orange, Blue, Olive], [Gray, Gray, Red, Red], [], []],
  [[Yellow, Brown, Aqua, Olive], [Orange, Blue, Red, Purple], [Pink, Yellow, Orange, Brown], [Aqua, Yellow, Orange, Olive], [Blue, Pink, Olive, Brown], [Olive, Blue, Blue, Purple], [Brown, Red, Green, Red], [Aqua, Green, Yellow, Orange], [Purple, Aqua, Pink, Green], [Pink, Red, Green, Purple], [], []],
  [[Orange, Purple, Blue, Lime], [Orange, Red, Gray, Lime], [Lime, Red, Green, Yellow], [Yellow, Pink, Yellow, Purple], [Green, Orange, Aqua, Gray], [Blue, Pink, Yellow, Pink], [Green, Aqua, Purple, Purple], [Red, Red, Aqua, Orange], [Lime, Blue, Blue, Aqua], [Green, Pink, Gray, Gray], [], []], -- 142
  [[Pink, Pink, Yellow, Yellow], [Cream, Aqua, Brown, Yellow], [Lime, Lime, Aqua, Red], [Brown, Brown, Orange, Blue], [Cream, Gray, Cream, Yellow], [Aqua, Blue, Purple, Blue], [Purple, Blue, Red, Purple], [Gray, Cream, Orange, Lime], [Aqua, Pink, Orange, Orange], [Lime, Gray, Purple, Red], [Gray, Brown, Red, Pink], [], []], -- 187
  [[Orange, Red, Yellow, Brown], [Purple, Lime, Lime, Cream], [Pink, Yellow, Blue, Gray], [Red, Red, Pink, Pink], [Blue, Aqua, Green, Orange], [Gray, Cream, Green, Blue], [Brown, Cream, Aqua, Gray], [Gray, Orange, Brown, Yellow], [Green, Pink, Purple, Cream], [Purple, Aqua, Green, Purple], [Yellow, Aqua, Blue, Brown], [Orange, Lime, Lime, Red], [], []], -- 198
  []]

main :: IO ()
main = do
  mapM_ putStrLn $ zipWith (\i state -> show i ++ ": " ++ showState state ++ "\n") [0 ..] states
  putStr "Enter the problem number: "
  hFlush stdout
  i <- readLn
  let state = states !! i
  if valid state
    then do
      let (path, visited, count) = search [[state]] empty 0
      mapM_ (putStrLn . showState) $ reverse path
      putStr "Steps: "
      print $ length path
      putStr "Visited states: "
      print $ length visited
      putStr "Total steps: "
      print count
    else print "Invalid state"
