import Control.Parallel.Strategies
import Control.Parallel
import qualified  Data.ByteString.Lazy as B
import            Data.Clustering.Hierarchical
import            Data.List (find, find, groupBy, sort)
import            Data.List.Split
import            Data.Maybe (catMaybes)
import qualified  Data.Map as Map
import qualified  Data.Set as S
import            GHC.Float
import            System.Environment
import            System.IO
--import            System.Directory

data Movie = Movie { movieRating :: Int } deriving (Show)

data Interest = Interest { interestIdentity :: String
                          , name :: String
                          , url :: String
                          , category :: String
                          , ratings :: [Rating]
                          } deriving (Eq, Show)

instance Ord Interest where
  (Interest i _ _ _ _) `compare` (Interest j _ _ _ _) = i `compare` j

data Rating = Rating { userId :: String
                     , interestId :: String
                     , rating :: Int
                     , timestamp :: Int
                     } deriving (Eq, Show)

instance Ord Rating where
  (Rating _ i _ _) `compare` (Rating _ j _ _) = i `compare` j

data RatedInterest = RatedInterest { interestRating :: Double
                   , interest :: Interest
                   } deriving(Eq, Show)

instance Ord RatedInterest where
  (RatedInterest s1 i1) `compare` (RatedInterest s2 i2) = s1 `compare` s2

data Config = Config { maxDistance :: Int 
                     , interestFilter :: S.Set String
                     } deriving (Show)

-- Parse a line from the interests tsv
parseInterestLine :: String -> Interest
parseInterestLine line = case splitOn "\t" line of
  [a,b,c,d] -> Interest a b c d []

-- Get interests from the file
getInterests :: FilePath -> IO [Interest]
getInterests file = do
  content <- readFile file
  return $ map parseInterestLine $ lines content 

-- parse a line from the ratings file
parseRatingsLine :: String -> Rating
parseRatingsLine line = case splitOn "\t" line of
  [a, b, c, d] -> Rating a b (f c) (f d) 
  where
    f :: String -> Int
    f x = read x :: Int

-- Get Ratings from file
getRatings :: FilePath -> IO [Rating]
getRatings file = do
  content <- readFile file
  return $ map parseRatingsLine $ lines content

-- Assume the list of ratings all have the same interestId
createNewInterestFromRatings :: [Rating] -> Map.Map String Interest -> Maybe Interest
createNewInterestFromRatings ratings interestMap = fmap (interestCopy ratings) (Map.lookup (interestId $ head ratings) interestMap) 
  where
    interestCopy :: [Rating] -> Interest -> Interest
    interestCopy rs (Interest a b c d _) = Interest a b c d rs

-- Ratings with same interest
ratingsWithSameInterest :: Rating -> Rating -> Bool
ratingsWithSameInterest rating1 rating2 = (interestId rating1) == (interestId rating2)

-- Get Ratings Matrix
interestMatrix :: [Rating] -> [Interest] -> [Interest]
interestMatrix ratings interests = newInterests ratingGroups
  where
    ratingGroups :: [[Rating]]
    ratingGroups = groupBy ratingsWithSameInterest ratings
    interestTuple :: Interest -> (String, Interest)
    interestTuple interest = ( (interestIdentity interest), interest )
    interestsMap :: Map.Map String Interest
    interestsMap = Map.fromList $ map interestTuple interests 
    newInterests :: [[Rating]] -> [Interest]
    newInterests rss = catMaybes $ map (\rs -> createNewInterestFromRatings rs interestsMap) rss
    --newInterests rss = catMaybes $ (parMap rpar) (\rs -> createNewInterestFromRatings rs interestsMap) rss
    -- that map above should be a pmap

ratingMap :: Int -> Int
ratingMap i = case i of
  1 -> 10 -- one stars
  2 -> 20 -- two stars
  3 -> 30 -- four stars
  4 -> 40 -- five stars
  5 -> 50 -- five stars
  10 -> 100 -- Favorited
  20 -> 5 -- Saved for later
  30 -> 5 -- Dont' Know
  69 -> 1 -- Not interested
  100 -> 5 -- consumed
  _ -> error "wtf rating?"

-- Norm of a vector of ratings
ratingNorm :: [Rating] -> Double
ratingNorm ratings = sqrt $ fromIntegral $ foldl folder 0 ratings
  where
    folder :: Int -> Rating -> Int
    folder i r = i + (ratingMap $ rating r)*(ratingMap $ rating r)

ratingDotProduct :: [Rating] -> [Rating] -> Int
ratingDotProduct rs1 rs2 = sum $ map dot $ filter sameUser [ (x,y) | x <- rs1, y <- rs2 ]
  where
    sameUser :: (Rating, Rating) -> Bool
    sameUser (r1, r2) = (userId r1) == (userId r2)
    dot :: (Rating, Rating) -> Int
    dot (r1, r2) = (ratingMap $ rating r1) * (ratingMap $ rating r2)

-- Calculate distance between a list of ratings
ratingsListDistance :: Config -> [Rating] -> [Rating] -> Double
ratingsListDistance config rs1 rs2 = case dotProduct of
  0 -> fromIntegral $ maxDistance config
  d -> (  (ratingNorm rs1) * (ratingNorm rs2) ) / (fromIntegral  d)  
  where
    dotProduct = ratingDotProduct rs1 rs2

-- Calculate distance between two interests
interestDistance :: Config -> Interest -> Interest -> Distance
interestDistance config i1 i2 = case (ratings i1, ratings i2) of
  ([],[]) -> fromIntegral (maxDistance config)
  (rs,[]) -> fromIntegral (maxDistance config)
  ([],rs) -> fromIntegral (maxDistance config) 
  (rs1, rs2) -> ratingsListDistance config rs1 rs2

filterInterest :: Config -> Interest -> Bool
filterInterest config interest = S.member (category interest) (interestFilter config)

printInterestsToFile :: [Interest] -> IO ()
printInterestsToFile interests = do
  outputFile <- openFile "tmp-interests.txt" WriteMode
  hPutStr outputFile $ show $ map (\x -> (interestIdentity x)) interests
  hClose outputFile

-- Find and interest by name
findInterestByName :: String -> [Interest] -> Maybe Interest
findInterestByName n is = find (\i -> (name i) == n) is

findInterestByUrl :: String -> [Interest] -> Maybe Interest
findInterestByUrl n is = find (\i -> (url i) == n) is

-- calculate and sort by closest score
--findTopComparisons :: (Ord a, Ord b) => Int -> a -> (a -> a -> b) -> [a] -> [b]
--findTopComparisons limit comparitor scoreFunc as = take limit $ sort $ (parMap rpar) (scoreFunc comparitor) as

findTopComparisons :: Int -> Interest -> (Interest -> Interest -> Double) -> [Interest] -> [RatedInterest]
findTopComparisons limit interest scoreFunc interests = 
  take limit $ sort $ map (\x -> RatedInterest ((scoreFunc interest) x) x) interests

main :: IO ()
main = do
  args <- getArgs
  let movieName = args !! 0
  let topN = args !! 1
  --let movieList = [ Movie 1, Movie 2, Movie 0, Movie 10, Movie 11 ]
  --let dendo = dendrogram SingleLinkage movieList distance
  --print $ show dendo
  let config = Config 200 $ S.fromList ["TV Show", "Book Series", "Movie", "Movie Actor", "Musical Artist", "Book", "Movie Series"]

  interests <- fmap sort $ getInterests "/home/aaron/dev/movievisor-challenge/dump_interests-no_header.tsv"
  ratings <- fmap sort $ getRatings "/home/aaron/dev/movievisor-challenge/dump_ratings-no_header.tsv"


  let interestsWithRatings = interestMatrix ratings $ filter (filterInterest config) interests

  let curriedDistance = interestDistance config

  let movieInterest = findInterestByUrl movieName interestsWithRatings

  case movieInterest of
    Nothing -> print $ "Could not find movie: " ++ movieName
    Just mi -> mapM_ putStrLn $ map (\x -> show $ (interestRating x , name $ interest x)) $ findTopComparisons (read topN :: Int) mi curriedDistance interestsWithRatings

  --mapM_ putStrLn $ map (\x -> interestIdentity x) interestsWithRatings

  --let dendo = dendrogram CLINK (filter  (filterInterest config) interestsWithRatings) $ interestDistance config
  --print $ show dendo
