import qualified Data.ByteString.Lazy as B
import Data.Clustering.Hierarchical
import Data.List (groupBy)
import Data.List.Split
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import System.Environment

--main :: IO ()
--main = getArgs >>= print . haqify . head
--haqify :: String -> String
--haqify s = "Haq! " ++ s

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

distance :: Movie -> Movie -> Distance
distance movie1 movie2 = sqrt( fromIntegral $ ( (movieRating movie1) - (movieRating movie2) ) )

toTuple4 :: [String] -> (String, String, String, String)
toTuple4 [a,b,c,d] = (a,b,c,d)

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

-- Get Ratings Matrix
interestMatrix :: [Rating] -> [Interest] -> [Interest]
interestMatrix ratings interests = newInterests ratingGroups
  where
    ratingsWithSameInterest :: Rating -> Rating -> Bool
    ratingsWithSameInterest rating1 rating2 = (interestId rating1) == (interestId rating2)
    ratingGroups :: [[Rating]]
    ratingGroups = groupBy ratingsWithSameInterest ratings
    interestTuple :: Interest -> (String, Interest)
    interestTuple interest = ( (interestIdentity interest), interest )
    interestsMap :: Map.Map String Interest
    interestsMap = Map.fromList $ map interestTuple interests 
    newInterests :: [[Rating]] -> [Interest]
    newInterests rss = catMaybes $ map (\rs -> createNewInterestFromRatings rs interestsMap) ratingGroups

main :: IO ()
main = do
  let movieList = [ Movie 1, Movie 2, Movie 0, Movie 10, Movie 11 ]
  let dendo = dendrogram SingleLinkage movieList distance
  print $ show dendo
  interests <- getInterests "/home/aaron/dev/movievisor-challenge/dump_interests-no_header.tsv"
  ratings <- getRatings "/home/aaron/dev/movievisor-challenge/dump_ratings-no_header.tsv"
  let newInterests = interestMatrix ratings interests
  print $ show newInterests
  print "done"
  --print $ interests
  --print $ ratings
