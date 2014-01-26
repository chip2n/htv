module Htv.Utils (
    TVShows,
    TVShow,
    Episode (..),
    findEpisodes,
    createShows,
    listShows
) where

import System.Directory
import Text.Regex.PCRE
import Data.Maybe
import Data.List
import qualified Data.Map as Map

data Episode = Episode { name :: Name
                       , season :: Int
                       , episode :: Int
                       , path :: FilePath
                       }

type Name = String
type TVShow = [Episode]
type TVShows = Map.Map Name TVShow

findEpisodes :: FilePath -> IO [Episode]
findEpisodes path = do
    a <- getDirectoryContents path
    let tvShows = map fromJust . filter isJust . map (parseTVShow path) $ a
    return tvShows

createShows :: [Episode] -> TVShows
createShows shows = foldl (\m s -> Map.insert (name s) (s:(current m s)) m) Map.empty shows
    where current m s = Map.findWithDefault [] (name s) m

parseTVShow :: FilePath -> String -> Maybe Episode
parseTVShow path s = maybeTV xs
    where (_,_,_,xs) = s =~ regexTV :: (String, String, String, [String])
          maybeTV [] = Nothing
          maybeTV xs = Just $ Episode (xs !! 0) (read (xs !! 1) :: Int) (read (xs !! 2) :: Int) (path ++ "/" ++ s)

listShows :: TVShows -> [Name]
listShows = Map.keys

regexTV :: String
regexTV = "([\\w\\.]+).S(\\d\\d)E(\\d\\d)"


instance Show Episode where
    show = name
