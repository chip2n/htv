module Htv.Utils (
    TVShows,
    TVShow,
    Episode (..),
    findEpisodes,
    createShows,
    listShows,
    playEpisode
) where

import System.Directory
import Text.Regex.PCRE
import Data.Maybe
import Data.List
import System.Process
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

scanForEpisodes :: FilePath -> [FilePath] -> ([Episode], [FilePath])
scanForEpisodes root dirs = undefined

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

playEpisode :: Episode -> IO ()
playEpisode ep = do
    file <- findVideoFile (path ep)
    runCommand $ "vlc " ++ file ++ " &> /dev/null"
    return ()

findVideoFile :: String -> IO String
findVideoFile path = do
    files <- getDirectoryContents path
    let a = filter (\e -> ".rar" `isSuffixOf` e) files
    return (path ++ "/" ++ head a)


instance Show Episode where
    show (Episode name season number path) = name ++ " Season " ++ show season ++ " - " ++ "Episode " ++ show number

instance Eq Episode where
    e1 == e2 = name e1 == name e2 && season e1 == season e2 && episode e1 == episode e2

instance Ord Episode where
    compare e1 e2 = seasonCompare e1 e2 `thenCmp` episodeCompare e1 e2
      where seasonCompare e1 e2 = (season e1) `compare` (season e2)
            episodeCompare e1 e2 = (episode e1) `compare` (episode e2)
            thenCmp o1 o2 | o1 == EQ = o2
                          | otherwise = o1
