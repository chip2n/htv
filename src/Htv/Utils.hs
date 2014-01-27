module Htv.Utils (
    TVShows,
    TVShow,
    Episode (..),
    findEpisodes,
    findAllEpisodes,
    createShows,
    listShows,
    playEpisode
) where

import System.Directory
import Text.Regex.PCRE
import Data.Maybe
import Data.List
import System.Process
import Control.Monad
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

findAllEpisodes :: FilePath -> IO [Episode]
findAllEpisodes path = do
    a <- getDirectoryContents path
    let (episodes, failedPaths) = scanForEpisodes path a
    b <- filterM (doesDirectoryExist) . map (joinPath path) . delete "." . delete ".." $ failedPaths
    all <- mapM findAllEpisodes b
    return (episodes ++ concat all)

scanForEpisodes :: FilePath -> [FilePath] -> ([Episode], [FilePath])
scanForEpisodes root dirs = foldl magic ([],[]) dirs
    where magic (eps, paths) path | parsed path == Nothing = (eps, (path:paths))
                                  | otherwise = (((fromJust (parsed path)):eps), paths)
          parsed = parseTVShow root

createShows :: [Episode] -> TVShows
createShows shows = foldl (\m s -> Map.insert (name s) (s:(current m s)) m) Map.empty shows
    where current m s = Map.findWithDefault [] (name s) m

parseTVShow :: FilePath -> String -> Maybe Episode
parseTVShow path s = maybeTV xs
    where (_,_,_,xs) = s =~ regexTV :: (String, String, String, [String])
          maybeTV [] = Nothing
          maybeTV xs = Just $ Episode name season episode newPath
          name = xs !! 0
          season = read $ xs !! 1 :: Int
          episode = read $ xs !! 2 :: Int
          newPath = path ++ "/" ++ s

listShows :: TVShows -> [Name]
listShows = Map.keys

regexTV :: String
regexTV = "([\\w\\.]+).S(\\d\\d)E(\\d\\d)"

playEpisode :: Episode -> IO ()
playEpisode ep = do
    file <- findVideoFile (path ep)
    runCommand $ "vlc " ++ file ++ " &> /dev/null"
    return ()

findVideoFile :: FilePath -> IO String
findVideoFile path = do
    files <- getDirectoryContents path
    let a = filter (\e -> ".rar" `isSuffixOf` e) files
    return (path ++ "/" ++ head a)

joinPath :: FilePath -> FilePath -> FilePath
joinPath fp d = fp ++ "/" ++ d



instance Show Episode where
    show (Episode name season number path) =
        name ++ " Season " ++ show season ++ " - " ++ "Episode " ++ show number

instance Eq Episode where
    e1 == e2 = name e1 == name e2 &&
               season e1 == season e2 &&
               episode e1 == episode e2

instance Ord Episode where
    compare e1 e2 = seasonCompare e1 e2 `thenCmp` episodeCompare e1 e2
      where seasonCompare e1 e2 = (season e1) `compare` (season e2)
            episodeCompare e1 e2 = (episode e1) `compare` (episode e2)
            thenCmp o1 o2 | o1 == EQ = o2
                          | otherwise = o1
