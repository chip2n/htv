module Htv.Gui (
    runGui
) where

import Htv.Utils
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.List
import Graphics.Vty.Widgets.Text
import Data.Map ((!))
import qualified Data.Text as T
import Control.Monad
import System.Process
import System.Directory
import Data.List

runGui :: IO ()
runGui = do
    episodes <- findEpisodes "/home/chip/Downloads/finished"
    let shows = createShows episodes
    c <- constructGui shows []
    runUi c defaultContext

constructGui :: TVShows -> TVShow -> IO Collection
constructGui shows displayedShow = do
    let showList = listShows shows
    let episodeList = map name displayedShow
    showListWidget <- newList (Attr Default Default Default) :: IO (Widget (List T.Text FormattedText))
    episodeListWidget <- newList (Attr Default Default Default) :: IO (Widget (List Episode FormattedText))

    forM_ showList (\s -> addToList showListWidget (T.pack s) =<< plainText (T.pack s) )

    box <- hBox showListWidget episodeListWidget
    ui <- centered box

    fg <- newFocusGroup
    addToFocusGroup fg showListWidget
    addToFocusGroup fg episodeListWidget

    c <- newCollection
    addToCollection c ui fg

    showListWidget `onItemActivated` (\event -> processSelected episodeListWidget . (!) shows . T.unpack =<< currentSelected showListWidget)

    episodeListWidget `onItemActivated` (\event -> playEpisode =<< currentSelectedEpisode episodeListWidget)

    return c


processSelected :: Widget (List Episode FormattedText) -> TVShow -> IO ()
processSelected listWidget episodes = do
    clearList listWidget
    forM_ episodes (\ep -> addToList listWidget ep =<< plainText (T.pack (formatEpisode ep)))
  where 

    

formatEpisode :: Episode -> String
formatEpisode (Episode name season number path) = name ++ " - " ++ "S" ++ show season ++ "E" ++ show number ++ " - " ++ path

currentSelected :: Widget (List T.Text b) -> IO T.Text
currentSelected list = do
    sel <- getSelected list
    return $ convert sel
  where convert Nothing = T.pack ""
        convert (Just (_, (a,_))) = a

currentSelectedEpisode :: Widget (List Episode b) -> IO Episode
currentSelectedEpisode list = do
    sel <- getSelected list
    return $ convert sel
  where convert Nothing = Episode "" 0 0 ""
        convert (Just (_, (a,_))) = a

playEpisode :: Episode -> IO ()
playEpisode ep = do
    file <- findVideoFile (path ep)
    --readProcess "vlc" [file] ""
    runCommand $ "vlc " ++ file ++ " &> /dev/null"
    return ()


findVideoFile :: String -> IO String
findVideoFile path = do
    files <- getDirectoryContents path
    let a = filter (\e -> ".rar" `isSuffixOf` e) files
    return (path ++ "/" ++ head a)
