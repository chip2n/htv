import Htv.Utils
import Htv.Gui

main = do
    episodes <- findEpisodes "/home/chip/Downloads/finished"
    let shows = createShows episodes
    mapM_ putStrLn (listShows shows)
    runGui

