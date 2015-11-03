
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


type Track = String
type TrackList = [Track]





data Action
    = PlayTrack Track
    | PrintString String
    | Quit




interface :: Char -> Action
interface 'q' = Quit
interface c = PlayTrack (show c)










termRecipe = [ (T.withoutMode, T.EnableEcho) ]


cookOne :: ((a -> b -> a), b) -> a -> a
cookOne (fn, target) attr = fn attr target

cook attributes = foldr cookOne attributes termRecipe

main = do
    SIO.hSetBuffering SIO.stdin SIO.NoBuffering

    attributes <- T.getTerminalAttributes 0
    T.setTerminalAttributes 0 (cook attributes) T.Immediately
    loop

    where
        loop :: IO ()
        loop = do
            key <- SIO.getChar

            case interface key of
                PlayTrack t -> do
                    SIO.putStrLn ("pressed: " ++ t)
                    loop
                Quit -> return ()
                _ -> loop
