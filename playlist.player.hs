
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


type Track = String
type TrackList = [Track]

data Action
    = Quit
    | DoNothing
    | PlayTrack Track
    | Message String


{- How to keep state?

State required:
    track list
    current track index
-}


interface :: Char -> Action
interface 'q' = Quit
interface c = PlayTrack (show c)


control :: IO Bool
control = do
    key <- SIO.getChar
    case interface key of

        Quit -> return False

        PlayTrack t -> do
            SIO.putStrLn ("play track: " ++ t)
            return True

        _ -> return True


cookTerminal :: IO ()
cookTerminal = do
    SIO.hSetBuffering SIO.stdin SIO.NoBuffering

    attributes <- T.getTerminalAttributes 0
    T.setTerminalAttributes 0 (cook attributes) T.Immediately

    where
        cook attributes = (foldr applyRecipeStep attributes termRecipe)
        termRecipe = [ (T.withoutMode, T.EnableEcho) ]
        applyRecipeStep :: ((a -> b -> a), b) -> a -> a
        applyRecipeStep (fn, target) attr = fn attr target


main = do
    cookTerminal
    loop

    where
        loop :: IO ()
        loop = do
            continue <- control
            if continue then loop else return ()
