
import qualified System.Environment as SE
import qualified System.Exit as Exit
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


type Track = String
type TrackList = [Track]

data State = State {
    trackList :: TrackList,
    currentTrackIndex :: Int
}

data Action
    = Quit
    | DoNothing
    | PlayTrack Track
    | Message String



interface :: State -> Char -> (State, Action)
interface state 'q' = (state, Quit)
interface state c = (state, PlayTrack (show c))
-- interface (State trackList currentTrackIndex) c = ((State trackList currentTrackIndex), PlayTrack (show c))


control :: State -> IO State
control state = do

    key <- SIO.getChar

    let (state, action) = interface state key in do
        case action of
            Quit -> do
                Exit.exitWith Exit.ExitSuccess

            PlayTrack t -> do
                SIO.putStrLn ("play track: " ++ t)

            _ -> return ()

        return state


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
    args <- SE.getArgs

    playlist <- case args of
        [playlistFilename] -> SIO.readFile playlistFilename
        _ -> usage >> Exit.exitWith Exit.ExitSuccess

    cookTerminal
    putStrLn "[debug]: ready"
    loop $ State (lines playlist) 0

    where
        loop :: State -> IO State
        loop state = control state >>= loop

        usage = putStrLn "Usage: playlist.player [playlist file]"
