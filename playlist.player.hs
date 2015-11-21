
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as SE
import qualified System.Exit as Exit
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


type Track = BS.ByteString
type TrackList = [Track]

data State = State {
    trackList :: TrackList,
    currentTrackIndex :: Int
}

currentTrack :: State -> Track
currentTrack (State trackList currentTrackIndex) = trackList !! currentTrackIndex


data Action
    = Quit
    | DoNothing
    | PlayNewTrack
    | Message String


interface :: State -> Char -> (State, Action)
interface state@(State trackList currentTrackIndex) c = case c of

    'q' -> (state, Quit)
    '?' -> (state, Message . show . currentTrack $ state)
    'n' -> moveTrackIndex 1
    ' ' -> moveTrackIndex 1
    'r' -> moveTrackIndex 0
    'p' -> moveTrackIndex (-1)
    _ -> ((State trackList currentTrackIndex), DoNothing)

    where
        moveTrackIndex delta = moveTrackIndex' (currentTrackIndex + delta)
        moveTrackIndex' newIndex | newIndex >= length trackList  = ((State trackList 0), Quit)
        moveTrackIndex' newIndex | newIndex < 0                  = ((State trackList 0), PlayNewTrack)
        moveTrackIndex' newIndex | otherwise                     = ((State trackList newIndex), PlayNewTrack)


control :: State -> IO State
control state = do

    key <- SIO.getChar

    let (newState, action) = interface state key in do
        case action of
            Quit -> do
                Exit.exitWith Exit.ExitSuccess

            PlayNewTrack -> do
                SIO.putStrLn ("play track: " ++ show (currentTrack newState))

            Message message ->
                SIO.putStrLn message

            _ -> return ()

        return newState


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
        [playlistFilename] -> BS.readFile playlistFilename
        _ -> usage >> Exit.exitWith Exit.ExitSuccess

    cookTerminal
--     putStrLn "[debug]: ready"

    loop $ State (BS.lines playlist) 0

    where
        loop :: State -> IO State
        loop state = control state >>= loop

        usage = putStrLn "Usage: playlist.player [playlist file]"
