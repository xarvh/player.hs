import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as SE
import qualified System.Exit as Exit
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


usage = "Usage: playlist.player [playlist file]"


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
    | PlayNewTrack Track
    | Message String


-- abusing closures?
interface :: State -> Char -> (State, Action)
interface state@(State trackList currentTrackIndex) c = case c of

    'q' -> (state, Quit)
    '?' -> (state, Message . show . currentTrack $ state)
    'n' -> moveTrackIndex 1
    ' ' -> moveTrackIndex 1
    'r' -> moveTrackIndex 0
    'p' -> moveTrackIndex (-1)
    _ -> (state, DoNothing)

    where
        moveTrackIndex delta = moveTrackIndex' (currentTrackIndex + delta)
        moveTrackIndex' newIndex | newIndex >= length trackList  = ((State trackList 0), Quit)
        moveTrackIndex' newIndex | newIndex < 0                  = moveTrackIndex'' 0
        moveTrackIndex' newIndex | otherwise                     = moveTrackIndex'' newIndex
        moveTrackIndex'' newIndex = let newState = State trackList newIndex in (newState, PlayNewTrack $ currentTrack newState)


doAction :: Action -> IO ()
doAction Quit = Exit.exitWith Exit.ExitSuccess
doAction (PlayNewTrack track) = SIO.putStrLn ("play track: " ++ show track)
doAction (Message message) = SIO.putStrLn message
doAction _ = return ()


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


loop :: (State, Action) -> IO ()
loop (state, action) = do
    doAction action
    key <- SIO.getChar
    loop $ interface state key


main = do
    args <- SE.getArgs

    playlist <- case args of
        [playlistFilename] -> BS.readFile playlistFilename
        _ -> putStrLn usage >> Exit.exitWith Exit.ExitSuccess

    cookTerminal

    let state0 = State (BS.lines playlist) 0
    loop $ interface state0 'r' -- not too happy about simulating 'r' keypress
