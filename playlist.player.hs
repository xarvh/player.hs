
import qualified System.IO as SIO
import qualified System.Posix.Terminal as T


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
            if key == 'q' then return ()
            else do
                SIO.putStrLn ("pressed: " ++ (show key))
                loop
