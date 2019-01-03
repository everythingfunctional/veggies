import           Juicer             (makeDriver)
import           System.Environment (getArgs)

main :: IO ()
main = do
    (driverSourceFile : testFiles) <- getArgs
    makeDriver driverSourceFile testFiles
