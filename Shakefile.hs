import           Control.Applicative          ((<|>))
import           Control.Exception            (catch, throwIO)
import           Control.Monad                (filterM, foldM, liftM)
import           Data.Char                    (isAsciiLower, isDigit, toLower)
import           Data.List                    (sort)
import           Data.List.Split              (splitOn)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.String.Utils            (endswith)
import           Development.Shake            (Action,
                                               Change (ChangeModtimeAndDigest),
                                               CmdOption (Cwd), Exit (Exit),
                                               FilePattern, Rules,
                                               Stderr (Stderr), action,
                                               actionOnException, alwaysRerun,
                                               cmd, command, command_,
                                               getDirectoryFilesIO,
                                               getShakeOptions, liftIO, need,
                                               phony, progressSimple,
                                               removeFilesAfter, shakeArgs,
                                               shakeChange, shakeFiles,
                                               shakeOptions, shakeProgress,
                                               shakeThreads, want, (%>), (&%>),
                                               (<//>))
import           Development.Shake.FilePath   (dropExtension, exe,
                                               takeDirectory, takeExtension,
                                               takeFileName, (-<.>), (<.>),
                                               (</>))
import           System.Directory             (doesFileExist, removeFile)
import           System.Exit                  (ExitCode (ExitFailure, ExitSuccess))
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (readProcessWithExitCode)
import           Text.ParserCombinators.ReadP (ReadP, char, eof, many, many1,
                                               option, pfail, readP_to_S,
                                               satisfy, skipSpaces, string)


-- All the extra type declarations we need
type ModuleName = String
type ProgramName = String
type ModuleNames = Set.Set ModuleName
type SpecialCommands = Map.Map FilePath String

data SourceFile =
    FortranSourceFile {
          getFileName           :: FilePath
        , getFortranModulesUsed :: ModuleNames
        , getCModulesUsed       :: ModuleNames
        , getModulesContained   :: ModuleNames
    }
    | CSourceFile {
          getFileName           :: FilePath
        , getFortranModulesUsed :: ModuleNames
    } deriving (Eq, Ord, Show)

data Module = Module {
      getModuleName   :: ModuleName
    , getModuleSource :: SourceFile
} deriving (Eq, Ord, Show)

data Program = Program {
      getProgramName   :: ProgramName
    , getProgramSource :: SourceFile
} deriving (Eq, Ord, Show)

type SourceFiles = Set.Set SourceFile
type Modules = Map.Map ModuleName Module
type Programs = Set.Set Program

data LineContents =
      FortranModuleUsed ModuleName
    | CModuleUsed ModuleName
    | ModuleDefined ModuleName
    | ProgramDefined ProgramName
    | Other deriving (Show)

data ModuleSearchTree =
    Node {
      getNodeDirectory  :: FilePath
    , getNodeModules    :: Modules
    , getRemainingNodes :: Set.Set ModuleSearchTree
} deriving (Eq, Ord, Show)

-- A handful of constants that are just basic configuration

-- compiler and flags
compiler = "g++"
baseCIncludeFlags = []
fortranExts = [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
cExts = [".c", ".C", ".cpp", ".cxx", ".c++"]
sourceExts = fortranExts ++ cExts
headerExts = [".h", ".H", ".hpp", ".hxx", ".h++"]
sourceDirs = ["src"]
flags = ["-Wall",  "-Wextra", "-Werror", "-pedantic"]
develFlags = ["-g"] ++ flags
fFlags = []
cFlags' cIncludeFlags = ["-std=c++11"] ++ cIncludeFlags
fDevelFlags = [] ++ fFlags ++ develFlags
cDevelFlags' cFlags = [] ++ cFlags ++ develFlags
develLinkFlags = develFlags ++ ["-lgfortran"]

-- directories
buildDir = "build"
testDir = "tests"
testBuildDir = "test_build"

-- The main routine that executes the build system
main :: IO ()
main = do
    otherCIncludeDirs <- getCIncludeDirs sourceDirs headerExts
    let cIncludeFlags = baseCIncludeFlags ++ (map (\dir -> "-I" ++ dir) otherCIncludeDirs)
    let cFlags = cFlags' cIncludeFlags
    let cDevelFlags = cDevelFlags' cFlags

    sourceFiles <- getDirectoriesFiles sourceDirs sourceExts
    (sources, modules, programs) <- scanSourceFiles Map.empty sourceFiles
    let sourceSearchTree = Node buildDir modules Set.empty

    testSourceFiles <- getDirectoriesFiles [testDir] sourceExts
    (testSources, testModules, testPrograms) <-
        scanSourceFiles modules testSourceFiles
    let testSearchTree = Node testBuildDir testModules (Set.singleton sourceSearchTree)

    shakeArgs shakeOptions{shakeFiles="_shake", shakeChange=ChangeModtimeAndDigest, shakeThreads=0, shakeProgress=progressSimple} $ do
        mapM_ (makeSourceRule fDevelFlags cDevelFlags cIncludeFlags sourceSearchTree) sources
        mapM_ (makeExecutableRule develLinkFlags cIncludeFlags sourceSearchTree) programs

        want ["tests"]
        phony "tests" $ do
            let progs = [testBuildDir </> getProgramName prog <.> exe | prog <- Set.toList testPrograms]
            need progs
            mapM_ (cmd :: FilePath -> Action ()) progs

        mapM_ (makeSourceRule fDevelFlags cDevelFlags cIncludeFlags testSearchTree) testSources
        mapM_ (makeExecutableRule develLinkFlags cIncludeFlags testSearchTree) testPrograms

        phony "clean" $ do
            removeFilesAfter buildDir ["//"]
            removeFilesAfter testBuildDir ["//"]

getCIncludeDirs :: [FilePath] -> [FilePattern] -> IO [FilePath]
getCIncludeDirs dirs exts = do
    headerFiles <- getDirectoriesFiles dirs exts
    return $ unique (map takeDirectory headerFiles)
    where unique = Set.toList . Set.fromList

-- A little wrapper around getDirectoryFiles so we can get files from multiple directories
getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts =
    getDirectoryFilesIO "" newPatterns
    where
        newPatterns = concatMap appendExts dirs
        appendExts dir = map ((dir <//> "*") ++) exts

-- The routines we need to parse all the source files
scanSourceFiles :: Modules -> [FilePath] -> IO (SourceFiles, Modules, Programs)
scanSourceFiles previousModules = foldl addNextContents (return (Set.empty, Map.empty, Set.empty))
    where
        addNextContents previousContents file = do
            (sources, modules, programs) <- previousContents
            newContents <- if takeExtension file `elem` fortranExts then
                scanFortranSourceFile (previousModules `Map.union` modules) file
            else
                scanCSourceFile (previousModules `Map.union` modules) file
            case newContents of
                Left (source, program) ->
                    return (source `Set.insert` sources, modules, program `Set.insert` programs)
                Right (source, newModules) ->
                    return (source `Set.insert` sources, newModules `Map.union` modules, programs)

scanFortranSourceFile :: Modules -> FilePath -> IO (Either (SourceFile, Program) (SourceFile, Modules))
scanFortranSourceFile previousModules file = do
    fileLines <- readFileLinesIO file
    let eitherContents = foldl addLineContents (Right (Set.empty, Set.empty, Set.empty, Nothing)) fileLines
    case eitherContents of
        Right contents ->
            case contents of
                (modulesContained, fortranModulesUsed, cModulesUsed, Nothing) ->
                    return $ Right (buildOutputWithModules file fortranModulesUsed cModulesUsed modulesContained)
                (modulesContained, fortranModulesUsed, cModulesUsed, Just programName) ->
                    return $ Left (buildOutputWithProgram file fortranModulesUsed cModulesUsed modulesContained programName)
        Left err ->
            fail $ "*** Error in file " ++ file ++ ": " ++ err
    where
        addLineContents contents line =
            case contents of
                Right (previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram) ->
                    case parseFortranLine line of
                        FortranModuleUsed moduleName ->
                            Right (previousModulesContained, moduleName `Set.insert` previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                        CModuleUsed moduleName ->
                            Right (previousModulesContained, previousFortranModulesUsed, moduleName `Set.insert` previousCModulesUsed, maybeProgram)
                        ModuleDefined moduleName ->
                            case Map.lookup moduleName previousModules of
                                Just module' ->
                                    Left $ "module " ++ moduleName ++ " was already defined in " ++ getFileName (getModuleSource module')
                                Nothing ->
                                    if moduleName `Set.member` previousModulesContained then
                                        Left $ "module " ++ moduleName ++ " defined twice"
                                    else
                                        Right (moduleName `Set.insert` previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                        ProgramDefined programName ->
                            case maybeProgram of
                                Just programName' ->
                                    Left $ "multiple programs defined: " ++ programName' ++ " and " ++ programName
                                Nothing ->
                                    Right (previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, Just programName)
                        Other ->
                            Right (previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                Left err ->
                    Left err

scanCSourceFile :: Modules -> FilePath -> IO (Either (SourceFile, Program) (SourceFile, Modules))
scanCSourceFile previousModules file = do
    fileLines <- readFileLinesIO file
    let eitherContents = foldl addLineContents (Right (Set.empty, Set.empty, Set.empty, Nothing)) fileLines
    case eitherContents of
        Right contents ->
            case contents of
                (modulesContained, fortranModulesUsed, cModulesUsed, Nothing) -> do
                    return $ Right (buildOutput file fortranModulesUsed modulesContained)
                (modulesContained, fortranModulesUsed, cModulesUsed, Just programName) ->
                    undefined
        Left err ->
            fail $ "*** Error in file " ++ file ++ ": " ++ err
    where
        addLineContents contents line =
            case contents of
                Right (previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram) ->
                    case parseCLine line of
                        FortranModuleUsed moduleName ->
                            Right (previousModulesContained, moduleName `Set.insert` previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                        CModuleUsed moduleName ->
                            undefined
                        ModuleDefined moduleName ->
                            case Map.lookup moduleName previousModules of
                                Just module' ->
                                    Left $ "module " ++ moduleName ++ " was already defined in " ++ getFileName (getModuleSource module')
                                Nothing ->
                                    if moduleName `Set.member` previousModulesContained then
                                        Left $ "module " ++ moduleName ++ " defined twice"
                                    else
                                        Right (moduleName `Set.insert` previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                        ProgramDefined programName ->
                            undefined
                        Other ->
                            Right (previousModulesContained, previousFortranModulesUsed, previousCModulesUsed, maybeProgram)
                Left err ->
                    Left err

readFileLinesIO :: FilePath -> IO [String]
readFileLinesIO file = do
    contents <- readFile file
    return $ lines contents

buildOutputWithModules :: FilePath -> ModuleNames -> ModuleNames -> ModuleNames -> (SourceFile, Modules)
buildOutputWithModules file fortranModulesUsed cModulesUsed modulesContained =
    let source = FortranSourceFile file (fortranModulesUsed `Set.difference` modulesContained) cModulesUsed modulesContained
        modules = foldl (addModuleWithSource source) Map.empty modulesContained in
    (source, modules)
    where
        addModuleWithSource source previousModules moduleName =
            Map.insert moduleName (Module moduleName source) previousModules

buildOutputWithProgram :: FilePath -> ModuleNames -> ModuleNames -> ModuleNames -> ProgramName -> (SourceFile, Program)
buildOutputWithProgram file fortranModulesUsed cModulesUsed modulesContained programName =
    let source = FortranSourceFile file (fortranModulesUsed `Set.difference` modulesContained) cModulesUsed modulesContained
        program = Program programName source in
    (source, program)

buildOutput :: FilePath -> ModuleNames -> ModuleNames -> (SourceFile, Modules)
buildOutput file fortranModulesUsed modulesContained =
    let source = CSourceFile file fortranModulesUsed
        modules = foldl (addModuleWithSource source) Map.empty modulesContained in
    (source, modules)
    where
        addModuleWithSource source previousModules moduleName =
            Map.insert moduleName (Module moduleName source) previousModules

parseFortranLine :: String -> LineContents
parseFortranLine line =
    let line' = map toLower line
        result = readP_to_S doFortranLineParse line' in
    getResult result
    where
        getResult (_:(contents, _):_) = contents
        getResult [(contents, _)]     = contents
        getResult []                  = Other

parseCLine :: String -> LineContents
parseCLine line =
    let line' = map toLower line
        result = readP_to_S doCLineParse line' in
    getResult result
    where
        getResult (_:(contents, _):_) = contents
        getResult [(contents, _)]     = contents
        getResult []                  = Other

doFortranLineParse :: ReadP LineContents
doFortranLineParse = option Other fortranUsefulContents

doCLineParse :: ReadP LineContents
doCLineParse = option Other cUsefulContents

fortranUsefulContents :: ReadP LineContents
fortranUsefulContents = moduleDeclaration <|> programDeclaration <|> useStatement <|> cUseStatement

cUsefulContents :: ReadP LineContents
cUsefulContents = moduleDeclaration <|> useStatement

moduleDeclaration :: ReadP LineContents
moduleDeclaration = do
    skipSpaces
    _ <- string "module"
    skipAtLeastOneWhiteSpace
    modName <- validIdentifier
    skipSpaceOrEnd
    if modName == "procedure" then
        pfail
    else
        return $ ModuleDefined modName

programDeclaration :: ReadP LineContents
programDeclaration = do
    skipSpaces
    _ <- string "program"
    skipAtLeastOneWhiteSpace
    progName <- validIdentifier
    skipSpaceOrEnd
    return $ ProgramDefined progName

useStatement :: ReadP LineContents
useStatement = do
    skipSpaces
    _ <- string "use"
    skipAtLeastOneWhiteSpace
    modName <- validIdentifier
    skipSpaceCommaOrEnd
    return $ FortranModuleUsed modName

cUseStatement :: ReadP LineContents
cUseStatement = do
    comment
    _ <- string "use"
    skipAtLeastOneWhiteSpace
    modName <- validIdentifier
    skipSpaceCommaOrEnd
    return $ CModuleUsed modName

skipAtLeastOneWhiteSpace :: ReadP ()
skipAtLeastOneWhiteSpace = do
    _ <- many1 whiteSpace
    return ()

skipSpaceOrEnd :: ReadP ()
skipSpaceOrEnd = eof <|> skipAtLeastOneWhiteSpace

skipSpaceCommaOrEnd :: ReadP ()
skipSpaceCommaOrEnd = eof <|> skipComma <|> skipAtLeastOneWhiteSpace

skipComma :: ReadP ()
skipComma = do
    _ <- char ','
    return ()

whiteSpace :: ReadP Char
whiteSpace = satisfy (`elem` " \t")

validIdentifier :: ReadP String
validIdentifier = do
    first <- validFirstCharacter
    rest <- many validIdentifierCharacter
    return $ first : rest

validFirstCharacter :: ReadP Char
validFirstCharacter = alphabet

validIdentifierCharacter :: ReadP Char
validIdentifierCharacter = alphabet <|> digit <|> underscore

alphabet :: ReadP Char
alphabet = satisfy isAsciiLower

digit :: ReadP Char
digit = satisfy isDigit

underscore :: ReadP Char
underscore = char '_'

comment :: ReadP ()
comment = firstColumnComment <|> freeFormComment

firstColumnComment :: ReadP ()
firstColumnComment = do
    _ <- char 'c' <|> char 'C'
    skipSpaces
    return ()

freeFormComment :: ReadP ()
freeFormComment = do
    skipSpaces
    _ <- char '!'
    skipSpaces
    return ()

-- Helper routines for generating the build rules
makeSourceRule :: [String] -> [String] -> [String] -> ModuleSearchTree -> SourceFile -> Rules ()
makeSourceRule fortranFlags cFlags cIncludeFlags moduleSearchTree source =
    makeSpecialSourceRule Map.empty fortranFlags cFlags cIncludeFlags moduleSearchTree source

makeSpecialSourceRule :: SpecialCommands -> [String] -> [String] -> [String] -> ModuleSearchTree -> SourceFile -> Rules ()
makeSpecialSourceRule specialCommands fortranFlags cFlags cIncludeFlags moduleSearchTree source =
    let buildDir = getNodeDirectory moduleSearchTree
        compDeps = compileTimeDepends source cIncludeFlags moduleSearchTree in
    case source of
        FortranSourceFile src _ _ modulesContained ->
            map (buildDir </>) (objectFileName : moduleFileNames) &%> \(obj:_) -> do
                need =<< liftIO compDeps
                case Map.lookup src specialCommands of
                    Just command ->
                        cmd command ["-o", obj, src]
                    Nothing ->
                        cmd compiler ["-c", "-J" ++ buildDir] additionalIncludeFlags fortranFlags ["-o", obj, src]
            where
                objectFileName = takeFileName src -<.> "o"
                moduleFileNames = map (-<.> "mod") $ Set.toList modulesContained
                additionalIncludeFlags = map ("-I" ++ ) (getAdditionalBuildDirs moduleSearchTree)
        CSourceFile src _ ->
            objectFileName %> \obj -> do
                need =<< liftIO compDeps
                case Map.lookup src specialCommands of
                    Just command ->
                        cmd command ["-o", obj, src]
                    Nothing ->
                        cmd compiler ["-c"] cFlags ["-o", obj, src]
            where
                objectFileName = buildDir </> takeFileName src -<.> "o"

getAdditionalBuildDirs :: ModuleSearchTree -> [FilePath]
getAdditionalBuildDirs moduleSearchTree =
    let remainingNodes = getRemainingNodes moduleSearchTree
        fromThisLevel = map getNodeDirectory (Set.toList remainingNodes)
        fromLowerLevels = concatMap getAdditionalBuildDirs remainingNodes in
    fromThisLevel ++ fromLowerLevels

makeExecutableRule :: [String] -> [String] -> ModuleSearchTree -> Program -> Rules ()
makeExecutableRule flags cIncludeFlags moduleSearchTree program =
    buildDir </> getProgramName program <.> exe %> \x -> do
        linkTimeDepends' <- liftIO $ linkTimeDepends (getProgramSource program) cIncludeFlags moduleSearchTree
        need linkTimeDepends'
        cmd compiler linkTimeDepends' ["-o", x] flags
    where
        buildDir = getNodeDirectory moduleSearchTree

compileTimeDepends :: SourceFile -> [String] -> ModuleSearchTree -> IO [FilePath]
compileTimeDepends source cIncludeFlags moduleSearchTree = do
    others <- recursiveCompileTimeDepends source cIncludeFlags moduleSearchTree
    return $ getFileName source : others

recursiveCompileTimeDepends :: SourceFile -> [String] -> ModuleSearchTree -> IO [FilePath]
recursiveCompileTimeDepends source cIncludeFlags moduleSearchTree =
    case source of
        FortranSourceFile _ fortranModulesUsed _ _ -> do
            modules <- foldM collect Set.empty fortranModulesUsed
            return $ Set.toList modules
            where
                collect previousModules nextModule =
                    if nextModule `Map.member` getNodeModules moduleSearchTree then
                        return $ (getNodeDirectory moduleSearchTree </> nextModule <.> "mod") `Set.insert` previousModules
                    else
                        do
                            moreMods <- foldM recursePart Set.empty (getRemainingNodes moduleSearchTree)
                            return $ moreMods `Set.union` previousModules
                        where
                            recursePart prev tree = do
                                more <- recursiveCompileTimeDepends source cIncludeFlags tree
                                return $ Set.fromList more `Set.union` prev
        CSourceFile file _ ->
            getHeaders file cIncludeFlags

linkTimeDepends :: SourceFile -> [String] -> ModuleSearchTree -> IO [FilePath]
linkTimeDepends source cIncludeFlags moduleSearchTree = do
    (fromModules, fromHeaders) <- recursiveLinkTimeDepends cIncludeFlags (Set.empty, Set.empty) source moduleSearchTree
    return $ Set.toList (fromModules `Set.union` fromHeaders)

recursiveLinkTimeDepends :: [String] -> (Set.Set FilePath, Set.Set FilePath) -> SourceFile -> ModuleSearchTree -> IO (Set.Set FilePath, Set.Set FilePath)
recursiveLinkTimeDepends cIncludeFlags (fromModules, fromHeaders) source moduleSearchTree =
    let obj = getNodeDirectory moduleSearchTree </> (takeFileName . getFileName) source -<.> "o"
        withCurrentObj = obj `Set.insert` fromModules in
    if obj `Set.member` fromModules then
        return (fromModules, fromHeaders)
    else
        case source of
            FortranSourceFile _ fortranModulesUsed cModulesUsed _ ->
                foldM (linkTimeModuleSearch cIncludeFlags moduleSearchTree) (withCurrentObj, fromHeaders) (Set.toList (fortranModulesUsed `Set.union` cModulesUsed))
            CSourceFile file fortranModulesUsed -> do
                headers <- getHeaders file cIncludeFlags
                theseHeaders <- objsFromHeaders headers moduleSearchTree
                foldM (linkTimeModuleSearch cIncludeFlags moduleSearchTree) (withCurrentObj, Set.fromList theseHeaders `Set.union` fromHeaders) (Set.toList fortranModulesUsed)

linkTimeModuleSearch :: [String] -> ModuleSearchTree -> (Set.Set FilePath, Set.Set FilePath) -> ModuleName -> IO (Set.Set FilePath, Set.Set FilePath)
linkTimeModuleSearch cIncludeFlags moduleSearchTree (fromModules, fromHeaders) moduleName  =
    case maybeModule of
        Just module' ->
            recursiveLinkTimeDepends cIncludeFlags (fromModules, fromHeaders) (getModuleSource module') moduleSearchTree
        Nothing ->
            foldM collect (fromModules, fromHeaders) (getRemainingNodes moduleSearchTree)
    where
        maybeModule = Map.lookup moduleName (getNodeModules moduleSearchTree)
        collect prevObjs nextSearchTree =
            linkTimeModuleSearch cIncludeFlags nextSearchTree prevObjs moduleName

getHeaders :: FilePath -> [String] -> IO [FilePath]
getHeaders file cIncludeFlags = do
    (exitCode, makeDepends, err) <- readProcessWithExitCode compiler (file : "-MM" : cIncludeFlags) ""
    case exitCode of
        ExitSuccess ->
            return $ filter ((`elem` headerExts) . takeExtension) (splitOn " " makeDepends)
        ExitFailure _ ->
            fail err

objsFromHeaders :: [FilePath] -> ModuleSearchTree -> IO [FilePath]
objsFromHeaders headers moduleSearchTree =
    liftM (map toObj) $ filterM cExists headers
    where
        toObj header = getNodeDirectory moduleSearchTree </> takeFileName header -<.> "o"
        cExists :: FilePath -> IO Bool
        cExists header = do
            filesPresent <- mapM (doesFileExist . (header -<.>)) cExts
            return $ or filesPresent

removeIfExists :: FilePath -> IO ()
removeIfExists file = removeFile file `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
