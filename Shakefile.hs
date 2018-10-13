import           Control.Applicative          ((<|>))
import           Data.Char                    (isAsciiLower, isDigit, toLower)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.String.Utils            (endswith)
import           Development.Shake            (Action,
                                               Change (ChangeModtimeAndDigest),
                                               FilePattern, Rules, cmd,
                                               getDirectoryFilesIO, need, phony,
                                               removeFilesAfter, shakeArgs,
                                               shakeChange, shakeFiles,
                                               shakeOptions, want, (%>), (&%>),
                                               (<//>))
import           Development.Shake.FilePath   (dropExtension, exe, takeFileName,
                                               (-<.>), (<.>), (</>))
import           Text.ParserCombinators.ReadP (ReadP, char, eof, many, many1,
                                               option, pfail, readP_to_S,
                                               satisfy, skipSpaces, string)


-- All the extra type declarations we need
type ModuleName = String
type ProgramName = String
type ModuleNames = Set.Set ModuleName

data SourceFile = SourceFile {
      getFileName         :: FilePath
    , getModulesUsed      :: ModuleNames
    , getModulesContained :: ModuleNames
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
      ModuleUsed ModuleName
    | ModuleDefined ModuleName
    | ProgramDefined ProgramName
    | Other deriving (Show)

data ModuleSearchTree =
    Node {
      getNodeDirectory  :: FilePath
    , getNodeModules    :: Modules
    , getRemainingNodes :: Set.Set ModuleSearchTree
} deriving (Show)

-- A handful of constants that are just basic configuration
compiler = "gfortran"
fflags = ["-Wall",  "-Wextra", "-Werror", "-pedantic"]
develFlags = ["-g"] ++ fflags
develDir = "build" </> "devel"
releaseFlags = ["-O2"] ++ fflags
releaseDir = "build" </> "release"
unitTestBuildDir = "unit_test_build"

-- The main routine that executes the build system
main :: IO ()
main = do
    sourceFiles <- getDirectoriesFiles ["src"] ["*.f90"]
    (sources, modules, programs) <- scanSourceFiles Map.empty sourceFiles
    let releaseSearchTree = Node releaseDir modules Set.empty
    let develSearchTree = Node develDir modules Set.empty

    unitTestSourceFiles <- getDirectoriesFiles ["unit_test", "vegetables"] ["*.f90"]
    (unitTestSources, unitTestModules, unitTestPrograms) <-
        scanSourceFiles modules unitTestSourceFiles
    let unitTestSearchTree = Node unitTestBuildDir unitTestModules (Set.singleton develSearchTree)

    shakeArgs shakeOptions{shakeFiles="_shake", shakeChange=ChangeModtimeAndDigest} $ do
        want ["release"]
        phony "release" $
            need [releaseDir </> getProgramName prog <.> exe | prog <- Set.toList programs]

        mapM_ (makeSourceRule releaseFlags releaseSearchTree) sources
        mapM_ (makeExecutableRule releaseFlags releaseSearchTree) programs

        phony "devel" $
            need [develDir </> getProgramName prog <.> exe | prog <- Set.toList programs]

        mapM_ (makeSourceRule develFlags develSearchTree) sources
        mapM_ (makeExecutableRule develFlags develSearchTree) programs

        phony "unit_tests" $ do
            let progs = [unitTestBuildDir </> getProgramName prog <.> exe | prog <- Set.toList unitTestPrograms]
            need progs
            mapM_ (cmd :: FilePath -> Action ()) progs

        mapM_ (makeSourceRule develFlags unitTestSearchTree) unitTestSources
        mapM_ (makeExecutableRule develFlags unitTestSearchTree) unitTestPrograms

        phony "clean" $ do
            removeFilesAfter "build" ["//"]
            removeFilesAfter "unit_test_build" ["//"]

-- A little wrapper around getDirectoryFiles so we can get files from multiple directories
getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts =
    getDirectoryFilesIO "" newPatterns
    where
        newPatterns = concatMap appendExts dirs
        appendExts dir = map (dir <//>) exts

-- The routines we need to parse all the source files
scanSourceFiles :: Modules -> [FilePath] -> IO (SourceFiles, Modules, Programs)
scanSourceFiles previousModules = foldl addNextContents (return (Set.empty, Map.empty, Set.empty))
    where
        addNextContents previousContents file = do
            (sources, modules, programs) <- previousContents
            newContents <- scanSourceFile (previousModules `Map.union` modules) file
            case newContents of
                Left (source, program) ->
                    return (source `Set.insert` sources, modules, program `Set.insert` programs)
                Right (source, newModules) ->
                    return (source `Set.insert` sources, newModules `Map.union` modules, programs)

scanSourceFile :: Modules -> FilePath -> IO (Either (SourceFile, Program) (SourceFile, Modules))
scanSourceFile previousModules file = do
    fileLines <- readFileLinesIO file
    let eitherContents = foldl addLineContents (Right (Set.empty, Set.empty, Nothing)) fileLines
    case eitherContents of
        Right contents ->
            case contents of
                (modulesContained, modulesUsed, Nothing) ->
                    return $ Right (buildOutputWithModules file modulesUsed modulesContained)
                (modulesContained, modulesUsed, Just programName) ->
                    return $ Left (buildOutputWithProgram file modulesUsed modulesContained programName)
        Left err ->
            fail $ "*** Error in file " ++ file ++ ": " ++ err
    where
        addLineContents contents line =
            case contents of
                Right (previousModulesContained, previousModulesUsed, maybeProgram) ->
                    case parseLine line of
                        ModuleUsed moduleName ->
                            Right (previousModulesContained, moduleName `Set.insert` previousModulesUsed, maybeProgram)
                        ModuleDefined moduleName ->
                            case Map.lookup moduleName previousModules of
                                Just module' ->
                                    Left $ "module " ++ moduleName ++ " was already defined in " ++ getFileName (getModuleSource module')
                                Nothing ->
                                    if moduleName `Set.member` previousModulesContained then
                                        Left $ "module " ++ moduleName ++ " defined twice"
                                    else
                                        Right (moduleName `Set.insert` previousModulesContained, previousModulesUsed, maybeProgram)
                        ProgramDefined programName ->
                            case maybeProgram of
                                Just programName' ->
                                    Left $ "multiple programs defined: " ++ programName' ++ " and " ++ programName
                                Nothing ->
                                    Right (previousModulesContained, previousModulesUsed, Just programName)
                        Other ->
                            Right (previousModulesContained, previousModulesUsed, maybeProgram)
                Left err ->
                    Left err

readFileLinesIO :: FilePath -> IO [String]
readFileLinesIO file = do
    contents <- readFile file
    return $ lines contents

buildOutputWithModules :: FilePath -> ModuleNames -> ModuleNames -> (SourceFile, Modules)
buildOutputWithModules file modulesUsed modulesContained =
    let source = SourceFile file (modulesUsed `Set.difference` modulesContained) modulesContained
        modules = foldl (addModuleWithSource source) Map.empty modulesContained in
    (source, modules)
    where
        addModuleWithSource source previousModules moduleName =
            Map.insert moduleName (Module moduleName source) previousModules

buildOutputWithProgram :: FilePath -> ModuleNames -> ModuleNames -> ProgramName -> (SourceFile, Program)
buildOutputWithProgram file modulesUsed modulesContained programName =
    let source = SourceFile file modulesUsed modulesContained
        program = Program programName source in
    (source, program)

parseLine :: String -> LineContents
parseLine line =
    let line' = map toLower line
        result = readP_to_S doLineParse line' in
    getResult result
    where
        getResult (_:(contents, _):_) = contents
        getResult [(contents, _)]     = contents
        getResult []                  = Other

doLineParse :: ReadP LineContents
doLineParse = option Other usefulContents

usefulContents :: ReadP LineContents
usefulContents = moduleDeclaration <|> programDeclaration <|> useStatement

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
    return $ ModuleUsed modName

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

-- Helper routines for generating the build rules
makeSourceRule :: [String] -> ModuleSearchTree -> SourceFile -> Rules ()
makeSourceRule flags moduleSearchTree source =
    let buildDir = getNodeDirectory moduleSearchTree in
    map (buildDir </>) (objectFileName : moduleFileNames) &%> \(obj:_) -> do
        need $ compileTimeDepends source moduleSearchTree
        cmd compiler ["-c", "-J" ++ buildDir] additionalIncludeFlags flags ["-o", obj, src]
    where
        src = getFileName source
        objectFileName = takeFileName src -<.> "o"
        moduleFileNames = map (-<.> "mod") $ Set.toList (getModulesContained source)
        additionalIncludeFlags = map ("-I" ++ ) (getAdditionalBuildDirs moduleSearchTree)

getAdditionalBuildDirs :: ModuleSearchTree -> [FilePath]
getAdditionalBuildDirs moduleSearchTree =
    let remainingNodes = getRemainingNodes moduleSearchTree
        fromThisLevel = map getNodeDirectory (Set.toList remainingNodes)
        fromLowerLevels = concatMap getAdditionalBuildDirs remainingNodes in
    fromThisLevel ++ fromLowerLevels

makeExecutableRule :: [String] -> ModuleSearchTree -> Program -> Rules ()
makeExecutableRule flags moduleSearchTree program =
    buildDir </> getProgramName program <.> exe %> \x -> do
        need linkTimeDepends'
        cmd compiler flags ["-o", x] linkTimeDepends'
    where
        buildDir = getNodeDirectory moduleSearchTree
        linkTimeDepends' = linkTimeDepends (getProgramSource program) moduleSearchTree

compileTimeDepends :: SourceFile -> ModuleSearchTree -> [FilePath]
compileTimeDepends source moduleSearchTree =
    getFileName source : recursiveCompileTimeDepends source moduleSearchTree

recursiveCompileTimeDepends :: SourceFile -> ModuleSearchTree -> [FilePath]
recursiveCompileTimeDepends source moduleSearchTree =
    let mods = foldl collect Set.empty (getModulesUsed source) in
    Set.toList mods
    where
        collect previousModules nextModule =
            if nextModule `Map.member` getNodeModules moduleSearchTree then
                (getNodeDirectory moduleSearchTree </> nextModule <.> "mod") `Set.insert` previousModules
            else
                Set.fromList (concatMap (recursiveCompileTimeDepends source) (getRemainingNodes moduleSearchTree)) `Set.union` previousModules

linkTimeDepends :: SourceFile -> ModuleSearchTree -> [FilePath]
linkTimeDepends source moduleSearchTree =
    let objs = recursiveLinkTimeDepends Set.empty source moduleSearchTree in
    Set.toList objs

recursiveLinkTimeDepends :: Set.Set FilePath -> SourceFile -> ModuleSearchTree -> Set.Set FilePath
recursiveLinkTimeDepends previousFiles source moduleSearchTree =
    if obj `Set.member` previousFiles then
        previousFiles
    else
        foldl collect withCurrentObj (getModulesUsed source)
    where
        obj = getNodeDirectory moduleSearchTree </> (takeFileName . getFileName) source -<.> "o"
        withCurrentObj = obj `Set.insert` previousFiles
        collect prevObjs moduleName =
            prevObjs `Set.union` linkTimeModuleSearch withCurrentObj moduleName moduleSearchTree

linkTimeModuleSearch :: Set.Set FilePath -> ModuleName -> ModuleSearchTree -> Set.Set FilePath
linkTimeModuleSearch previousFiles moduleName moduleSearchTree =
    case maybeModule of
        Just module' ->
            recursiveLinkTimeDepends previousFiles (getModuleSource module') moduleSearchTree
        Nothing ->
            foldl collect previousFiles (getRemainingNodes moduleSearchTree)
    where
        maybeModule = Map.lookup moduleName (getNodeModules moduleSearchTree)
        collect prevObjs nextSearchTree =
            prevObjs `Set.union` linkTimeModuleSearch prevObjs moduleName nextSearchTree
