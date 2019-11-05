module Juicer
    ( makeDriver
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isAsciiLower
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( intercalate )
import           Data.String.Utils              ( replace
                                                , startswith
                                                )
import           System.FilePath                ( dropExtension
                                                , takeFileName
                                                )
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , char
                                                , many
                                                , many1
                                                , readP_to_S
                                                , satisfy
                                                , skipSpaces
                                                , string
                                                )

type ModuleName = String
type FunctionName = String

makeDriver :: FilePath -> [FilePath] -> IO ()
makeDriver driverFile collectionFiles = do
    used <- getTestInfo collectionFiles
    let driverName = (takeFileName . dropExtension) driverFile
    let program    = makeProgram driverName used
    writeFile driverFile program

getTestInfo :: [FilePath] -> IO [(ModuleName, [FunctionName])]
getTestInfo = mapM getIndividualTestInfo

getIndividualTestInfo :: FilePath -> IO (ModuleName, [FunctionName])
getIndividualTestInfo testFile = do
    let moduleName = (takeFileName . dropExtension) testFile
    functions <- scanTestFile testFile
    return (moduleName, functions)

scanTestFile :: FilePath -> IO [FunctionName]
scanTestFile testFile = do
    fileLines <- readFileLines testFile
    let maybeFuncs = map parseTestLine fileLines
    return $ foldl
        (\fs f -> case f of
            Nothing -> fs
            Just f' -> f' : fs
        )
        []
        maybeFuncs

makeProgram :: String -> [(ModuleName, [FunctionName])] -> String
makeProgram testDriverName testInfo =
    unlines
        $  [ ("program " ++ testDriverName)
           , "    implicit none"
           , ""
           , "    call run()"
           , "contains"
           , "    subroutine run()"
           ]
        ++ (makeUseStatements testInfo)
        ++ [ "        use iso_varying_string"
           , "        use Vegetables_m, only: TestItem_t, testThat, runTests"
           , ""
           , "        type(TestItem_t) :: tests"
           ]
        ++ (makeTestArray testInfo)
        ++ [ ""
           , "        call runTests(tests)"
           , "    end subroutine run"
           , "end program " ++ testDriverName
           ]

makeUseStatements :: [(ModuleName, [FunctionName])] -> [String]
makeUseStatements testInfo = map makeUseStatement testInfo

makeUseStatement :: (ModuleName, [FunctionName]) -> String
makeUseStatement testInfo = case testInfo of
    (_      , []   ) -> ""
    (modName, funcs) -> "        use " ++ modName ++ ", only: &\n" ++ funcParts
      where
        funcParts = intercalate ", &\n" (map rename funcs)
        rename func = "            " ++ renamed modName func ++ " => " ++ func

makeTestArray :: [(ModuleName, [FunctionName])] -> [String]
makeTestArray testInfo =
    [ "        type(TestItem_t) :: individual_tests("
        ++ show (length allFuncs)
        ++ ")"
        , ""
        ]
        ++ individual_assignments
        ++ ["        tests = testThat(individual_tests)"]
  where
    allFuncs = concatMap theFuncs testInfo
    theFuncs info = case info of
        (_      , []   ) -> []
        (modName, funcs) -> map (\f -> renamed modName f ++ "()") funcs
    individual_assignments = map
        (\(index, call) ->
            "        individual_tests(" ++ show index ++ ") = " ++ call
        )
        (zip [1 ..] allFuncs)

--makeTestArray :: [(ModuleName, [FunctionName])] -> [String]
--makeTestArray testInfo = ["    tests = testThat( &\n        [" ++ arrayParts ++ "])"]
--    where arrayParts = intercalate ", &\n        " $ concatMap theFuncs testInfo
--          theFuncs info = case info of
--              (_, [])          -> []
--              (modName, funcs) -> map (\f -> renamed modName f ++ "()") funcs

renamed :: String -> String -> String
renamed modName funcName = replace "test_test_" "" (modName ++ "_" ++ funcName)

parseTestLine :: String -> Maybe FunctionName
parseTestLine line =
    let line'  = map toLower line
        result = readP_to_S doTestLineParse line'
    in  getResult result
  where
    getResult (_ : (contents, _) : _) = contents
    getResult [(contents, _)        ] = contents
    getResult []                      = Nothing

doTestLineParse :: ReadP (Maybe FunctionName)
doTestLineParse = do
    skipSpaces
    _ <- string "function"
    skipAtLeastOneWhiteSpace
    funName <- validIdentifier
    skipSpaceOrOpenParen
    if startswith "test_" funName then return $ Just funName else return Nothing

skipSpaceOrOpenParen :: ReadP ()
skipSpaceOrOpenParen = skipAtLeastOneWhiteSpace <|> skipOpenParen

skipOpenParen :: ReadP ()
skipOpenParen = do
    _ <- char '('
    return ()

readFileLines :: FilePath -> IO [String]
readFileLines file = do
    contents <- readFile file
    return $ lines contents

skipAtLeastOneWhiteSpace :: ReadP ()
skipAtLeastOneWhiteSpace = do
    _ <- many1 whiteSpace
    return ()

validIdentifier :: ReadP String
validIdentifier = do
    first <- validFirstCharacter
    rest  <- many validIdentifierCharacter
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

whiteSpace :: ReadP Char
whiteSpace = satisfy (`elem` " \t")
