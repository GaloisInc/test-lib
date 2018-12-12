module TestLib (Config(..), mainWith, mainWithOpts, main, Options(..)) where

import SimpleGetOpt
import Control.Monad (foldM,when)
import System.Directory ( getDirectoryContents,doesDirectoryExist
                        , createDirectoryIfMissing,canonicalizePath )
import System.Environment (withArgs)
import System.FilePath((</>),(<.>),splitFileName,splitDirectories,takeFileName
                      , isRelative, pathSeparator, takeExtension )
import System.Process ( createProcess,CreateProcess(..), StdStream(..)
                      , proc, waitForProcess, readProcessWithExitCode
                       )
import System.IO(IOMode(..),withFile,Handle,hSetBuffering,BufferMode(..))
import System.Exit(exitSuccess)
import Test.Framework (defaultMain,Test,testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)
import qualified Control.Exception as X
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


-- | Specifies how the test runner should behave.
data Config = Config
  { cfgDefaultBinary :: String
    -- ^ Use this binary unless one is explicitly provided.

  , cfgBinOpts       :: String -> [String]
    -- ^ Given a test, produce a set of parameters for the binary.

  , cfgIsTestCase    :: String -> Bool
    -- ^ Examine a file name to determine if it is a test.
  }

main :: IO ()
main =
  do opts <- getOpts options
     mainWithOpts opts

-- | Define a @main@ function for an executable.
mainWith :: Config -> IO ()
mainWith cfg =
  do opts0 <- getOpts options
     let opts = opts0 { optCfg = Just cfg }
     mainWithOpts $ case optBinary opts of
                      "" -> opts { optBinary = cfgDefaultBinary cfg }
                      _  -> opts

-- | Run with the given options
mainWithOpts :: Options -> IO ()
mainWithOpts opts =
  do when (optHelp opts) $
        do dumpUsage options
           exitSuccess

     -- Normalize paths
     bin' <- if pathSeparator `elem` optBinary opts
                     && isRelative (optBinary opts)
                then canonicalizePath (optBinary opts)
                else return (optBinary opts)
     resultsDir <- canonicalizePath (optResultDir opts)
     let opts' = opts { optResultDir = resultsDir, optBinary = bin' }

     createDirectoryIfMissing True resultsDir
     testFiles  <- findTests opts'
     withArgs (optOther opts') (defaultMain (generateTests opts' testFiles))





-- Command Line Options --------------------------------------------------------

data Options = Options
  { optBinary         :: String
  , optOther          :: [String]
  , optHelp           :: Bool
  , optResultDir      :: FilePath
  , optTests          :: [FilePath]
  , optDiffTool       :: Maybe String
  , optIgnoreExpected :: Bool
  , optTestFileExts   :: [String]
  , optBinFlags       :: [String]
    -- ^ Add this flags to the binary, followed by the test file
  , optCfg            :: Maybe Config
  }


options :: OptSpec Options
options = OptSpec
  { progDefaults = Options { optBinary         = ""
                           , optOther          = []
                           , optHelp           = False
                           , optResultDir      = "output"
                           , optTests          = []
                           , optDiffTool       = Nothing
                           , optBinFlags       = []
                           , optTestFileExts   = []
                           , optIgnoreExpected = False
                           , optCfg            = Nothing
                           }

  , progOptions =
      [ Option "c" ["exe"]
        "the binary executable to use"
        $ ReqArg "PATH" $ \s o -> Right o { optBinary = s }

     , Option "F" ["flag"]
        "add a flag to the test binary"
        $ ReqArg "STRING" $ \s o -> Right o { optBinFlags = optBinFlags o ++[s]}

      , Option "r" ["result-dir"]
        "the result directory for test runs"
        $ ReqArg "PATH" $ \s o -> Right o { optResultDir = s }

      , Option "p" ["diff-prog"]
        "use this diffing program on failures"
        $ ReqArg "PROG" $ \s o -> Right o { optDiffTool = Just s }

      , Option "T" []
        "add an argument to pass to the test-runner main"
        $ ReqArg "STRING" $ \s o -> Right o { optOther = s : optOther o }

      , Option "i" ["ignore-expected"]
        "ignore expected failures"
        $ NoArg $ \o -> Right o { optIgnoreExpected = True }

      , Option "" ["ext"]
        "files with this extension are tests"
        $ ReqArg "STRING" $ \s o ->
            let e = case s of
                      '.' : _ -> s
                      _ -> '.' : s
            in Right o { optTestFileExts = e : optTestFileExts o }


      , Option "h" ["help"]
        "display this message"
        $ NoArg $ \o -> Right o { optHelp = True }
      ]

  , progParamDocs =
      [ ("FILES/DIRS",   "The tests to run.") ]

  , progParams = \p o -> Right o { optTests = p : optTests o }
  }

-- Test Generation -------------------------------------------------------------

-- | Turn a directory tree of tests into a collection of tests.
-- Tests in the same directory share a test-group.
generateTests :: Options -> TestFiles -> [Test]
generateTests opts = loop ""
  where
  loop dir tests = as ++ grouped
    where
    as      = map (generateAssertion opts dir) (Set.toList (files tests))
    grouped = [ testGroup path (loop (dir </> path) t)
              | (path,t) <- Map.toList (subDirs tests) ]



-- | This is how we run a test.
generateAssertion :: Options -> FilePath -> FilePath -> Test
generateAssertion opts dir file = testCase file runTest
  where
  -- file locations:
  resultDir        = optResultDir opts </> dir        -- test output goes here
  goldFile         = dir </> file <.> "stdout"        -- what we expect to see
  knownFailureFile = dir </> file <.> "fails"         -- expected failur
  resultOut        = resultDir </> file <.> "stdout"  -- outputfile

  runTest =
    do createDirectoryIfMissing True resultDir
       withFile resultOut WriteMode $ \ hout ->
         do hSetBuffering hout NoBuffering
            runBinary opts hout dir file

       out      <- readFile resultOut
       expected <- readFile goldFile
       mbKnown  <- X.try (readFile knownFailureFile)
       checkOutput mbKnown expected out

  checkOutput mbKnown expected out
    | expected == out =
      case mbKnown of
        Left _  -> return ()
        -- Test passed, but we expected a failure.
        Right _ ->
          assertFailure $
            "Test completed successfully.  Please remove " ++ knownFailureFile


      -- Gold and output differ
    | otherwise =
      case mbKnown of

        -- No expected errors.
        Left (X.SomeException {})

          -- A custom diff tool was lister.  We don't run it,
          -- we just write it on the terminal for easy copy and paste.
          | Just prog <- optDiffTool opts ->
            do goldFile' <- canonicalizePath goldFile
               assertFailure $ unlines
                  [ unwords [ prog, goldFile', "\\\n    ", resultOut ]
                  , makeGold resultOut goldFile'
                  ]

          -- Just use "diff"
          | otherwise ->
            do goldFile' <- canonicalizePath goldFile
               (_,diffOut,_) <-
                  readProcessWithExitCode "diff" [ goldFile', resultOut ] ""
               assertFailure $ unlines [ diffOut, makeGold resultOut goldFile' ]

        Right fail_msg
          -- Expected error.
          | optIgnoreExpected opts -> return ()
          -- Different expected error.
          | otherwise              -> assertFailure fail_msg

  makeGold out gold =
    unlines [ "# If output is OK:"
            , unwords [ "cp", out, "\\\n    ", gold ]
            ]



-- | Write the output of stdout and stderr for a run of the binary to
-- the given handle.
runBinary :: Options -> Handle -> FilePath -> String -> IO ()
runBinary opts hout path file =
  do let bin  = optBinary opts
         args = case optCfg opts of
                  Just x -> optBinFlags opts ++ cfgBinOpts x file
                  Nothing -> optBinFlags opts ++ [file]
     (_, _, _, ph) <- createProcess (proc bin args)
                        { cwd     = Just path
                        , std_out = UseHandle hout
                        , std_in  = Inherit
                        , std_err = UseHandle hout
                        }
     _ <- waitForProcess ph
     return ()




-- Test Discovery --------------------------------------------------------------

-- | Directory structure of the discovered tests.  Each entry in the map
-- represents a single folder, with the top-level list representing tests
-- inside the base directory.
data TestFiles = TestFiles
  { subDirs :: Map String TestFiles
  , files   :: Set String
  }

-- | An empty collection of tests.
noTests :: TestFiles
noTests = TestFiles { subDirs = Map.empty, files = Set.empty }

-- | Join two collections of tests, removing duplicates.
joinTests :: TestFiles -> TestFiles -> TestFiles
joinTests ts1 ts2 = TestFiles
  { files   = Set.union (files ts1) (files ts2)
  , subDirs = Map.unionWith joinTests (subDirs ts1) (subDirs ts2)
  }

-- | Create a test collection with a single file.
testFile :: FilePath -> TestFiles
testFile path = foldr addDir baseTest dirs
  where
  baseTest   = noTests { files = Set.singleton file }
  (dir,file) = splitFileName path
  dirs       = splitDirectories dir
  addDir d t = TestFiles (Map.singleton d t) Set.empty


-- | Find a bunch of tests.
findTests :: Options -> IO TestFiles
findTests opts = searchMany noTests (optTests opts)
  where
  searchMany tests = foldM step tests

  step tests path =
    do isDir <- doesDirectoryExist path
       if isDir
          then do fs <- getDirectoryContents path
                  searchMany tests [ path </> f | f <- fs, not (isDotFile f) ]
          else if isTestFile path
                    then return $! joinTests (testFile path) tests
                    else return tests

  isDotFile path = case path of
                     '.' : _ -> True
                     _       -> False

  isTestFile f = case optCfg opts of
                   Nothing -> byExt
                   Just cfg -> byExt || cfgIsTestCase cfg file
    where
    file  = takeFileName f
    byExt = takeExtension file `elem` optTestFileExts opts







