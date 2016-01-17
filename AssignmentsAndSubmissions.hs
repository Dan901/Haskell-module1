module AssignmentsAndSubmissions where

import Data.Char
import Data.List
import System.IO
import System.Directory
import System.IO.Error
import System.FilePath
import Control.Monad
import Data.Time.Clock


-- | Root directory for storing everything
root = "/home"</>"dan"</>"PUH-Project"

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data Type = Homework | Exam | Project deriving Show

data Assignment = Assignment { year     :: Year
                             , typeOf   :: Type
                             , number   :: Int
                             } deriving Show

data Configuration = Configuration { published    :: UTCTime -- When to publish
                                   , deadline     :: UTCTime -- Submission deadline
                                   , lateDeadline :: UTCTime -- Late submission deadline
                                   , files        :: [String] -- File names to expect
                                   , minScore     :: Double -- Minimum achievable
                                   , maxScore     :: Double -- Maximum achievable
                                   , required     :: Double -- Score req to pass
                                   } deriving (Show, Read)


data Submission = Submission { assignment     :: Assignment
                             , uid            :: UserIdentifier
                             , uploadedFiles  :: [FilePath]
                             } deriving Show



-- | From module User
type UserIdentifier = String

-- | Values for testing
time = (read "2016-01-01 00:00:01 UTC")::UTCTime
a1 = Assignment 2015 Homework 5
a2 = Assignment 2015 Homework 6
c1 = Configuration time time time ["Homework.hs", "Exercise.hs"] 0 10 5


-- | Computes a file path for an assignment
getAssignmentPath :: Assignment -> FilePath
getAssignmentPath a = root</>(show $ year a)</>(map toLower $ show $ typeOf a)</>(show $ number a)


-- | Creates a new assignment from assignment, configuration and PDF file
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a c pdf = do
  let dir = getAssignmentPath a
  createDirectoryIfMissing True dir
  writeFile (dir</>".config") $ show c
  copyFile pdf $ dir</>"Assignment.pdf"


-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration a = do
  let f = (getAssignmentPath a)</>".config"
  e <- doesFileExist f
  if e 
    then do
      s <- readFile f
      return (read s :: Configuration)
    else error "Assignment does not exist"


-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions a = do
  let dir = getAssignmentPath a
  e <- doesDirectoryExist dir
  if e
    then do
      files <- listDir dir
      return $ filter (\f -> (f/="Assignment.pdf") && (f/=".config")) files
    else error "Assignment does not exist"

       
-- | Lists files and subdirectories in given directory
listDir :: FilePath -> IO [FilePath]
listDir dir = getDirectoryContents dir >>=  
                filterM (fmap not . doesDirectoryExist)


-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a id = do
  let dir = (getAssignmentPath a)</>id
  e <- doesDirectoryExist dir
  files <- listDir dir
  return $ Submission a id files

-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will override already made
-- | submissions.
upload :: Assignment -> UserIdentifier -> String -> String -> IO (Maybe Submission)
upload a id text name = do
  let dir = getAssignmentPath a
  e <- doesDirectoryExist dir
  if e
    then do
      createDirectoryIfMissing False $ dir</>id
      conf <- getConfiguration a
      if (null $ files conf) || (name `elem` files conf)
        then do
          writeFile (dir</>id</>name) text
          s <- getSubmission a id
          return $ Just s
        else
          error "Filename is not permitted"
    else error "Assignment does not exist"


-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles s = return $ uploadedFiles s


-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath s = (getAssignmentPath $ assignment s)</>(uid s)
