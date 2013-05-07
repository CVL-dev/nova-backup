{-
Nova backup utility.

Copyright (C) 2013  Carlo Hamalainen, University of Queensland.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Enquires: Carlo.Hamalainen@gmail.com or help@massive.org.au
-}

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity
import Data.Maybe
import Data.List (intercalate, sortBy, isInfixOf)
import Data.Ord (comparing)
import Data.Time (UTCTime, getCurrentTime)
import System.Environment ( getArgs )
import System.Cmd
import System.IO
import System.Process

isLeft  = either (const True) (const False)
isRight = either (const False) (const True)

data VM = VM { vmHash   :: String
             , vmName   :: String
             , vmStatus :: Status
             , vmNet    :: Maybe Network
             } deriving (Show)

data Image = Image { imageID        :: String
                   , imageName      :: String
                   , imageStatus    :: Status
                   , imageServer    :: String
                   } deriving (Show)

data Network = Network { networkCell :: String
                       , networkIP   :: String
                       } deriving (Show)

data Status = Active | Building | Unknown | StatusError String deriving (Eq, Show)

sToStatus :: String -> Status
sToStatus "ACTIVE"  = Active
sToStatus "BUILD"   = Building
sToStatus "UNKNOWN" = Unknown
sToStatus s         = StatusError s

strip  = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

delimeter4 :: ParsecT String u Data.Functor.Identity.Identity (Char)
delimeter4 = do
    char '+'
    many (char '-')
    char '+'
    many (char '-')
    char '+'
    many (char '-')
    char '+'
    many (char '-')
    char '+'

delimeter2 :: ParsecT String u Data.Functor.Identity.Identity (Char)
delimeter2 = do
    char '+'
    many (char '-')
    char '+'
    many (char '-')
    char '+'


header :: ParsecT String u Data.Functor.Identity.Identity (Char)
header = do
    char '|'
    spaces
    string "ID"
    spaces
    char '|'
    spaces
    string "Name"
    spaces
    char '|'
    spaces
    string "Status"
    spaces
    char '|'
    spaces
    string "Networks"
    spaces
    char '|'

headerImage :: ParsecT String u Data.Functor.Identity.Identity (Char)
headerImage = do
    char '|'
    spaces
    string "Property"
    spaces
    char '|'
    spaces
    string "Value"
    spaces
    char '|'

headerImageList :: ParsecT String u Data.Functor.Identity.Identity (Char)
headerImageList = do
    char '|'
    spaces
    string "ID"
    spaces
    char '|'
    spaces
    string "Name"
    spaces
    char '|'
    spaces
    string "Status"
    spaces
    char '|'
    spaces
    string "Server"
    spaces
    char '|'

propertyLine :: ParsecT String u Data.Functor.Identity.Identity ((String, String))
propertyLine = do
    char '|'
    spaces
    propertyName <- (rstrip . lstrip) <$> many (noneOf "|")
    char '|'
    spaces
    propertyValue <- (rstrip . lstrip) <$> many (noneOf "|")
    char '|'

    return (propertyName, propertyValue)

imageLine :: ParsecT String u Data.Functor.Identity.Identity (Image)
imageLine = do
    char '|'
    spaces
    id <- many (noneOf " ")
    spaces
    char '|'
    spaces
    name <- (rstrip . lstrip) <$> many (noneOf "|")
    char '|'
    spaces
    status <- sToStatus <$> many (noneOf " ")
    spaces
    char '|'
    server <- (lstrip . rstrip) <$> many (noneOf "|")
    char '|'

    return $ Image id name status server

networkInfo :: ParsecT String u Data.Functor.Identity.Identity (Maybe Network)
networkInfo = do
    location <- many (noneOf "=\n")
    char '='
    ip <- many (noneOf " \n")

    return $ Just $ Network location ip

statusLine :: ParsecT String u Data.Functor.Identity.Identity VM
statusLine = do
    char '|'
    spaces
    hash <- many (noneOf " ")
    spaces
    char '|'
    spaces
    name <- rstrip <$> (many (noneOf "|"))
    spaces
    char '|'
    spaces
    state <- sToStatus <$> (many (noneOf " "))
    spaces
    char '|'
    spaces
    net <- try (networkInfo <* spaces <* char '|') <|> (spaces >> char '|' >> (return Nothing))
    return $ VM hash name state net

list = do
    delimeter4
    newline
    header
    newline
    delimeter4
    newline

    lines <- manyTill (statusLine <* newline) (delimeter4 >> newline)

    return lines

imagePropertyList = do
    delimeter2
    newline
    headerImage
    newline
    delimeter2
    newline

    lines <- manyTill (propertyLine <* newline) (delimeter2 >> newline)

    return lines

imageList = do
    delimeter4
    newline
    headerImageList
    newline
    delimeter4
    newline

    lines <- manyTill (imageLine <* newline) (delimeter4 >> newline)

    return lines

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else do x <- hGetContents handle
                return x

backupCmd vm = "nova backup " ++ hash ++ " '" ++ backupImage ++ "' " ++ " weekly 4"
    where hash = vmHash vm
          backupImage = "CVL_BACKUP_" ++ (vmName vm)

parseVmList = do
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["list"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    return $ parse list "" stdout

runBackups dryRun  = do
    x <- parseVmList

    case x of (Right result) -> do let activeVMs = filter (\v -> (vmStatus v == Active) && (isJust $ vmNet v)) result
                                   if dryRun
                                    then forM_ activeVMs                 (\a -> do getCurrentTime >>= (putStrLn . show)
                                                                                   putStrLn $ backupCmd a)
                                    else forM_ (map backupCmd activeVMs) (\c -> do getCurrentTime >>= (putStrLn . show)
                                                                                   putStrLn c
                                                                                   system c)
              (Left  err)    -> print err

showBackups = do
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["image-list"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    let cvlLines = filter (\x -> isInfixOf "CVL_BACKUP_" x) (lines stdout)
    forM_ cvlLines putStrLn

getImageProperties :: String -> [String] -> IO [(String, String, String)]
getImageProperties name imageIDs = do
    blah <- mapM (\i -> do (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["image-show", i]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
                           stdout <- readRestOfHandle hout
                           stderr <- readRestOfHandle herr -- FIXME handle stderr?

                           let (Right imageProperties) = parse imagePropertyList "" stdout -- FIXME ignoring the Left
                               progress = lookup "progress" imageProperties
                               status   = lookup "status"   imageProperties
                               updated  = lookup "updated"  imageProperties
                           return (fromJust updated, fromJust progress, fromJust status)) imageIDs
    return $ reverse $ sortBy (comparing (\(x, _, _) -> x)) blah

showLatestBackups = do
    -- Grab a list of the current VMs. For each of these we will check
    -- for a backup image.
    _vmList <- parseVmList
    when (isLeft _vmList) (error $ show _vmList)
    let (Right vmList) = _vmList

    -- Run nova image-list
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["image-list"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    let _images = parse imageList "" stdout
    when (isLeft _images) (error $ show _images)
    let (Right images) = _images

    forM_ vmList (\v -> do let name         = vmName v :: String
                               backupName   = "CVL_BACKUP_" ++ name
                               imageIDs     = map imageID $ filter (\i -> imageName i == backupName) images :: [String]
                           blah <- getImageProperties name imageIDs
                           let dates = map (\(x, _, _) -> x) blah
                               newest = if dates == [] then "NONE" else head dates

                           -- putStrLn $ name ++ " ==> " ++ (intercalate " " dates)
                           putStrLn $ newest ++ " " ++ name
                           -- forM_ blah (\(updated, progress, status) -> putStrLn $ updated ++ " " ++ progress ++ " " ++ status)
                           -- putStrLn ""
                 )





go :: [String] -> IO ()
go ["--dry-run", "--run-backups"] = runBackups True
go ["--run-backups"]              = runBackups False
go ["--show-backups"]             = showBackups
go ["--show-latest-backups"]      = showLatestBackups

go _ = do
    putStrLn "Usage:"
    putStrLn ""
    putStrLn "    nova-backup-util [--dry-run] --run-backups"
    putStrLn ""
    putStrLn "    nova-backup-util --show-backups"
    putStrLn ""
    putStrLn "    nova-backup-util --show-latest-backups"
    putStrLn ""

main :: IO ()
main = getArgs >>= go



