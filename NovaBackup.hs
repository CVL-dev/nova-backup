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
import Control.Monad.State
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity
import Data.Maybe
import Data.List ( isInfixOf )
import Safe (headMay)
import System.Environment ( getArgs )
import System.Cmd
import System.IO
import System.Process

data VM = VM { vmHash   :: String
             , vmName   :: String
             , vmStatus :: Status
             , vmNet    :: Maybe Network
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

delimeter :: ParsecT String u Data.Functor.Identity.Identity (Char)
delimeter = do
    char '+'
    many (char '-')
    char '+'
    many (char '-')
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
    delimeter
    newline
    header
    newline
    delimeter
    newline

    lines <- manyTill (statusLine <* newline) (delimeter >> newline)

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


runBackups dryRun  = do
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["list"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    let x = parse list "" stdout

    case x of (Right result) -> do let activeVMs = filter (\v -> (vmStatus v == Active) && (isJust $ vmNet v)) result
                                   if dryRun
                                    then forM_ activeVMs (putStrLn . backupCmd)
                                    else forM_ (map backupCmd activeVMs) system
              (Left  err)    -> print err

showBackups = do
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "nova" ["image-list"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    let cvlLines = filter (\x -> isInfixOf "CVL_BACKUP_" x) (lines stdout)
    forM_ cvlLines putStrLn

go :: [String] -> IO ()
go ["--dry-run", "--run-backups"] = runBackups True
go ["--run-backups"]              = runBackups False
go ["--show-backups"]             = showBackups

go _ = do
    putStrLn "Usage:"
    putStrLn ""
    putStrLn "    NovaBackup [--dry-run] --run-backups"
    putStrLn ""
    putStrLn "    NovaBackup --show-backups"
    putStrLn ""

main :: IO ()
main = getArgs >>= go



