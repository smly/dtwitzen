{-# OPTIONS -fglasgow-exts #-}
module Main (main) where
import System.Process (runInteractiveProcess)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import System.IO
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath (combine)
import System.Timeout
import Fetch (fetchContents)
import Request (Account(..), reqTimeline)
import Network.HTTP (Response(..))
import Codec.Binary.Base64.String (decode)
import qualified System.IO.UTF8 as U

import Foreign
import Foreign.C
import System.Posix.Signals

foreign import ccall "sleep"  system_sleep  :: Int -> IO ()

type Twit = (String, String, Integer)

usage :: IO ()
usage = U.hPutStrLn stderr $ "usage: ./dtwitzen <dzen's arguments>"

waitInterval :: Int
waitInterval = 5 * 60 -- secounds

tolerance :: Int
tolerance = 10^7 -- microseconds

main :: IO ()
main =
    do { args <- getArgs
       ; account <- config
       ; let initState = 0
       ; (inp,out,_,pid) <- runInteractiveProcess dzen args Nothing Nothing
       ; forkIO(U.hPutStrLn inp "dtwitzen" >> hFlush inp)
       ; mainLoop initState account inp
       ; return ()
       }
    where dzen = "dzen2"

mainLoop :: Integer -> Account -> Handle -> IO Integer
mainLoop last account inp =
    do { t <- printStatus last account inp
       ; forkIO(hFlush inp)
       ; system_sleep waitInterval
       ; mainLoop t account inp
       }

strFormat :: (String,String,Integer) -> String
strFormat (a,b,_) = (" [[" ++ b ++ "]] " ++ a)

printRecent :: Handle -> Integer -> [Twit] -> IO Integer
printRecent inp t twit =
    do { mapM_ ( \str -> U.hPutStrLn inp str >> hFlush inp ) $ reverse $
               map strFormat $ takeWhile (\(x,y,z) -> (z > t)) twit
       ; return $ lastId
       }
    where lastId = (\(x,y,z) -> z) $ head twit

printStatus :: Integer -> Account -> Handle-> IO (Integer)
printStatus t account inp =
    do { mr <- timeout tolerance $ reqTimeline account
       ; case mr of
           Nothing -> (system_sleep waitInterval >> return t)
           Just (Right res)
               -> do { (contents, others, statusId) <- fetchContents $ rspBody res
                     ; let twit = zip3 contents others (map read statusId)
                     ; printRecent inp t twit >>= return
                     }
       }

config :: IO Account
config =
    do { home <- getHomeDirectory
       ; let configFile = combine home ".dtwitzen"
       ; exists <- doesFileExist configFile
       ; case exists of
           True      -> readAccount configFile >>= return
           otherwise -> error "$HOME/.dtwitzen: file not found."
       }

readAccount :: FilePath -> IO Account
readAccount configFile =
    do { inh <- openFile configFile ReadMode
       ; inpStr <- hGetLine inh
       ; case isEncoded inpStr of
           True  -> (stringToAc $ decode inpStr) >>= return
           False -> (stringToAc inpStr) >>= return
       }

isEncoded [] = True
isEncoded (x:xs)
    | x==':' = False
    | otherwise = isEncoded xs

stringToAc :: String -> IO Account
stringToAc line =
    do { let (username, password) = parseAccount line ""
       ; return $ MkAccount { acUsername = username
                            , acPassword = password
                            }
       }
    where
      parseAccount :: String -> String -> (String, String)
      parseAccount (x:xs) user
          | x==':' = (reverse user,xs)
          | otherwise = parseAccount xs (x:user)
