module Request
    (
      Account(..)
    , reqTimeline
    ) where

import Data.Maybe
import System.Environment
import Network.Browser
import Network.HTTP
import Network.URI

data Account = MkAccount { acUsername :: String
                         , acPassword :: String
                         }
             deriving (Show, Read)

reqTimeline :: Account -> IO (Result Response)
reqTimeline = httpRequest . mkRequest

mkRequest :: Account -> (Request, Account)
mkRequest ac = (Request { rqURI = uri
                        , rqMethod = GET
                        , rqHeaders = [Header HdrHost "twitter.com"]
                        , rqBody = "" }
               ,ac)
    where uri = fromJust $ parseURI $ "http://twitter.com/statuses/friends_timeline/"++(acUsername ac)++".xml"

httpRequest :: (Request, Account) -> IO (Result Response)
httpRequest (req,account)
 = do
  proxy <- catch
           (getEnv "HTTP_PROXY" >>= return . (flip Proxy Nothing))
           (\_ -> return NoProxy)
  (_, res) <- browse( let uri = rqURI req in
                       do { setOutHandler (noTrace)
                          ; setErrHandler (noTrace)
                          ; setProxy proxy
                          ; setAuthorities [AuthBasic { auRealm = ""
                                                      , auUsername = acUsername account
                                                      , auPassword = acPassword account
                                                      , auSite = uri }
                                           ]
                          ; request req
                          }
                     )
  return (Right res)
      where noTrace s = return ()
