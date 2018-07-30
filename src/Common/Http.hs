module Common.Http where

import Control.Lens
import Control.Lens.Combinators
import Data.ByteString.Lazy.Internal
import Network.Wreq (getWith,
                     postWith,
                     checkResponse,
                     defaults,
                     Response)
import Network.Wreq.Types (Postable)

get :: String -> IO (Response ByteString)
get url = getWith opts url
  where opts = set checkResponse (Just (\_ _ -> return ())) defaults

post :: Postable a => String -> a -> IO (Response ByteString)
post url a = postWith opts url a
  where opts = set checkResponse (Just (\_ _ -> return ())) defaults

