{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import GHC.Generics
import Network.Wai
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

type Root   =  Rest
          :<|> Static
type Static =  Raw
type Rest   =  Raw

serveRoot   :: Server Root
serveRoot    = serveRest
          :<|> serveStatic

serveStatic :: Server Static
serveStatic  = serveDirectory "browser"

serveRest = serveStatic

app :: Application
app = serve (Proxy :: Proxy Root) serveRoot

application :: IO ()
application = run 8081 app
