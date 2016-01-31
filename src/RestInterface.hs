{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RestInterface where

import Network.Wai
import Servant
import Network.Wai.Handler.Warp
import System.FilePath (addTrailingPathSeparator)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import WaiAppStatic.Types
import Network.HTTP.Types.Status

import Cluster

type Root   =  Static
type Static =  Raw

serveRoot   :: Server Root
serveRoot    = serveStatic

serveStatic :: Server Static
serveStatic  = staticApp $
             ( defaultFileServerSettings
            $  addTrailingPathSeparator "browser" ) { ss404Handler = Just serve404 }

serve404 :: Application
serve404 _ respond = respond $ responseFile notFound404 [] "browser/index.html" Nothing

serveRest   :: Server Static
serveRest    = undefined

app         :: Application
app          = serve (Proxy :: Proxy Root) serveRoot

run :: ClusterNode value transceiver -> IO ()
run _ = Network.Wai.Handler.Warp.run 8081 app
