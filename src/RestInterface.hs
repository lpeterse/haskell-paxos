{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RestInterface where

import Network.Wai
import Servant
import Network.Wai.Handler.Warp

import Cluster

type Root   =  Rest
          :<|> Static
type Static =  Raw
type Rest   =  Raw

serveRoot   :: Server Root
serveRoot    = serveRest
          :<|> serveStatic

serveStatic :: Server Static
serveStatic  = serveDirectory "browser"

serveRest   :: Server Static
serveRest    = serveStatic

app         :: Application
app          = serve (Proxy :: Proxy Root) serveRoot

run :: Cluster value -> IO ()
run _ = Network.Wai.Handler.Warp.run 8081 app
