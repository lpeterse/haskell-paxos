module DistributedLog where

import Control.Exception
import Control.Monad.STM.TVar

commit :: DistributedLog value -> value -> IO ()

stream :: DistributedLog value -> IO (value, IO )

data Logstream value
   = Logstream value (IO (Logstream value))


-- Wie sieht das oberste Objekt aus, das das Cluster repräsentiert?

data Peer
   = Peer
     { identity :: Identity
     , address  :: Address
     }

-- Welche Clustereigenschaften müssen synchronisiert werden?

data ClusterConfig
   = ClusterConfig
     { reincarnation :: Int
     , peers         :: [Peer]
     , 
     }

class Transmitter t where
  type Message t
  type Peer    t
  type Context t
  send      :: Peer t -> Message t -> (Context t) ()
  broadcast :: Message t -> (Context t) ()
  receive   :: (Context t) (Peer t, Message t)

-- Wie übergebe/maintaine ich den main-thread?

-- 1. DistributedLog Object exportiert ein `run :: IO ()`
--    -> erstes Aufrufen   -> startet Paxos
--    -> weiteres aufrufen -> wirft exception, wenn es bereits läuft
--    -> Nutzer kann Thread kontrollieren, stoppen, Status prüfen

data DistributedLog value
   = DistributedLog
     { peers   :: TVar [Peer]
     , socket  :: TVar Socket
     , logs    :: IntMap Log
     , running :: TVar Bool
     }

data LogEntry value
   = LogEntry value (IO (LogEntry value))

run :: DistributedLog value -> IO ()
run log = do
  -- Waiting here if another thread is already executing until
  -- it has died or been stopped.
  atomically $ do
    r <- swapTVar (running log) True
    if r
      then retry
      else return ()
  main `finally` atomically $ writeTVar (running log) False
  where
    main = undefined

entries :: DistributedLog value -> IO (LogEntry value)
entries = undefined

append  :: DistributedLog value -> value -> IO ()
append = undefined

