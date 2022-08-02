{- |
Module      :  VMCMixer.Backend
Description :  Backend programs for vmc-mixer
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

"Backend" is codes that treats VMCP messages.
Those functions are separated from UI.
-}

module VMCMixer.Backend where

import Control.Concurrent.Async (async, link, Async, cancel)
import Control.Monad (forever)
import Control.Monad.Trans.State (execStateT, StateT(..), modify', get)
import Data.List (find)
import qualified Sound.OSC as OSC
import Sound.OSC.Transport.FD (Transport, withTransport, sendPacket, recvPacket)
import Sound.OSC.Transport.FD.UDP (udpServer, udp_server, sendTo)
import qualified Network.Socket as N
import Pipes.Concurrent
import Pipes
import VMCMixer.UI.Brick.Event

-- | Treats brick UI's event and do whatever we need.
mainLoop :: (IO VMCMixerUIEvent) -> Output OSC.Packet -> Async () -> IO [Async ()]
mainLoop readUIEvent packetOutput outputAsync =  return . fmap snd =<< execStateT go []
  where
    go :: StateT [((String, Int), Async ())] IO ()
    go = forever $ do
      msg <- liftIO readUIEvent
      case msg of
        NewAddr host port -> do
          a <- liftIO . async $ awaitPacket (host, port) packetOutput
          liftIO $ link a
          modify' (\l -> ((host, port), a):l)
          -- TODO: Let brick know that work is done by emitting Msg
        RemoveAddr host port -> do
          s <- get
          case find ((== (host, port)) . fst) s of
            Nothing -> pure ()
            Just (_, asyncObj) -> do
              liftIO $ cancel asyncObj
              modify' $ filter ((/= (host, port)) . fst)


sendIt :: (String, Int) -> Input OSC.Packet -> IO ()
sendIt addr msgIn = withTransport (udp_server . fromIntegral $ N.defaultPort)
              $ \socket -> runEffect (fromInput msgIn >-> sendIt' addr socket)
                           >> performGC

-- | Awaits from given 'Input', and send it to given Address.
sendIt' :: (MonadIO m, MonadFail m) => (String, Int) -> OSC.UDP -> Consumer OSC.Packet m ()
sendIt' (host, port) socket = forever $ do
  packet <- await
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4

  -- I needed to use 'sendTo' instead of 'send' so that it does not requiire to have connection.
  --
  -- Those two lines are borrowed from implementation of 'Sound.OSC.Transport.FD.UDP.udp_socket'
  -- https://hackage.haskell.org/package/hosc-0.19.1/docs/src/Sound.OSC.Transport.FD.UDP.html#udp_socket
  i:_ <- liftIO $ N.getAddrInfo (Just hints) (Just host) (Just (show port))
  liftIO $ sendTo socket packet (N.addrAddress i)



awaitPacket :: (String, Int) -> Output OSC.Packet -> IO ()
awaitPacket addr output =
  withTransport (uncurry udpServer $ addr) $ \socket -> do
    runEffect $ (forever $ liftIO (recvPacket socket) >>= yield) >-> toOutput output
    performGC