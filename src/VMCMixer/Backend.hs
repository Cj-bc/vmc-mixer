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

module VMCMixer.Backend (
  mainLoop
, sendIt
, awaitPacket
  ) where

import Control.Concurrent.Async (async, link, Async, cancel)
import Control.Monad (forever, forM_, join)
import Control.Monad.State.Strict (execStateT, StateT(..), modify', get)
import Data.List (find)
import Sound.OSC.Transport.FD (withTransport, recvMessage)
import Sound.OSC.Transport.FD.UDP (udp_server)
import Pipes.Concurrent
import Pipes
import Pipes.VMCP.Marionette (recvMarionetteMsgWithUdp, mkPacket)
import VMCMixer.UI.Brick.Event
import VMCMixer.Types (Marionette, Performer(Performer), performerPort)
import VMCMixer.Backend.Sender (sendIt')
import VMCMixer.Backend.Filter (SenderCmd(..), applyFilter, FilterLayerState(FilterLayerState), filterLayerInitialState)
import Data.VMCP.Message (VMCPMessage, fromOSCMessage)
import Lens.Micro ((^.))
import qualified Network.Socket as N

-- | Treats brick UI's event and do whatever we need.
mainLoop :: (IO VMCMixerUIEvent) -> Output SenderCmd -> [Performer] -> IO [Async ()]
mainLoop readUIEvent packetOutput initialInputs =  return . fmap snd =<< execStateT (spawnInitials >> go) []
  where
    spawn :: Performer -> StateT [(Performer, Async ())] IO ()
    spawn performer = do
      a <- liftIO . async $ awaitPacket performer packetOutput
      liftIO $ link a
      modify' (\l -> (performer, a):l)

    spawnInitials :: StateT [(Performer, Async ())] IO ()
    spawnInitials = forM_ initialInputs spawn

    go :: StateT [(Performer, Async ())] IO ()
    go = forever $ do
      msg <- liftIO readUIEvent
      case msg of
        NewAddr p -> spawn p
          -- TODO: Let brick know that work is done by emitting Msg
        RemoveAddr p -> do
          s <- get
          case find ((== p) . fst) s of
            Nothing -> pure ()
            Just (_, asyncObj) -> do
              liftIO $ cancel asyncObj
              modify' $ filter ((/= p) . fst)
        UIEventUpdateFilter filter ->
          runEffect $ yield (UpdateFilter filter) >-> toOutput packetOutput

-- | Run 'sendIt'' with UDP socket bracket.
-- TODO: @Performer 0 Nothing@ is temporary written here, it should be given by user at runtime.
sendIt :: Marionette -> Input SenderCmd -> IO ()
sendIt addr msgIn = withTransport (udp_server . fromIntegral $ N.defaultPort) $ \socket -> do
  flip execStateT (filterLayerInitialState $ Performer 0 Nothing)
    $ runEffect (fromInput msgIn
                 >-> applyFilter
                 >-> mkPacket
                 >-> sendIt' addr socket)
  performGC


awaitPacket :: Performer -> Output SenderCmd -> IO ()
awaitPacket performer output =
  withTransport (udp_server (performer^.performerPort)) $ \socket -> do
    runEffect $ (recvMarionetteMsgWithUdp socket)
      >-> for cat (yield . Packet performer)
      >-> toOutput output
    performGC
