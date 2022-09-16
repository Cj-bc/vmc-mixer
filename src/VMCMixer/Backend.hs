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
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module VMCMixer.Backend (
  mainLoop
, sendIt
, awaitPacket
  ) where

import Control.Concurrent.Async (async, link, Async, cancel, AsyncCancelled (AsyncCancelled))
import Control.Monad (forever, forM_, join)
import Control.Monad.State.Strict (execStateT, StateT(..), modify', get)
import Data.List (find)
import Sound.OSC.Transport.FD (withTransport, recvMessage)
import Sound.OSC.Transport.FD.UDP (udp_server)
import Pipes.Concurrent
import Pipes
import Pipes.VMCP.Marionette (recvMarionetteMsgWithUdp, mkPacket)
import VMCMixer.UI.Brick.Event
import VMCMixer.Types (Marionette, Performer(Performer), performerPort, performerName)
import VMCMixer.Backend.Sender (sendIt')
import VMCMixer.Backend.Filter (SenderCmd(..), applyFilter, FilterLayerState(FilterLayerState), filterLayerInitialState)
import Data.VMCP.Message (VMCPMessage, fromOSCMessage)
import Lens.Micro ((^.))
import qualified Network.Socket as N
import Control.Monad.Writer (runWriterT)
import Control.Exception (finally)
import qualified Data.Text as T

-- | Treats brick UI's event and do whatever we need.
mainLoop :: IO VMCMixerUIEvent -> Output SenderCmd -> [Performer] -> IO ()
mainLoop readUIEvent packetOutput initialInputs = do
  appendFile "/tmp/vmc-mixer.log" "[mainLoop]\tstarted.\n"
  children <- execStateT (spawnInitials >> go) []
  appendFile "/tmp/vmc-mixer.log" "[mainLoop]\tmain Loop finished. Doing cleanup...\n"
  -- Stop all 'awaitPacket' asyncs
  -- By explictly stop them, 'sendIt' async will also
  -- stop because 'fromInput' will be terminated.
  -- For more, please refer to pipes-concurrency's document:
  -- https://hackage.haskell.org/package/pipes-concurrency-2.0.14/docs/Pipes-Concurrent-Tutorial.html#g:3
  sequence_ $ cancel . snd <$> children
  appendFile "/tmp/vmc-mixer.log" "[mainLoop]\texited\n"

  where
    spawn :: Performer -> StateT [(Performer, Async ())] IO ()
    spawn performer = do
      a <- liftIO . async $ awaitPacket performer packetOutput
      liftIO $ link a
      modify' (\l -> (performer, a):l)

    spawnInitials :: StateT [(Performer, Async ())] IO ()
    spawnInitials = forM_ initialInputs spawn

    go :: StateT [(Performer, Async ())] IO ()
    go = do
      -- In order to return all 'Async's inside State,
      -- I have to escape from loop when 'mainLoop' receives
      -- cancellation.
      msgOrQuit <- liftIO $ (Right <$> readUIEvent)
                   `catch` \(_ :: AsyncCancelled) -> return $ Left ()
      liftIO $ appendFile "/tmp/vmc-mixer.log" "[mainLoop]\tmsg is provided\n"
      case msgOrQuit of
        Left _ -> pure ()
        Right msg -> executeMessage msg >> go

    -- | run 'VMCMixerUIEvent'
    executeMessage :: VMCMixerUIEvent -> StateT [(Performer, Async ())] IO ()
    executeMessage = \case
      NewAddr p -> spawn p
      -- TODO: Let brick know that work is done by emitting Msg
      RemoveAddr p -> do
        s <- get
        case find ((== p) . fst) s of
          Nothing -> pure ()
          Just (_, asyncObj) -> do
            liftIO $ cancel asyncObj
            modify' $ filter ((/= p) . fst)
      UIEventUpdateFilter filter -> do
        liftIO $ appendFile "/tmp/vmc-mixer.log" ("[mainLoop]\tFilter is updated to:" ++ show filter ++ "\n")
        runEffect $ yield (UpdateFilter filter) >-> toOutput packetOutput

-- | Run 'sendIt'' with UDP socket bracket.
sendIt :: Performer -> Marionette -> Input SenderCmd -> IO ()
sendIt _fallback addr msgIn = withTransport (udp_server . fromIntegral $ N.defaultPort) $ \socket -> do
  (a, w) <- runWriterT . flip execStateT (filterLayerInitialState _fallback)
            $ runEffect (fromInput msgIn
                         >-> applyFilter
                         >-> mkPacket
                         >-> sendIt' addr socket)
  performGC
  appendFile "/tmp/vmc-mixer.log"  $ unlines ("sendIt is finished. Below are logs from it":w)
  appendFile "/tmp/vmc-mixer.log" "\n---\n"

awaitPacket :: Performer -> Output SenderCmd -> IO ()
awaitPacket performer output =
  flip finally performGC
     $ withTransport (udp_server (performer^.performerPort)) $ \socket -> do
        runEffect
          $ recvMarionetteMsgWithUdp socket
          >-> for cat (yield . Packet performer)
          >-> toOutput output
