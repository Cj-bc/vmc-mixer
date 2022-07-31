{- |
Module      :  Main
Description :  mix multiple VMCP data and produce new data
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

This file is part of vmc-mixer.

vmc-mixer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

vmc-mixer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with vmc-mixer. If not, see <https://www.gnu.org/licenses/>.


-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Concurrent.Async (mapConcurrently, async, link, wait, Async, cancel)
import Control.Exception (bracket)
import Control.Monad (void, forever, forM)
import Control.Monad.Trans.State (execStateT, StateT(..), modify', get)
import Data.List (find)
import qualified Data.Vector as V
import qualified Sound.OSC as OSC
import Sound.OSC.Transport.FD (Transport, withTransport, sendPacket, recvPacket, close)
import Sound.OSC.Transport.FD.UDP (udpServer, udp_server, sendTo, udpSocket)

import qualified Network.Socket as N
import VMCMixer.UI.Brick.Attr
import VMCMixer.UI.Brick.Event
import VMCMixer.UI.Brick (app, AppState(..), Name(..), initialState)
import Brick (defaultMain)
import Brick.BChan (BChan, newBChan, readBChan)

import Pipes.Concurrent
import Pipes

main = do
  void $ defaultMain app initialState

{-
今気をつけなければいけないもの:
+ Asyncをきちんとcancelする
+ Socketをきちんとcloseする
-}
main' :: IO ()
main' = do
  let inputs   = [("127.0.0.1", 39541), ("192.168.10.3", 39541)]
      outAddr = ("127.0.0.1", 39540)

  -- Create 'Pipes.Concurrent.Mailbox', which received packet will be
  -- go through.
  (msgOut, msgIn) <- spawn unbounded

  -- Create 'Async's for each input socket.
  inputAsyncs <- forM inputs $ \addr -> do
    a <- async $ awaitPacket addr msgOut
    link a
    return (addr, a)

  -- Opens outputSocket, send messages received.
  -- 'N.defaultPort' will let system decide what port number to use.
  -- https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html#v:bind
  output <- async $ withTransport (udp_server . fromIntegral $ N.defaultPort) $ \socket -> do
                                                         runEffect $ fromInput msgIn >-> sendIt outAddr socket
                                                         performGC
  void . sequence $ wait <$> (output:inputAsyncs)

-- | Treats brick UI's event and do whatever we need.
mainLoop :: BChan VMCMixerUIEvent -> Output OSC.Packet -> [((String, Int), Async ())] -> Async () -> IO [Async ()]
mainLoop eventCh packetOutput initialInputs outputAsync =  return . fmap snd =<< execStateT go initialInputs
  where
    go :: StateT [((String, Int), Async ())] IO ()
    go = forever $ do
      msg <- liftIO $ readBChan eventCh
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

      

-- | Awaits from given 'Input', and send it to given Address.
sendIt :: (MonadIO m, MonadFail m) => (String, Int) -> OSC.UDP -> Consumer OSC.Packet m ()
sendIt (host, port) socket = forever $ do
  packet <- await
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4

  -- I needed to use 'sendTo' instead of 'send' so that it does not requiire
  -- to have connection.
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
