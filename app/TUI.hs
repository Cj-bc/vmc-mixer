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

import Control.Concurrent.Async (mapConcurrently, async, link, wait)
import Control.Exception (bracket)
import Control.Monad (void, forever, forM)
import qualified Data.Vector as V
import qualified Sound.OSC as OSC
import Sound.OSC.Transport.FD (Transport, withTransport, sendPacket, recvPacket, close)
import Sound.OSC.Transport.FD.UDP (udpServer, openUDP)

import VMCMixer.UI.Brick.Attr
import VMCMixer.UI.Brick (app, AppState(..), Name(..), initialState)
import Brick (defaultMain)

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
    return a

  -- Opens outputSocket, send messages received.
  output <- async $ sendPacket' outAddr msgIn
  void . sequence $ wait <$> (output:inputAsyncs)

sendPacket' :: (String, Int) -> Input OSC.Packet -> IO ()
sendPacket' addr input = do
  withTransport (uncurry openUDP $ addr) $ \socket -> do
    runEffect $ fromInput input >-> (await >>= \packet -> liftIO (sendPacket socket packet))
    performGC

awaitPacket :: (String, Int) -> Output OSC.Packet -> IO ()
awaitPacket addr output =
  withTransport (uncurry udpServer $ addr) $ \socket -> do
    runEffect $ (forever $ liftIO (recvPacket socket) >>= yield) >-> toOutput output
    performGC
