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

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket)
import Control.Monad (void, forever)
import qualified Data.Vector as V
import Sound.OSC.Transport.FD (Transport, withTransport, sendPacket, recvPacket, close)
import Sound.OSC.Transport.FD.UDP (udpServer, openUDP)

import VMCMixer.UI.Brick.Attr
import VMCMixer.UI.Brick (app, AppState(..), Name(..), initialState)
import Brick (defaultMain)

main = do
  void $ defaultMain app initialState

main' :: IO ()
main' = do
  let inputs   = [("127.0.0.1", 39541), ("192.168.10.3", 39541)]
      (out_addr, out_port) = ("127.0.0.1", 39540)
      openServer = uncurry udpServer

  void $ withTransports (sequence $ fmap openServer inputs) $ \ins ->
    withTransport (openUDP out_addr out_port) $ \sendUdp -> do
    let bypass inSocket outSocket = forever $ do
          packet <- recvPacket inSocket
          sendPacket outSocket packet
    void $ mapConcurrently (flip  bypass sendUdp) ins
    
withTransports :: Transport t => IO [t] -> ([t] -> IO a) -> IO a
withTransports generator = bracket generator (sequence . fmap close)

