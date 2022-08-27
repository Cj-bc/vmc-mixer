{- |
Module      :  VMCMixer.Backend.Sender
Description :  Functions for backend's sender
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
module VMCMixer.Backend.Sender where
import Control.Monad (forever)
import Data.VMCP.Message (VMCPMessage, toOSCMessage)
import Data.VMCP.Marionette (MarionetteMsg)
import qualified Network.Socket as N
import qualified Sound.OSC as OSC
import Sound.OSC.Transport.FD (withTransport)
import Sound.OSC.Transport.FD.UDP (udp_server, sendTo)
import Sound.OSC.Packet (Packet(Packet_Message))
import Lens.Micro ((^.))
import Pipes.Concurrent
import Pipes
import VMCMixer.Types (Marionette, marionetteAddress, marionettePort) 

-- | Awaits from given 'Input', and send it to given Address.
sendIt' :: (MonadIO m, MonadFail m, VMCPMessage msg) => Marionette -> OSC.UDP -> Consumer msg m ()
sendIt' marionette socket = forever $ do
  let host = marionette^.marionetteAddress
      port = marionette^.marionettePort
  msg <- await
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4

  -- I needed to use 'sendTo' instead of 'send' so that it does not requiire to have connection.
  --
  -- Those two lines are borrowed from implementation of 'Sound.OSC.Transport.FD.UDP.udp_socket'
  -- https://hackage.haskell.org/package/hosc-0.19.1/docs/src/Sound.OSC.Transport.FD.UDP.html#udp_socket
  i:_ <- liftIO $ N.getAddrInfo (Just hints) (Just host) (Just (show port))
  liftIO $ sendTo socket (Packet_Message . toOSCMessage $ msg) (N.addrAddress i)
