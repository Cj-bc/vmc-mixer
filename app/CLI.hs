{- |
Module      :  Main
Description :  vmc-mixer with CLI interface
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

This file is part of vmc-mixer.

vmc-mixer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

vmc-mixer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with vmc-mixer. If not, see <https://www.gnu.org/licenses/>.

= Commentary

This executable uses 'CLI' interface.
It isn't interactive, you should provide all information
at launching.
-}

module Main where
import VMCMixer.Types (Performer(Performer), Filter(Filter), MarionetteMsgAddresses(RootTransform), toFilter)
import VMCMixer.Backend (sendIt, awaitPacket)
import VMCMixer.Backend.Filter (SenderCmd (UpdateFilter))
import Control.Concurrent.Async (async, link, waitAny, cancel, forConcurrently_, wait)
import VMCMixer.Options  (getOption)
import qualified VMCMixer.Options as Opt
import Lens.Micro ((^.))
import Pipes.Concurrent (spawn, unbounded, Mailbox, toOutput)
import Control.Monad (void, forM)
import Data.VMCP.Marionette (MarionetteMsg)
import Pipes (runEffect, yield, (>->))
import qualified Data.HashMap.Strict as HMap

main = do
  opt <- getOption
  (msgOut, msgIn) <- spawn unbounded :: IO (Mailbox SenderCmd)

  print $ "Input from: " ++ (show $ opt^.Opt.performers)

  output <- async $ sendIt (opt^.Opt.marionette) msgIn

  -- set filter
  let filter_ = toFilter $ opt^.Opt.filterOpt
  runEffect $ yield (UpdateFilter filter_) >-> toOutput msgOut

  as <- forM (opt^.Opt.performers) $ \p -> do
    a <- async $ awaitPacket p msgOut
    link a
    return a
  -- forConcurrently_ (opt^.Opt.inputs) $ flip awaitPacket msgOut
  waitAny (output:as)
  void $ forM  as cancel
  void $ wait output
