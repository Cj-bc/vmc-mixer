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


= Thread structure

This have complecated thread structure. I'm not sure if this is good design,
but I didn't have other idea. If you know better implementation,
please let me know by opening issue.

Illustration below describes how it's structured:

@
Main thread --- Event manager -+- receiver
                               |- receiver
                               |...
@

== Definitions:

- [@Main thread@]: what @stack run@ starts. Also, it's the thread brick's UI loop is running.
- [@Event manager@]:  Thread that will receive and emit Brick's CustomEvent.
- [@receiver@]: Thread that will receive 'OSC.Packet' from 'Network.Socket'
-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Concurrent.Async (async, Async, cancel)
import Control.Monad (void)
import VMCMixer.UI.Brick.Event
import VMCMixer.UI.Brick (app, initialState)
import VMCMixer.Backend (mainLoop, sendIt)
import VMCMixer.Options (getOption)
import qualified VMCMixer.Options as Opt
import Brick (defaultMain)
import Brick.BChan (BChan, newBChan, readBChan)

import Pipes.Concurrent
import Lens.Micro ((^.))

{-
今気をつけなければいけないもの:
+ Asyncをきちんとcancelする
+ Socketをきちんとcloseする
-}
main :: IO ()
main = do
  opts <- getOption
  let inputs  = opts^.Opt.inputs
      outAddr = opts^.Opt.out

  -- Create 'Pipes.Concurrent.Mailbox', which received packet will be
  -- go through.
  (msgOut, msgIn) <- spawn unbounded

  -- Opens outputSocket, send messages received.
  -- 'N.defaultPort' will let system decide what port number to use.
  -- https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html#v:bind
  output <- async $ sendIt outAddr msgIn

  brickCh <- newBChan 1
  restAsyncs <- async $ mainLoop (readBChan brickCh) msgOut inputs
  defaultMain app (initialState brickCh inputs)

  void $ cancel restAsyncs

