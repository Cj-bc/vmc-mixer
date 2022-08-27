{- |
Module      :  VMCMixer.Backend.Filter
Description :  Filter layer that we use filtering messages
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

'MarionetteMsg' の種別によるフィルタリング機能を実装します。
アドレス別にフィルタリングをするために、 'MarionetteMsg' から各アドレスのみを
別の型に抽出して扱っています。

本来は 'MarionetteMsg' だけでなく 'VMCPMessage' インスタンスを持つ型全て
に対応するのが良いですが、現状 'MarionetteMsg' しかないので一旦これで。
-}
{-# LANGUAGE TemplateHaskell #-}
module VMCMixer.Backend.Filter where
import Control.Monad (liftM2, forever)
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (StateT)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe (maybe)
import Data.VMCP.Marionette (MarionetteMsg)
import Data.VRM (BlendShapeExpression)
import Data.UnityEditor (HumanBodyBones)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((%~), (.~))
import Lens.Micro.Extras (view)
import Pipes (Pipe, await, yield)
import VMCMixer.Types

  
-- | Represents one filter
data Filter = Filter { _fallback :: Performer
                     , _filters :: Map.Map MarionetteMsgAddresses [Performer] -- ^ Use those 
                     }
makeLenses ''Filter
  

-- | State
data FilterLayerState = FilterLayerState { _fallback :: Performer
                                         , _filters  :: Map.Map MarionetteMsgAddresses [Performer]
                                         , _previousPerformer :: Map.Map MarionetteMsgAddresses Performer
                                         }
makeLenses ''FilterLayerState

filterLayerInitialState :: Performer -> FilterLayerState
filterLayerInitialState fallback = FilterLayerState fallback Map.empty Map.empty

-- | Command sender to do some action
data SenderCmd = UpdateFilter Filter -- ^ Update filter information used in filter
               | Packet Performer MarionetteMsg   -- ^ Packet to send
  
-- | Apply Packet filter before sending it.
--
-- It will check
-- + Where the packet is came from
-- + Wheather higher-prioritized packet isn't ongoing
applyFilter :: MonadIO m => Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) ()
applyFilter = do
  forever go
  where
    go :: MonadIO m => Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) ()
    go = do
      cmd <- await
      case cmd of
        UpdateFilter (Filter fb fs)
          modify $ (fallback.~fb).(filters.~fs)
        Packet p msg -> do
          let msgAddr = extractAddress msg
          shouldYield <- applyFilter' p msgAddr
          if shouldYield
            then yield msg
            else pure ()

          updatePrev p msgAddr

-- | Apply filter
applyFilter' :: MonadIO m => Performer -> MarionetteMsgAddresses -> Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) Bool
applyFilter' p msgAddr = do
  fil <- gets (Map.lookup msgAddr .view filters)
  prev <- gets (Map.lookup msgAddr . view previousPerformer)
  case (fil, prev) of
    -- If filter isn't set, that message should be passed
    (Nothing, _) -> return True
    (Just ps, Nothing) -> return True
    (Just ps, Just prev') ->
      -- Lower number has higher priority
      let prevPerformerPriority    = maybe 10000 id $ List.findIndex (== prev') ps
          currentPerformerPriority = maybe 10000 id $ List.findIndex (== p) ps
      in return $ prevPerformerPriority > currentPerformerPriority

-- | Update
--
-- 指定した 'MarionetteMsgAddresses' を前回投げた
updatePrev :: MonadIO m => Performer -> MarionetteMsgAddresses -> Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) ()
updatePrev p msgAddr = modify $ previousPerformer%~(Map.update (const $ Just p) msgAddr)
