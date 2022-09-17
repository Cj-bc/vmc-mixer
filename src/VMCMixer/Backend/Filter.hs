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

== フィルタリングの仕組み

「より優先順位の高い 'Performer' からパケットが送信されている間は、優先度の低い 'Performer' からの
パケットは送信しない」ようにしています。

具体的には各 'MarionetteMsgAddresses' 毎に直前にフィルターを通ったパケットの送信元 'Performer' を記録しておき、
それと新しく来たパケットの送信元 'Performer' の優先順位を比較、優先順位が高い場合のみ送信
するようにしています。

-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module VMCMixer.Backend.Filter where
import Control.Monad (liftM2, forever, when)
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (StateT)
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import Data.Maybe (maybe, fromMaybe)
import Data.VMCP.Marionette (MarionetteMsg)
import Data.VRM (BlendShapeExpression)
import Data.UnityEditor (HumanBodyBones)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Extras (view)
import Pipes (Pipe, await, yield, cat, for)
import VMCMixer.Types
import Control.Monad.Writer (MonadWriter, tell)


-- | State
data FilterLayerState = FilterLayerState { _messageFilter :: Filter
                                         , _previousPerformer :: HMap.HashMap MarionetteMsgAddresses [Performer]
                                         }
makeLenses ''FilterLayerState

filterLayerInitialState :: FilterLayerState
filterLayerInitialState = FilterLayerState (Filter HMap.empty) HMap.empty

-- | Command sender to do some action
data SenderCmd = UpdateFilter Filter -- ^ Update filter information used in filter
               | Packet Performer MarionetteMsg   -- ^ Packet to send
  
-- | Apply Packet filter before sending it.
--
-- It will check
-- + Where the packet is came from
-- + Wheather higher-prioritized packet isn't ongoing
applyFilter :: (Monad m, MonadWriter [String] m) => Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) ()
applyFilter = for cat $ \case
  UpdateFilter f ->
    modify $ messageFilter.~f
  Packet p msg -> do
    let msgAddr = extractAddress msg
    tell [show (p^.performerPort) ++ ": " ++ show msgAddr]
    shouldYield <- gets $ applyFilter' p msgAddr
    prevs <- gets (HMap.lookup msgAddr . view previousPerformer)
    if shouldYield
      then yield msg
      else tell ["FILTERED: " ++ show (p^.performerPort) ++ ": " ++ show msgAddr ++ "; " ++ show prevs]
    updatePrev p msgAddr

{-
この時点の実装だと、同じ 'Performer' からのパケットを連続で受け取った際は 'then' 節ではなく
'else' 節に行く ('<' であり '<=' でないため)
そうするとこのフィルターを通るためには直近に送信されたパケットが全て同じ 'Performer' からのものである必要があるが、
複数の 'Performer' からデータが送られてくる以上、直近の規定数が同じ 'Performer' からのものである確率はそんなに高くない。
その結果として、送信しない判断が出されやすい。

最も優先順位が高い 'Performer' でも同じことが起きてしまい、結果として何も送信されないといった事態になる。

かといって '<=' にするとどうなるか。今度は同じ 'Performer' からのパケットを連続で受け取った際は全て 'else' 節に
行くこととなり、これは優先順位の低い 'Performer' に関しても同じことが言える。
つまり、誤って送信する判断が取られやすい。


ここで、「前回の 'Performer' と今回のパケットを送信してきた 'Performer' と送信可否の組合せ」を考えるとこんな感じになる:

| 前回 | 今回 | 送信する？        |
| Low  | Low  | 'else' 節に任せる |
| Low  | High | YES               |
| High | Low  | No                |
| High | High | YES               |

これを見ると、「同じ

但し、2以上の 'Performer' から同じ 'MarionetteMsgAddresses' のパケットが送られてきた場合の事を考えると
その通りでもない

'_previousPerformer' に溜まった 'Performer' の中で、一つでも優先順位の高いパケットがあれば送信しない、
なければ送信する、という判断でどうか？
-}

-- | Apply filter
applyFilter' :: Performer -> MarionetteMsgAddresses -> FilterLayerState -> Bool
applyFilter' p msgAddr layerState =
  let fil  = HMap.lookup msgAddr . view (messageFilter.filters) $ layerState
      filter_ = view messageFilter layerState
      prev = HMap.lookup msgAddr . view previousPerformer $ layerState
  in case (fil, prev) of
       -- If filter isn't set, that message should be passed
       (Nothing, _) -> True
       (Just _, Nothing) -> True
       (Just _, Just prev') ->
         let prevPerformerPriority    = calcPriority filter_ msgAddr <$> prev'
             currentPerformerPriority = calcPriority filter_ msgAddr p
         in all (currentPerformerPriority <=) prevPerformerPriority

-- | Calculate priority value for given Performer based on given 'Filter'
--
-- If 'Performer' isn't on filter list, it'll get lowest priority
--
-- Priority will _decrease_ as value gets bigger.
-- i.e. highest priority value is @0@.
calcPriority :: Filter -> MarionetteMsgAddresses -> Performer -> Int
calcPriority f addr p = fromMaybe maxBound
  $ HMap.lookup addr (view filters f) >>= List.elemIndex p


-- | Update
--
-- 指定した 'MarionetteMsgAddresses' を前回投げた
updatePrev :: Monad m => Performer -> MarionetteMsgAddresses -> Pipe SenderCmd MarionetteMsg (StateT FilterLayerState m) ()
updatePrev p msgAddr = modify $ previousPerformer%~HMap.insertWith (\n o -> take 10 $ n++o) msgAddr [p]
