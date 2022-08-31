{- |
Module      :  VMCMixer.Types
Description :  Common types for VMCMixer
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
{-# LANGUAGE TemplateHaskell #-}
module VMCMixer.Types where
import Data.Text (Text)
import Network.Socket (PortNumber)
import Lens.Micro.TH (makeLenses)
import qualified Data.VMCP.Marionette as Marionette
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap

data Performer = Performer { _performerPort :: Int
                           , _performerName :: Maybe Text
                           } deriving (Show, Eq)
makeLenses ''Performer

data Marionette = Marionette { _marionetteAddress :: String
                             , _marionettePort :: Int
                             , _marionetteName :: Maybe Text
                             } deriving (Show, Eq)
makeLenses ''Marionette

-- | Address-Only 'MarionetteMsg'
--
-- 'MarionetteMsg' で使われているアドレスのみを取り出した型です。
-- パラメーターを持っていると都合が悪いので取り出しています。
--
-- This might be better to be included in hVMCP
data MarionetteMsgAddresses = 
  -- | 利用可否
  Available
  -- | 送信側相対時刻
  | Time
  -- | モデルのrootとなるオブジェクトの絶対姿勢
  | RootTransform
  -- | モデルのrootとなるオブジェクトのLocal姿勢
  | BoneTransform -- HumanBodyBones
  -- | BlendShapeProxyの値。
  | VRMBlendShapeProxyValue -- BlendShapeExpression
  -- | 一連の内容が送信された後送信される
  | VRMBlendShapeProxyApply
  deriving (Show, Eq)


-- | Extract 'MarionetteMsgAddresses' from 'MarionetteMsg'
extractAddress :: Marionette.MarionetteMsg -> MarionetteMsgAddresses
extractAddress msg = case msg of
  Marionette.Available _                 -> Available
  Marionette.Time _                      -> Time
  Marionette.RootTransform _ _           -> RootTransform
  Marionette.BoneTransform _ _ _         -> BoneTransform -- HumanBodyBones
  Marionette.VRMBlendShapeProxyValue _ _ -> VRMBlendShapeProxyValue -- BlendShapeExpression
  Marionette.VRMBlendShapeProxyApply     -> VRMBlendShapeProxyApply
  unknown                     -> error $ mconcat ["Unknown MarionetteMsg: "
                                                 , show unknown
                                                 , "\n"
                                                 , "Please report this to https://github.com/Cj-bc/vmc-mixer/issues"
                                                 ]

-- | Represents one filter
data Filter = Filter { _fallback :: Performer
                     , _filters :: HMap.HashMap MarionetteMsgAddresses [Performer] -- ^ Use those 
                     }
makeLenses ''Filter
