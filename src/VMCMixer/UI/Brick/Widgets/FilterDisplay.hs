{- |
Module      :  VMCMixer.UI.Brick.Widgets.FilterDisplay
Description :  Brick widget to display filters
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
{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module VMCMixer.UI.Brick.Widgets.FilterDisplay where
import Brick.Widgets.Core (Named(..), vBox, hBox, str, txt, clickable, padLeft, withAttr)
import Brick.Widgets.Border (hBorder)
import Brick.Types (Widget, Padding(Pad), EventM)
import qualified Data.HashMap.Strict as HMap
import VMCMixer.Types (MarionetteMsgAddresses, Performer, performerName, performerPort, Filter(Filter))
import Lens.Micro ((^.), (%~), (&), _2, (.~), over, each)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as Vty
import Brick (AttrName)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Zipper


data FilterDisplay n = FilterDisplay { _displayName :: n
                                     , _containedFilters :: Zipper (MarionetteMsgAddresses, V.Vector Performer)
                                     , _fallbackFilter :: (n, Performer)
                                     }
makeLenses ''FilterDisplay

toFilter :: FilterDisplay n -> Filter
toFilter d = Filter (d^.fallbackFilter._2)
             . HMap.fromList
             . over (each._2) V.toList
             $ d^.containedFilters.before ++ [d^.containedFilters.peeked] ++ d^.containedFilters.after

-- | Add new filter spec to peeked address
filterAdd :: Performer -> FilterDisplay n -> FilterDisplay n
filterAdd p display = display&containedFilters.peeked._2%~(`V.snoc` p)

-- | Remove specified filter spec from peeked address
filterRemove :: Performer -> FilterDisplay n -> FilterDisplay n
filterRemove p display = display&containedFilters.peeked._2%~V.filter (/= p)

-- | Set new fallback 'Performer'
filterSetFallback :: Performer -> FilterDisplay n -> FilterDisplay n
filterSetFallback p display = display&fallbackFilter._2.~p

filterDisplay :: n -> [(MarionetteMsgAddresses, V.Vector Performer)] -> (n, Performer) -> FilterDisplay n
filterDisplay n (peeked':rest) fallback = FilterDisplay n (Zipper peeked' [] rest) fallback

instance Named (FilterDisplay n) n where
  getName = view displayName

renderAddrInfo :: (Ord n, Show n) => Performer -> Widget n
renderAddrInfo performer = hBox [txt . maybe "" id $ view performerName performer
                                , str $ " (" ++ (show $ view performerPort performer) ++ ")"
                                ]

renderFallback :: (Ord n, Show n) => FilterDisplay n -> Widget n
renderFallback fs = let (name, p) = view fallbackFilter fs
                    in clickable name (renderAddrInfo p)
 
renderFilterDisplay :: (Ord n, Show n) => Bool -> FilterDisplay n -> Widget n
renderFilterDisplay isFocused map =
  let filters = mconcat [(False,) <$> map^.containedFilters.before
                        , [(True, map^.containedFilters.peeked)]
                        , (False,) <$> map^.containedFilters.after
                        ]
  in vBox $ [renderFallback map, hBorder] ++ (uncurry renderFilterInfoRow <$> filters)

renderFilterInfoRow :: (Ord n, Show n) => Bool -> (MarionetteMsgAddresses, V.Vector Performer) -> Widget n
renderFilterInfoRow isFocused (addr, ls) = withAttr atr $ vBox [str $ show addr
                                                               , padLeft (Pad 2) . vBox . V.toList $ renderAddrInfo <$> ls
                                                               ]
  where
    atr = if isFocused then filterDisplayPeekedAttr else filterDisplayAttr

-- | The top-level attribute used for entire filterDisplay widget
filterDisplayAttr :: AttrName
filterDisplayAttr = "filterDisplay"

-- | The attribute used for 'Filter' that is currently peeked
filterDisplayPeekedAttr :: AttrName
filterDisplayPeekedAttr = filterDisplayAttr <> "Peeked"


handleFilterDisplayEvent :: Vty.Event -> FilterDisplay n -> EventM n (FilterDisplay n)
handleFilterDisplayEvent (Vty.EvKey (Vty.KChar 'j') []) d = return $ d&containedFilters%~(\d -> fromMaybe d $ view next d)
handleFilterDisplayEvent (Vty.EvKey (Vty.KChar 'k') []) d = return $ d&containedFilters%~(\d -> fromMaybe d $ view previous d)
handleFilterDisplayEvent _ d = return d
