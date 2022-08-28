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
module VMCMixer.UI.Brick.Widgets.FilterDisplay where
import Brick.Widgets.Core (Named(..), vBox, hBox, str, txt, clickable)
import Brick.Widgets.Border (hBorder)
import Brick.Types (Widget)
import Brick.Widgets.List (renderList, List)
import qualified Data.Map as M
import VMCMixer.Types (MarionetteMsgAddresses, Performer, performerName, performerPort)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

data FilterDisplay n = FilterDisplay { _displayName :: n
                                     , _containedFilters :: M.Map MarionetteMsgAddresses (List n Performer)
                                     , _fallbackFilter :: (n, Performer)
                                     }
makeLenses ''FilterDisplay

filterDisplay :: n -> [(MarionetteMsgAddresses, List n Performer)] -> (n, Performer) -> FilterDisplay n
filterDisplay n fs fallback = FilterDisplay n (M.fromList fs) fallback

instance Named (FilterDisplay n) n where
  getName = view displayName

renderAddrInfo :: (Ord n, Show n) => Bool -> Performer -> Widget n
renderAddrInfo isFocused performer = hBox [txt . maybe "" id $ view performerName performer
                                          , str $ " (" ++ (show $ view performerPort performer) ++ ")"
                                          ]

renderFallback :: (Ord n, Show n) => FilterDisplay n -> Widget n
renderFallback fs = let (name, p) = view fallbackFilter fs
                    in clickable name (renderAddrInfo False p)
 
renderFilterDisplay :: (Ord n, Show n) => Bool -> FilterDisplay n -> Widget n
renderFilterDisplay isFocused map =
        vBox $ [renderFallback map, hBorder] ++ (renderFilterInfoRow isFocused <$> (M.toList . view containedFilters $ map))

renderFilterInfoRow :: (Ord n, Show n) => Bool -> (MarionetteMsgAddresses, List n Performer) -> Widget n
renderFilterInfoRow isFocused (addr, ls) = vBox [str $ show addr
                                                , renderList renderAddrInfo isFocused ls
                                                ]
