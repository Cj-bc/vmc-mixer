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
import Brick.Types (Widget, Padding(Pad))
import Brick.Widgets.List (renderList, List)
import qualified Data.HashMap.Strict as HMap
import VMCMixer.Types (MarionetteMsgAddresses, Performer, performerName, performerPort)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Brick (AttrName)

-- {{{ Zipper implementation
import Lens.Micro (to, SimpleGetter)

-- | Simple zipper
data Zipper a = Zipper { _peeked :: a
                       , _before :: [a]
                       , _after  :: [a]
                       }
makeLenses  ''Zipper

next :: SimpleGetter (Zipper a) (Maybe (Zipper a))
next = let getter (Zipper c bs []) = Nothing
           getter (Zipper c bs (a:as)) = Just $ Zipper a (c:bs) as
  in to getter

-- | Return Zipper that peeks previous value
--
--  view (next . previous) == id
--  view (previous . next) == id
previous :: SimpleGetter (Zipper a) (Maybe (Zipper a))
previous = let getter (Zipper c [] as)     = Nothing
               getter (Zipper c (b:bs) as) = Just $ Zipper b bs (c:as)
  in to getter

-- }}}

data FilterDisplay n = FilterDisplay { _displayName :: n
                                     , _containedFilters :: Zipper (MarionetteMsgAddresses, List n Performer)
                                     , _fallbackFilter :: (n, Performer)
                                     }
makeLenses ''FilterDisplay

filterDisplay :: n -> [(MarionetteMsgAddresses, List n Performer)] -> (n, Performer) -> FilterDisplay n
filterDisplay n (peeked':rest) fallback = FilterDisplay n (Zipper peeked' [] rest) fallback

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
  let filters = mconcat [(False,) <$> map^.containedFilters.before
                        , [(True, map^.containedFilters.peeked)]
                        , (False,) <$> map^.containedFilters.after
                        ]
  in vBox $ [renderFallback map, hBorder] ++ (uncurry renderFilterInfoRow <$> filters)

renderFilterInfoRow :: (Ord n, Show n) => Bool -> (MarionetteMsgAddresses, List n Performer) -> Widget n
renderFilterInfoRow isFocused (addr, ls) = withAttr atr $ vBox [str $ show addr
                                                              , padLeft (Pad 2) $ renderList renderAddrInfo isFocused ls
                                                              ]
  where
    atr = if isFocused then filterDisplayPeekedAttr else filterDisplayAttr

-- | The top-level attribute used for entire filterDisplay widget
filterDisplayAttr :: AttrName
filterDisplayAttr = "filterDisplay"

-- | The attribute used for 'Filter' that is currently peeked
filterDisplayPeekedAttr :: AttrName
filterDisplayPeekedAttr = filterDisplayAttr <> "Peeked"
