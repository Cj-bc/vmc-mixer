{- |
Module      :  VMCMixer.UI.Brick.Attr
Description :  Attributes for this brick application
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

Contains all 'Attr's used in VMCMixer brick UI.
-}
{-# LANGUAGE OverloadedStrings #-}
module VMCMixer.UI.Brick.Attr where

import Brick (AttrName, AttrMap, attrMap)
import Brick.Widgets.List (listSelectedAttr)
import Brick.Widgets.Border (borderAttr)
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes.Color as Color
import VMCMixer.UI.Brick.Widgets.FilterDisplay (filterDisplayPeekedAttr, filterDisplayAttr)

-- | Default 'AttrMap' for vmc-mixer application.
vmcmAttrmap :: AttrMap
vmcmAttrmap = attrMap Vty.defAttr
  [(listSelectedAttr, Vty.withBackColor Vty.defAttr Color.black)
  ,(borderFocusedAttr, Vty.withForeColor Vty.defAttr Color.blue)
  ,(filterDisplayAttr, Vty.withBackColor  Vty.defAttr Color.red)
  ,(filterDisplayPeekedAttr, Vty.withBackColor Vty.defAttr Color.brightBlue)
  ]

-- | Used for borders of Widget that is currently focused on.
borderFocusedAttr :: AttrName
borderFocusedAttr = borderAttr<>"focused"
