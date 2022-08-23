{- |
Module      :  VMCMixer.UI.Brick
Description :  Brick frontend codes
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
module VMCMixer.UI.Brick where

import Brick 
import Brick.BChan (BChan, writeBChan)
import Brick.Focus (FocusRing, focusNext, focusPrev, focusRing, withFocusRing, focusGetCurrent)
import Brick.Widgets.Core (str, (<+>))
import Brick.Widgets.List (renderList, List, list, handleListEvent, listInsert, listRemove, listSelected, listSelectedElement)
import Brick.Widgets.Edit (editor, Editor, handleEditorEvent, renderEditor, getEditContents, applyEdit)
import Brick.Widgets.Border (border, borderAttr)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import qualified Data.Text.Zipper as Z
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes.Color as Color
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (%~), (.~), set)
import Lens.Micro.Extras (view)
import Network.Socket (Socket)

import VMCMixer.UI.Brick.Attr
import VMCMixer.UI.Brick.Event
import VMCMixer.Parser (parsePerformer)
import VMCMixer.Types (Performer, Marionette, performerPort)

data Name = InputStreams | NewAddrEditor deriving (Ord, Eq, Show)

data AppState = AppState { _inputStreams :: List Name Performer
                         , _inputStreamSockets :: V.Vector Socket
                         , _newAddrEditor :: Editor String Name
                         , _focus :: FocusRing Name
                         , _uiEventEmitter :: BChan VMCMixerUIEvent
                         }
makeLenses ''AppState

renderAddrInfo :: Bool -> Performer -> Widget Name
renderAddrInfo isFocused = str . show . view performerPort
  where

-- | Draw 'Widget' in border, but with focus-aware attribute
--
-- This function is intented to used with 'withFocusRing'.
withFocusedBorder :: (Bool -> a -> Widget Name) -> Bool -> a -> Widget Name
withFocusedBorder renderer isFocused st  = _border $ renderer isFocused st
  where
    _border = if isFocused
              then overrideAttr borderAttr borderFocusedAttr . border
              else border

ui :: AppState -> [Widget Name]
ui s = [vBox [ withFocusRing (s^.focus) (withFocusedBorder $ renderList renderAddrInfo)    (s^.inputStreams)
             , withFocusRing (s^.focus) (withFocusedBorder $ renderEditor (str . unlines)) (s^.newAddrEditor)]]

eHandler :: AppState -> BrickEvent Name VMCMixerUIEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar '-') [])) =
  case (listSelectedElement $ s^.inputStreams) of
    Nothing -> continue s
    Just (idx, port) -> do
      liftIO $ writeBChan (s^.uiEventEmitter) (RemoveAddr port)
      continue $ s&inputStreams%~(\l -> maybe l (\idx -> listRemove idx l) (listSelected l))
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar '\t') []))  = continue $ s&focus%~focusNext
eHandler s (VtyEvent (Vty.EvKey Vty.KBackTab []))     = continue $ s&focus%~focusPrev
eHandler s (VtyEvent ev) = continue =<< case (focusGetCurrent (s^.focus)) of
                                          (Just InputStreams ) -> handleEventLensed s inputStreams handleListEvent ev
                                          (Just NewAddrEditor) -> handleEditorEvent' ev s
                                          Nothing -> return s
eHandler s _ = continue s

-- | Almost same as 'handleEditorEvent', but 'Vty.KEnter' will submit it to list.
handleEditorEvent' :: Vty.Event -> AppState -> EventM Name AppState
handleEditorEvent' (Vty.EvKey Vty.KEnter []) s = 
  let ed = s^.newAddrEditor
      l  = s^.inputStreams
      ed' = applyEdit Z.clearZipper ed
  in case (parsePerformer . head $ getEditContents ed) of
       (Left err) -> return s -- TODO: Display error message on UI.
       (Right marionette) -> do
         let l' = listInsert 0 marionette l
         liftIO $ writeBChan (s^.uiEventEmitter) (NewAddr marionette)
         return $ s&(newAddrEditor.~ed').(inputStreams.~l')

handleEditorEvent' ev s = handleEditorEvent ev (s^.newAddrEditor) >>= return . flip (set newAddrEditor) s

app :: App AppState VMCMixerUIEvent Name
app = App { appDraw = ui
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appAttrMap      = const vmcmAttrmap
          }

initialState :: BChan VMCMixerUIEvent -> [Performer] -> AppState
initialState evEmitterCh initialInputs = AppState (list InputStreams (V.fromList initialInputs) 2)
               (V.empty) (editor NewAddrEditor (Just 1) "") (focusRing [InputStreams, NewAddrEditor]) evEmitterCh
