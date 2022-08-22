{- |
Module      :  VMCMixer.UI.Brick.Event
Description :  Event definition and related functions for brick UI
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

module VMCMixer.UI.Brick.Event where

-- | Events that will be emitted by UI manipulation.
data VMCMixerUIEvent = NewAddr Int -- ^ Emitted when new address is added to the list
                     | RemoveAddr Int -- ^ Emitted when any address is removed from the list

-- | Events that will be emitted by external thread to notify its work progress.
data BrickUIResponseEvent = Completed
