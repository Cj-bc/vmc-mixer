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

data Performer = Performer { _incomingPort :: PortNumber
                           } deriving (Show)
makeLenses ''Performer

data Marionette = Marionette { _address :: String
                             , _port :: PortNumber
                             } deriving (Show)
makeLenses ''Marionette
