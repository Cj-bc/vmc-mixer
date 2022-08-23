{- |
Module      :  VMCMixer.Options
Description :  Command line options for vmc-mixer
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
It's recommended to do qualified import,
as some lens have common name.
-}
{-# LANGUAGE TemplateHaskell #-}
module VMCMixer.Options where
import Options.Applicative
import VMCMixer.Parser (parseMarionette, parsePerformer)
import VMCMixer.Types (Performer, Marionette)
import Lens.Micro.TH (makeLenses)

-- | vmc-mixer's command line options
data Option = Option { _performers :: [Performer]
                       -- ^ List of input ports
                     , _marionette :: Marionette
                       -- ^ Output address
                     } deriving (Show)

makeLenses ''Option

-- | Parse command line option and returns its result
-- as 'Option' data.
getOption :: IO Option
getOption = execParser $ info vmcmixerOpts fullDesc

-- | Parser for vmc-mixer's all options
vmcmixerOpts :: Parser Option
vmcmixerOpts = Option <$> (many inputPortList)
                      <*> argument (eitherReader parseMarionette) (metavar "output")

-- | Small parser for inputAddress
inputPortList :: Parser Performer
inputPortList = option (eitherReader parsePerformer)
                (long "inputs" <> short 'i')

