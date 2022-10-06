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
import VMCMixer.Parser (parseMarionette, parsePerformer, completeFilterRow, parseFilter)
import VMCMixer.Types (Performer, Marionette, MarionetteMsgAddresses)
import Lens.Micro.TH (makeLenses)
import qualified Data.Vector as V

-- | vmc-mixer's command line options
data Option = Option { _performers :: [Performer]
                       -- ^ List of input ports
                     , _marionette :: Marionette
                       -- ^ Output address
                     , _filterOpt :: [(MarionetteMsgAddresses, V.Vector Performer)]
                     } deriving (Show)

makeLenses ''Option

-- | Parse command line option and returns its result
-- as 'Option' data.
getOption :: IO Option
getOption = execParser $ info vmcmixerOpts fullDesc

-- | Parser for vmc-mixer's all options
vmcmixerOpts :: Parser Option
vmcmixerOpts = let performers = many performerList
               in Option <$> performers
                  <*> argument (eitherReader parseMarionette) (metavar "marionette")
                  <*> (completeFilterRow <$> performers <*> many filterRow)

-- | Small parser for inputAddress
performerList :: Parser Performer
performerList = option (eitherReader parsePerformer)
                (long "performer" <> short 'p')

filterRow :: Parser (MarionetteMsgAddresses, V.Vector (Either Int String))
filterRow = option (eitherReader parseFilter)
             (long "filter" <> short 'f')
