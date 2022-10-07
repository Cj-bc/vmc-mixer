{- |
Module      :  VMCMixer.Options.Internal
Description :  Internal datatypes/functions for 'VMCMixer.Options'
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
{-# LANGUAGE  TemplateHaskell #-}
module VMCMixer.Options.Internal where
import Options.Applicative
import VMCMixer.Parser (parseMarionette, parsePerformer, completeFilterRow, parseFilter)
import VMCMixer.Types (Performer, Marionette, MarionetteMsgAddresses)
import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)

-- | vmc-mixer's command line options
--
-- Users mainly need to use this intead of 'PartialOption'
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
getOption = fmap fillPartial . execParser $ info vmcmixerOpts fullDesc

-- | [Internal] 
--
-- This exists just because I couldn't convret '_partFilterOpt' with list of 'Performer's
data PartialOption = PartOpt { _partPerformers :: [Performer]
                               -- ^ List of input ports
                             , _partMarionette :: Marionette
                             -- ^ Output address
                             , _partFilterOpt :: [(MarionetteMsgAddresses, V.Vector (Either Int String))]
                             -- ^ List of filter rows but need some modification
                             }

-- | [Internal] Create 'Option' from 'PartialOption'
--
-- It only converts filter row
fillPartial :: PartialOption -> Option
fillPartial popt = let performers = _partPerformers popt
                       marionette = _partMarionette popt
                       filterRows = _partFilterOpt popt
                   in Option performers marionette (completeFilterRow performers  filterRows)

-- | Parser for vmc-mixer's all options
--
-- This isn't intended to be used by user.
-- Use 'getOption' to get 'Option'
vmcmixerOpts :: Parser PartialOption
vmcmixerOpts = PartOpt <$> many performerList
               <*> argument (eitherReader parseMarionette) (metavar "marionette")
               <*> many filterRow

-- | Small parser for inputAddress
performerList :: Parser Performer
performerList = option (eitherReader parsePerformer)
                (long "performer" <> short 'p')

filterRow :: Parser (MarionetteMsgAddresses, V.Vector (Either Int String))
filterRow = option (eitherReader parseFilter)
             (long "filter" <> short 'f')
