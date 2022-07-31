{- |
Module      :  VMCMixer.Parser
Description :  Parsers used in VMCMixer
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
{-# LANGUAGE OverloadedStrings #-}
module VMCMixer.Parser where
import Data.Attoparsec.Text as AT
import Control.Applicative ((<|>))
import qualified Data.Text as T

data HostName = IPAddress Int Int Int Int
              | DomainName T.Text
              | Localhost

instance Show HostName where
  show (IPAddress x y z w) = mconcat [show x, ".", show y, ".", show z, ".", show w]
  show (DomainName s)      = T.unpack s
  show Localhost           = "localhost"

parseAddress :: String -> Either String (String, Int)
parseAddress s = eitherResult $ parse addressWithPort (T.pack s) `feed` ""

addressWithPort :: Parser (String, Int)
addressWithPort = do
  n <- hostName
  char ':'
  p <- decimal
  return (show n, p)

hostName :: Parser HostName
hostName = ipAddress <|> localhost <|> domainName 

ipAddress :: Parser HostName
ipAddress = IPAddress <$> decimal <*  string "." <*> decimal <* string "." <*> decimal <* string "." <*> decimal

domainName :: Parser HostName
domainName = DomainName <$> AT.takeWhile (/= ':')

localhost :: Parser HostName
localhost = Localhost <$ string "localhost"
