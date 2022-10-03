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
import Control.Monad (fail, when, guard)
import Data.Attoparsec.Text as AT
import Data.UnityEditor (HumanBodyBones(..))
import Control.Applicative ((<|>))
import qualified Data.Text as T
import VMCMixer.Types (Performer(..), Marionette(..))
import Lens.Micro ((%~), _1, _2)
import Data.Char (isSpace)

data HostName = IPAddress Int Int Int Int
              | DomainName T.Text
              | Localhost deriving (Eq)

instance Show HostName where
  show (IPAddress x y z w) = mconcat [show x, ".", show y, ".", show z, ".", show w]
  show (DomainName s)      = T.unpack s
  show Localhost           = "localhost"

parseAddress :: String -> Either String (String, Int)
parseAddress s = eitherResult $ parse addressWithPort (T.pack s) `feed` ""

parsePerformer :: String -> Either String Performer
parsePerformer s = eitherResult $ parse performer (T.pack s) `feed` ""

parseMarionette :: String -> Either String Marionette
parseMarionette s = eitherResult $ parse marionette (T.pack s) `feed` ""

-- | Non standard ports are required.
parsePort :: String -> Either String Int
parsePort s = eitherResult $ parse validPortNumber (T.pack s) `feed` ""

performer :: Parser Performer
performer = do
    name <- option Nothing (try nameField)
    port <- validPortNumber
    return $ Performer port name
  where
    nameField = do
      name <- takeTill (== ',')
      char ','
      skipSpace
      return $ Just name

marionette :: Parser Marionette
marionette = do
  name <- option Nothing (try nameField)
  (addr, port) <- addressWithPort
  return $ Marionette addr port name
  where
    nameField = do
      name <- takeTill (== ',')
      char ','
      skipSpace
      return $ Just name

-- | Int parser with port number range validation.
--
-- port number range is defined by RFC6335.
-- Although it's not encoradged to use 0-1023 which is defined as "System ports",
-- I decided not to limit it as it's not forced.
--
-- https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
-- https://www.rfc-editor.org/rfc/rfc6335.html#section-6
validPortNumber :: Parser Int
validPortNumber = do
      num <- decimal
      when (num < 0 || 65535 < num) $ fail "number isn't in valid range (0~65535)"
      return num

addressWithPort :: Parser (String, Int)
addressWithPort = do
  n <- hostName
  char ':'
  p <- validPortNumber
  return (show n, p)

-- | Parser of valid hostnames
--
-- /This currently doesn't work because "domainName" accepts any string./
hostName :: Parser HostName
hostName = ipAddress <|> localhost <|> domainName 

ipAddress :: Parser HostName
ipAddress = IPAddress <$> decimal <*  string "." <*> decimal <* string "." <*> decimal <* string "." <*> decimal

domainName :: Parser HostName
domainName = DomainName <$> AT.takeWhile (/= ':')

localhost :: Parser HostName
localhost = Localhost <$ string "localhost"

humanBodyBones :: Parser HumanBodyBones
humanBodyBones = choice $ fmap f allBones
  where
    f bone = asciiCI (T.pack $ show bone) >> return bone
    allBones = [ Hips
               , LeftUpperLeg, RightUpperLeg
               , LeftLowerLeg, RightLowerLeg
               , LeftFoot, RightFoot
               , Spine
               , Chest
               , UpperChest
               , Neck
               , Head
               , LeftShoulder, RightShoulder
               , LeftUpperArm, RightUpperArm
               , LeftLowerArm, RightLowerArm
               , LeftHand, RightHand
               , LeftToes, RightToes
               , LeftEye, RightEye
               , Jaw
               , LeftThumbProximal
               , LeftThumbIntermediate
               , LeftThumbDistal
               , LeftIndexProximal
               , LeftIndexIntermediate
               , LeftIndexDistal
               , LeftMiddleProximal
               , LeftMiddleIntermediate
               , LeftMiddleDistal
               , LeftRingProximal
               , LeftRingIntermediate
               , LeftRingDistal
               , LeftLittleProximal
               , LeftLittleIntermediate
               , LeftLittleDistal
               , RightThumbProximal
               , RightThumbIntermediate
               , RightThumbDistal
               , RightIndexProximal
               , RightIndexIntermediate
               , RightIndexDistal
               , RightMiddleProximal
               , RightMiddleIntermediate
               , RightMiddleDistal
               , RightRingProximal
               , RightRingIntermediate
               , RightRingDistal
               , RightLittleProximal
               , RightLittleIntermediate
               , RightLittleDistal
               , LastBone
               ]

blendShapeExpression :: Parser BlendShapeExpression
blendShapeExpression = choice $ fmap f allExps ++ [Custom . T.pack <$> many1 letter]
  where
    f exp = do
      asciiCI (T.pack $ show exp)
      nextChar <- peekChar 
      -- make sure shorter name doesn't eat longer name: e.g. prevent 'Angry' from recognized as 'A'
      guard $ maybe True isSpace nextChar
      return exp

    allExps =  [Neutral
               , A, Data.VRM.I, U, E, O
               , Blink
               , Joy, Angry, Sorrow, Fun
               , LookUp, LookDown
               , LookLeft, LookRight
               , BlinkL, BlinkR
               ]
