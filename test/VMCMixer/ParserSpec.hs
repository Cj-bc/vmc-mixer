module VMCMixer.ParserSpec where

import VMCMixer.Parser
import VMCMixer.Types (Performer(Performer), Marionette(Marionette))
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Attoparsec.Text
import qualified Data.Text as T

tryParse :: Parser a -> T.Text -> Maybe a
tryParse parser txt = maybeResult (parse parser txt `feed` (T.pack ""))

spec = do
  describe "ipAddress" $ do
    it "should pass valid ip address" $
      ipAddress `tryParse` (T.pack "192.168.10.0") == Just (IPAddress 192 168 10 0)

    it "should validate if each portion of address is numeric" $
      ipAddress `tryParse` (T.pack "inv.alid.ip.address") == Nothing

    it "should validate if each portion of address is in valid range(0~255)" $
      pendingWith "Not implemented yet"

  describe "domainName" $ do
    it "should pass valid domain name" $
      domainName `tryParse` (T.pack "foobar.org:") == Just (DomainName (T.pack "foobar.org"))

    it "should validate domain name" $
      pendingWith "Not implemented yet"

  describe "localhost" $ do
    it "accepts string 'localhost'" $
      localhost `tryParse` (T.pack "localhost") == Just Localhost

    it "should not accept others" $
      localhost `tryParse` (T.pack "invalid") == Nothing

  describe "hostName" $ do
    it "accepts IP Address" $
      hostName `tryParse` (T.pack "192.168.10.0") == Just (IPAddress 192 168 10 0)

    it "accepts 'localhost'" $
      hostName `tryParse` (T.pack "localhost") == Just Localhost

    it "accepts domain name" $
      hostName `tryParse` (T.pack "foobar.org") == Just (DomainName (T.pack "foobar.org"))

  describe "validPortNumber" $ do
    it "should accept minimum port number 0" $
      validPortNumber `tryParse` (T.pack "0") == Just 0

    it "shuold accept maximum port number 65535" $
      validPortNumber `tryParse` (T.pack "65535") == Just 65535

    it "should not accept port number bigger than 65535" $
      validPortNumber `tryParse` (T.pack "65536") == Nothing

  describe "addressWithPort" $ do
    it "should accept 'ADDRESS:PORT_NUMBER' format" $
      addressWithPort `tryParse` (T.pack "foobar.org:300") == Just ("foobar.org", 300)

    it "should assume 'ADDRESS' is 'localhost' when 'ADDRESS' isn't provided (':PORT_NUMBER')" $
      pendingWith "Not implemented yet"
      -- addressWithPort `tryParse` (T.pack ":300") == Just ("localhost", 300)

    it "should not accept lack of 'PORT_NUMBER'" $
      addressWithPort `tryParse` (T.pack "foobar.org") == Nothing
      
    it "should not accept lack of 'PORT_NUMBER' (2)" $
      addressWithPort `tryParse` (T.pack "foobar.org:") == Nothing

  describe "performer" $ do
    it "should accept 'NAME, PORT_NUMBER' format" $
      performer `tryParse` (T.pack "foo, 300") == Just (Performer 300 (Just $ T.pack "foo"))

  describe "marionette" $ do
    it "should accept 'NAME, ADDRESS:PORT_NUMBER' format" $
      marionette `tryParse` (T.pack "foo, foobar.org:300") == Just (Marionette "foobar.org" 300 (Just $ T.pack "foo"))
