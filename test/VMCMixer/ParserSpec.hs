module VMCMixer.ParserSpec where

import VMCMixer.Parser
import VMCMixer.Types (Performer(Performer), Marionette(Marionette))
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.UnityEditor as UE
import qualified Data.VRM as VRM

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

    it "should accept 'PORT_NUMBER' format" $
      performer `tryParse` (T.pack "300") == Just (Performer 300 Nothing)

  describe "marionette" $ do
    it "should accept 'NAME, ADDRESS:PORT_NUMBER' format" $
      marionette `tryParse` (T.pack "foo, foobar.org:300") == Just (Marionette "foobar.org" 300 (Just $ T.pack "foo"))

    it "should accept 'ADDRESS:PORT_NUMBER' format" $
      marionette `tryParse` (T.pack "foobar.org:300") == Just (Marionette "foobar.org" 300 Nothing)

  describe "humanBodyBones" $ do
    it "should parse all valid names" $ do
      humanBodyBones `tryParse` (T.pack "Hips")                    `shouldBe` Just UE.Hips
      humanBodyBones `tryParse` (T.pack "LeftUpperLeg")            `shouldBe` Just UE.LeftUpperLeg
      humanBodyBones `tryParse` (T.pack "RightUpperLeg")           `shouldBe` Just UE.RightUpperLeg
      humanBodyBones `tryParse` (T.pack "LeftFoot")                `shouldBe` Just UE.LeftFoot
      humanBodyBones `tryParse` (T.pack "RightFoot")               `shouldBe` Just UE.RightFoot
      humanBodyBones `tryParse` (T.pack "Spine")                   `shouldBe` Just UE.Spine
      humanBodyBones `tryParse` (T.pack "Chest")                   `shouldBe` Just UE.Chest
      humanBodyBones `tryParse` (T.pack "UpperChest")              `shouldBe` Just UE.UpperChest
      humanBodyBones `tryParse` (T.pack "Neck")                    `shouldBe` Just UE.Neck
      humanBodyBones `tryParse` (T.pack "LeftShoulder")            `shouldBe` Just UE.LeftShoulder
      humanBodyBones `tryParse` (T.pack "RightShoulder")           `shouldBe` Just UE.RightShoulder
      humanBodyBones `tryParse` (T.pack "LeftUpperArm")            `shouldBe` Just UE.LeftUpperArm
      humanBodyBones `tryParse` (T.pack "RightUpperArm")           `shouldBe` Just UE.RightUpperArm
      humanBodyBones `tryParse` (T.pack "LeftLowerArm")            `shouldBe` Just UE.LeftLowerArm
      humanBodyBones `tryParse` (T.pack "RightLowerArm")           `shouldBe` Just UE.RightLowerArm
      humanBodyBones `tryParse` (T.pack "LeftHand")                `shouldBe` Just UE.LeftHand
      humanBodyBones `tryParse` (T.pack "RightHand")               `shouldBe` Just UE.RightHand
      humanBodyBones `tryParse` (T.pack "LeftToes")                `shouldBe` Just UE.LeftToes
      humanBodyBones `tryParse` (T.pack "RightToes")               `shouldBe` Just UE.RightToes
      humanBodyBones `tryParse` (T.pack "LeftIndexProximal")       `shouldBe` Just UE.LeftIndexProximal
      humanBodyBones `tryParse` (T.pack "LeftIndexIntermediate")   `shouldBe` Just UE.LeftIndexIntermediate
      humanBodyBones `tryParse` (T.pack "LeftIndexDistal")         `shouldBe` Just UE.LeftIndexDistal
      humanBodyBones `tryParse` (T.pack "LeftMiddleProximal")      `shouldBe` Just UE.LeftMiddleProximal
      humanBodyBones `tryParse` (T.pack "LeftMiddleIntermediate")  `shouldBe` Just UE.LeftMiddleIntermediate
      humanBodyBones `tryParse` (T.pack "LeftMiddleDistal")        `shouldBe` Just UE.LeftMiddleDistal
      humanBodyBones `tryParse` (T.pack "LeftRingProximal")        `shouldBe` Just UE.LeftRingProximal
      humanBodyBones `tryParse` (T.pack "LeftRingIntermediate")    `shouldBe` Just UE.LeftRingIntermediate
      humanBodyBones `tryParse` (T.pack "LeftRingDistal")          `shouldBe` Just UE.LeftRingDistal
      humanBodyBones `tryParse` (T.pack "LeftLittleProximal")      `shouldBe` Just UE.LeftLittleProximal
      humanBodyBones `tryParse` (T.pack "LeftLittleIntermediate")  `shouldBe` Just UE.LeftLittleIntermediate
      humanBodyBones `tryParse` (T.pack "LeftLittleDistal")        `shouldBe` Just UE.LeftLittleDistal
      humanBodyBones `tryParse` (T.pack "RightThumbProximal")      `shouldBe` Just UE.RightThumbProximal
      humanBodyBones `tryParse` (T.pack "RightThumbIntermediate")  `shouldBe` Just UE.RightThumbIntermediate
      humanBodyBones `tryParse` (T.pack "RightThumbDistal")        `shouldBe` Just UE.RightThumbDistal
      humanBodyBones `tryParse` (T.pack "RightIndexProximal")      `shouldBe` Just UE.RightIndexProximal
      humanBodyBones `tryParse` (T.pack "RightIndexIntermediate")  `shouldBe` Just UE.RightIndexIntermediate
      humanBodyBones `tryParse` (T.pack "RightIndexDistal")        `shouldBe` Just UE.RightIndexDistal
      humanBodyBones `tryParse` (T.pack "RightMiddleProximal")     `shouldBe` Just UE.RightMiddleProximal
      humanBodyBones `tryParse` (T.pack "RightMiddleIntermediate") `shouldBe` Just UE.RightMiddleIntermediate
      humanBodyBones `tryParse` (T.pack "RightMiddleDistal")       `shouldBe` Just UE.RightMiddleDistal
      humanBodyBones `tryParse` (T.pack "RightRingProximal")       `shouldBe` Just UE.RightRingProximal
      humanBodyBones `tryParse` (T.pack "RightRingIntermediate")   `shouldBe` Just UE.RightRingIntermediate
      humanBodyBones `tryParse` (T.pack "RightRingDistal")         `shouldBe` Just UE.RightRingDistal
      humanBodyBones `tryParse` (T.pack "RightLittleProximal")     `shouldBe` Just UE.RightLittleProximal
      humanBodyBones `tryParse` (T.pack "RightLittleIntermediate") `shouldBe` Just UE.RightLittleIntermediate
      humanBodyBones `tryParse` (T.pack "RightLittleDistal")       `shouldBe` Just UE.RightLittleDistal
      humanBodyBones `tryParse` (T.pack "LastBone")                `shouldBe` Just UE.LastBone

    it "should be case-insensitive" $
      humanBodyBones `tryParse` (T.pack "NeCK") == humanBodyBones `tryParse` (T.pack "Neck")

  describe "blendShapeExpression" $ do
    it "should parse all valid names" $ do
      blendShapeExpression `tryParse` (T.pack "Neutral")   `shouldBe` Just VRM.Neutral
      blendShapeExpression `tryParse` (T.pack "A")         `shouldBe` Just VRM.A
      blendShapeExpression `tryParse` (T.pack "I")         `shouldBe` Just VRM.I
      blendShapeExpression `tryParse` (T.pack "U")         `shouldBe` Just VRM.U
      blendShapeExpression `tryParse` (T.pack "E")         `shouldBe` Just VRM.E
      blendShapeExpression `tryParse` (T.pack "O")         `shouldBe` Just VRM.O
      blendShapeExpression `tryParse` (T.pack "Blink")     `shouldBe` Just VRM.Blink
      blendShapeExpression `tryParse` (T.pack "Joy")       `shouldBe` Just VRM.Joy
      blendShapeExpression `tryParse` (T.pack "Angry")     `shouldBe` Just VRM.Angry
      blendShapeExpression `tryParse` (T.pack "Sorrow")    `shouldBe` Just VRM.Sorrow
      blendShapeExpression `tryParse` (T.pack "Fun")       `shouldBe` Just VRM.Fun
      blendShapeExpression `tryParse` (T.pack "LookUp")    `shouldBe` Just VRM.LookUp
      blendShapeExpression `tryParse` (T.pack "LookDown")  `shouldBe` Just VRM.LookDown
      blendShapeExpression `tryParse` (T.pack "LookLeft")  `shouldBe` Just VRM.LookLeft
      blendShapeExpression `tryParse` (T.pack "LookRight") `shouldBe` Just VRM.LookRight
      blendShapeExpression `tryParse` (T.pack "Blink_L")   `shouldBe` Just VRM.BlinkL
      blendShapeExpression `tryParse` (T.pack "Blink_R")   `shouldBe` Just VRM.BlinkR
    
    it "should be case-insensitive" $
      blendShapeExpression `tryParse` (T.pack "neuTrAl") == blendShapeExpression `tryParse` (T.pack "Neutral")
    
