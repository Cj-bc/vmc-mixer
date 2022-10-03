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
    it "should parse all valid names" $ foldl1 (&&)
      [ humanBodyBones `tryParse` (T.pack "Hips")                    == Just UE.Hips
      , humanBodyBones `tryParse` (T.pack "LeftUpperLeg")            == Just UE.LeftUpperLeg
      , humanBodyBones `tryParse` (T.pack "RightUpperLeg")           == Just UE.RightUpperLeg
      , humanBodyBones `tryParse` (T.pack "LeftFoot")                == Just UE.LeftFoot
      , humanBodyBones `tryParse` (T.pack "RightFoot")               == Just UE.RightFoot
      , humanBodyBones `tryParse` (T.pack "Spine")                   == Just UE.Spine
      , humanBodyBones `tryParse` (T.pack "Chest")                   == Just UE.Chest
      , humanBodyBones `tryParse` (T.pack "UpperChest")              == Just UE.UpperChest
      , humanBodyBones `tryParse` (T.pack "Neck")                    == Just UE.Neck
      , humanBodyBones `tryParse` (T.pack "LeftShoulder")            == Just UE.LeftShoulder
      , humanBodyBones `tryParse` (T.pack "RightShoulder")           == Just UE.RightShoulder
      , humanBodyBones `tryParse` (T.pack "LeftUpperArm")            == Just UE.LeftUpperArm
      , humanBodyBones `tryParse` (T.pack "RightUpperArm")           == Just UE.RightUpperArm
      , humanBodyBones `tryParse` (T.pack "LeftLowerArm")            == Just UE.LeftLowerArm
      , humanBodyBones `tryParse` (T.pack "RightLowerArm")           == Just UE.RightLowerArm
      , humanBodyBones `tryParse` (T.pack "LeftHand")                == Just UE.LeftHand
      , humanBodyBones `tryParse` (T.pack "RightHand")               == Just UE.RightHand
      , humanBodyBones `tryParse` (T.pack "LeftToes")                == Just UE.LeftToes
      , humanBodyBones `tryParse` (T.pack "RightToes")               == Just UE.RightToes
      , humanBodyBones `tryParse` (T.pack "LeftIndexProximal")       == Just UE.LeftIndexProximal
      , humanBodyBones `tryParse` (T.pack "LeftIndexIntermediate")   == Just UE.LeftIndexIntermediate
      , humanBodyBones `tryParse` (T.pack "LeftIndexDistal")         == Just UE.LeftIndexDistal
      , humanBodyBones `tryParse` (T.pack "LeftMiddleProximal")      == Just UE.LeftMiddleProximal
      , humanBodyBones `tryParse` (T.pack "LeftMiddleIntermediate")  == Just UE.LeftMiddleIntermediate
      , humanBodyBones `tryParse` (T.pack "LeftMiddleDistal")        == Just UE.LeftMiddleDistal
      , humanBodyBones `tryParse` (T.pack "LeftRingProximal")        == Just UE.LeftRingProximal
      , humanBodyBones `tryParse` (T.pack "LeftRingIntermediate")    == Just UE.LeftRingIntermediate
      , humanBodyBones `tryParse` (T.pack "LeftRingDistal")          == Just UE.LeftRingDistal
      , humanBodyBones `tryParse` (T.pack "LeftLittleProximal")      == Just UE.LeftLittleProximal
      , humanBodyBones `tryParse` (T.pack "LeftLittleIntermediate")  == Just UE.LeftLittleIntermediate
      , humanBodyBones `tryParse` (T.pack "LeftLittleDistal")        == Just UE.LeftLittleDistal
      , humanBodyBones `tryParse` (T.pack "RightThumbProximal")      == Just UE.RightThumbProximal
      , humanBodyBones `tryParse` (T.pack "RightThumbIntermediate")  == Just UE.RightThumbIntermediate
      , humanBodyBones `tryParse` (T.pack "RightThumbDistal")        == Just UE.RightThumbDistal
      , humanBodyBones `tryParse` (T.pack "RightIndexProximal")      == Just UE.RightIndexProximal
      , humanBodyBones `tryParse` (T.pack "RightIndexIntermediate")  == Just UE.RightIndexIntermediate
      , humanBodyBones `tryParse` (T.pack "RightIndexDistal")        == Just UE.RightIndexDistal
      , humanBodyBones `tryParse` (T.pack "RightMiddleProximal")     == Just UE.RightMiddleProximal
      , humanBodyBones `tryParse` (T.pack "RightMiddleIntermediate") == Just UE.RightMiddleIntermediate
      , humanBodyBones `tryParse` (T.pack "RightMiddleDistal")       == Just UE.RightMiddleDistal
      , humanBodyBones `tryParse` (T.pack "RightRingProximal")       == Just UE.RightRingProximal
      , humanBodyBones `tryParse` (T.pack "RightRingIntermediate")   == Just UE.RightRingIntermediate
      , humanBodyBones `tryParse` (T.pack "RightRingDistal")         == Just UE.RightRingDistal
      , humanBodyBones `tryParse` (T.pack "RightLittleProximal")     == Just UE.RightLittleProximal
      , humanBodyBones `tryParse` (T.pack "RightLittleIntermediate") == Just UE.RightLittleIntermediate
      , humanBodyBones `tryParse` (T.pack "RightLittleDistal")       == Just UE.RightLittleDistal
      , humanBodyBones `tryParse` (T.pack "LastBone")                == Just UE.LastBone
      ]

    it "should be case-insensitive" $
      humanBodyBones `tryParse` (T.pack "NeCK") == humanBodyBones `tryParse` (T.pack "Neck")

  describe "blendShapeExpression" $ do
    it "should parse all valid names" $ foldl1 (&&)
      [ blendShapeExpression `tryParse` (T.pack "Neutral")   == Just VRM.Neutral
      , blendShapeExpression `tryParse` (T.pack "A")         == Just VRM.A
      , blendShapeExpression `tryParse` (T.pack "I")         == Just VRM.I
      , blendShapeExpression `tryParse` (T.pack "U")         == Just VRM.U
      , blendShapeExpression `tryParse` (T.pack "E")         == Just VRM.E
      , blendShapeExpression `tryParse` (T.pack "O")         == Just VRM.O
      , blendShapeExpression `TryParse` (T.pack "Blink")     == Just VRM.Blink
      , blendShapeExpression `tryParse` (T.pack "Joy")       == Just VRM.Joy
      , blendShapeExpression `tryParse` (T.pack "Angry")     == Just VRM.Angry
      , blendShapeExpression `tryParse` (T.pack "Sorrow")    == Just VRM.Sorrow
      , blendShapeExpression `tryParse` (T.pack "Fun")       == Just VRM.Fun
      , blendShapeExpression `tryParse` (T.pack "LookUp")    == Just VRM.LookUp
      , blendShapeExpression `tryParse` (T.pack "LookDown")  == Just VRM.LookDown
      , blendShapeExpression `tryParse` (T.pack "LookLeft")  == Just VRM.LookLeft
      , blendShapeExpression `tryParse` (T.pack "LookRight") == Just VRM.LookRight
      , blendShapeExpression `tryParse` (T.pack "BlinkL")    == Just VRM.BlinkL
      , blendShapeExpression `tryParse` (T.pack "BlinkR")    == Just VRM.BlinkR
      ]
    
    it "should be case-insensitive" $
      blendShapeExpression `tryParse` (T.pack "neuTrAl") == blendShapeExpression `tryParse` (T.pack "Neutral")
    
