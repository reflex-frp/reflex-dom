{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Constraint (Dict(..))
import Data.Constraint.Extras
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..), (==>), EqTag(..), ShowTag(..))
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Functor.Misc
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.IORef (IORef)
import Data.Maybe (fromMaybe, fromJust)
import Data.Proxy
import Data.Text (Text)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace)
import Reflex.Dom.Core hiding (fromJSString)
import Reflex.Patch.DMapWithMove
import System.Directory
import System.IO (stderr)
import System.IO.Silently
import System.IO.Temp
import System.Process
import System.Timeout.Lifted
import qualified Test.HUnit as HUnit (assertEqual, assertFailure)
import qualified Test.Hspec as H
import Test.Hspec.WebDriver hiding (runWD)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver (WD)

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.NonElementParentNode

import Control.Arrow (first, second)
import Control.Lens ((^.), preview, _Left, _Right)
import Control.Monad (join, void)
import Data.Bifunctor (bimap)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum((:=>)))
import Data.Functor (($>))
import Data.Functor.Const (Const(Const))
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Misc (ComposeMaybe(ComposeMaybe), Const2(Const2))
import Data.Monoid ((<>))
import Data.Some (Some(This))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Dependent.Map as DMap
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHCJS.DOM.File as File
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.FilePath as FilePath
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

-- TODO Remove orphan
deriving instance MonadRef WD

assertEqual :: (MonadIO m, Eq a, Show a) => String -> a -> a -> m ()
assertEqual a b = liftIO . HUnit.assertEqual a b

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . HUnit.assertFailure

testTimeLimit :: Int
testTimeLimit = 5 * 1000 * 1000

chromeConfig :: Text -> WD.WDConfig
chromeConfig fp = WD.useBrowser (WD.chrome { WD.chromeBinary = Just $ T.unpack fp, WD.chromeOptions = ["--headless"] }) WD.defaultConfig

keyMap :: DMap DKey Identity
keyMap = DMap.fromList
  [ Key_Int ==> 0
  , Key_Char ==> 'A'
  ]

data DKey a where
  Key_Int :: DKey Int
  Key_Char :: DKey Char
  Key_Bool :: DKey Bool

instance ArgDict DKey where
  type ConstraintsFor DKey c = (c Int, c Char, c Bool)
  type ConstraintsFor' DKey c g = (c (g Int), c (g Char), c (g Bool))
  argDict = \case
    Key_Int -> Dict
    Key_Char -> Dict
    Key_Bool -> Dict
  argDict' = undefined -- Missing constraints?

textKey :: DKey a -> Text
textKey = \case
  Key_Int -> "Key_Int"
  Key_Char -> "Key_Char"
  Key_Bool -> "Key_Bool"

deriveGEq ''DKey
deriveGCompare ''DKey
deriveGShow ''DKey

instance ShowTag DKey Identity where
  showTaggedPrec = \case
    Key_Int -> showsPrec
    Key_Char -> showsPrec
    Key_Bool -> showsPrec

instance EqTag DKey Identity where
  eqTagged Key_Int Key_Int = (==)
  eqTagged Key_Char Key_Char = (==)
  eqTagged Key_Bool Key_Bool = (==)

main :: IO ()
main = withSeleniumServer $ \selenium -> do
  browserPath <- liftIO $ T.strip . T.pack <$> readProcess "which" [ "chromium" ] ""
  when (T.null browserPath) $ fail "No browser found"
  let wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
      chromeCaps' = WD.getCaps $ chromeConfig browserPath
  hspec (tests wdConfig [chromeCaps'] selenium) `finally` _selenium_stopServer selenium

tests :: WD.WDConfig -> [Capabilities] -> Selenium -> Spec
tests wdConfig caps _selenium = do
  let session' = sessionWith wdConfig "" . using (map (,"") caps)
      runWD = runWDOptions (WdOptions False)
  describe "Basic testing" $ session' $ do
    it "works without using getMounted/domSnapshot" $ runWD $ do
      testWidget (checkTextInId "id1" "hello world") $ do
        divId "id1" $ text "hello world"

  describe "getMounted basic tests" $ session' $ do
    let
      idTop = "idTop"
      txtTop = "txt_top"
      id1 = "id1"
      id2 = "id2"
      divId1Dom = "<div id=\"id1\">txt_string_1</div>"
      txtString1 = "txt_string_1"
      txtString2 = "txt_string_2"
    it "works for simple static widget" $ runWD $ do
      let
        expected = [[DomMountSnapshot id1 0 Mounted (Just txtString1)]]
      testWidget (checkDomSnapshots expected) $ do
        divId id1 $ do
          captureDomMountSnapshot id1 True
          text txtString1

    it "works for initial widget" $ runWD $ do
      let
        expected = [[DomMountSnapshot id1 0 Mounted (Just txtString1)]]
      testWidget (checkDomSnapshots expected) $ do
        let
          initWidget = divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
        void $ widgetHold initWidget (return () <$ never)

    it "works for replaced widget" $ runWD $ do
      let
        expected = [[DomMountSnapshot id1 0 Mounted (Just txtString1)]]
      testWidget (checkDomSnapshots expected) $ do
        let
          initWidget = el "div" $ text "initWidget text"
          replacedWidget = divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
        pb <- getPostBuild
        void $ widgetHold initWidget (replacedWidget <$ pb)

    it "works for replaced widget, replaced with getPostBuild" $ runWD $ do
      let
        expected = [[DomMountSnapshot id2 0 Mounted (Just txtString2)]]
      testWidget (checkDomSnapshots expected) $ do
        let
          initWidget = divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
          replacedWidget = divId id2 $ do
            captureDomMountSnapshot id2 True
            text txtString2
        pb <- getPostBuild
        void $ widgetHold initWidget (replacedWidget <$ pb)

    it "works for replaced widget, replaced with getMounted" $ runWD $ do
      let
        expected = [[DomMountSnapshot id2 0 Mounted (Just txtString2)]]
      testWidget (checkDomSnapshots expected) $ do
        let
          initWidget = divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
            -- delay 1 =<< getMounted
            -- delay 0 =<< getMounted
            getMounted
            -- return never
          replacedWidget = divId id2 $ do
            captureDomMountSnapshot id2 True
            text txtString2
            return never
        rec
          let ev = switch (current evDyn)
          evDyn <- widgetHold initWidget (replacedWidget <$ ev)
        return ()

    it "Fires once for parent widget" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted Nothing]
          , [DomMountSnapshot id2 0 Mounted (Just txtString2)]
          ]
      testWidget (checkDomSnapshots expected) $ do
        let
          initWidget = divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
          replacedWidget = divId id2 $ do
            captureDomMountSnapshot id2 True
            text txtString2

        divId idTop $ do
          captureDomMountSnapshot idTop False
          pb <- getPostBuild
          void $ widgetHold initWidget (replacedWidget <$ pb)
        return ()

    it "test notReady" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted (Just "")]
          , [DomMountSnapshot id1 0 Mounted (Just txtString1)]
          ]
      testWidget (checkDomSnapshots expected) $ do
        divId idTop $ do
          captureDomMountSnapshot idTop True
          pb <- getPostBuild
          pbDyn <- holdDyn notReady . ffor pb $ \ _ -> divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
          void $ dyn pbDyn

    it "test notReady delayed" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted (Just "")]
          , [DomMountSnapshot id1 0 Mounted (Just txtString1)]
          ]
      testWidget (checkDomSnapshots expected) $ do
        divId idTop $ do
          captureDomMountSnapshot idTop True
          pb <- delay 0.1 =<< getPostBuild
          pbDyn <- holdDyn notReady . ffor pb $ \ _ -> divId id1 $ do
            captureDomMountSnapshot id1 True
            text txtString1
          void $ dyn pbDyn

    it "test notReady in replaced widget" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted (Just divId1Dom)]
          , [DomMountSnapshot id1 0 Mounted (Just txtString1)]
          ]
      testWidget (checkDomSnapshots expected) $ do
        pb <- getPostBuild
        void $ widgetHold blank $ ffor pb $ \_ -> do
          divId idTop $ do
            captureDomMountSnapshot idTop True
            pb <- getPostBuild
            pbDyn <- holdDyn notReady . ffor pb $ \ _ -> divId id1 $ do
              captureDomMountSnapshot id1 True
              text txtString1
            void $ dyn pbDyn

    it "test notReady delayed in replaced widget" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted (Just divId1Dom)]
          , [DomMountSnapshot id1 0 Mounted (Just txtString1)]
          ]
      testWidget (checkDomSnapshots expected) $ do
        pb <- getPostBuild
        void $ widgetHold blank $ ffor pb $ \_ -> do
          divId idTop $ do
            captureDomMountSnapshot idTop True
            pb <- delay 0.1 =<< getPostBuild
            pbDyn <- holdDyn notReady . ffor pb $ \ _ -> divId id1 $ do
              captureDomMountSnapshot id1 True
              text txtString1
            void $ dyn pbDyn

  describe "getMounted in traverseDMapWithKeyWithAdjust" $ session' $ do
    let
      idTop = "idTop"
      initDom = "<button>new thing!</button><div></div>"
    it "check initital dom" $ runWD $ do
      let
        expected = [[DomMountSnapshot idTop 0 Mounted (Just initDom)]]
      testWidget (checkDomSnapshots expected) $ do
        divId idTop $ do
          captureDomMountSnapshot idTop True
          dmapWithAdjust

    it "Add one thing" $ runWD $ do
      let
        expected =
          [ [DomMountSnapshot idTop 0 Mounted (Just initDom)]
          , [DomMountSnapshot (dmapItemId 0) 0 Mounted Nothing]
          ]
        check = do
          WD.click =<< WD.findElem (WD.ByTag "button")
          liftIO $ threadDelay 100000
          (checkDomSnapshots expected)
      testWidget check $ do
        divId idTop $ do
          captureDomMountSnapshot idTop True
          dmapWithAdjust

-- delayedWidget wName = do
--   dumpMount wName
--   pb <- getPostBuild
--   mev <- getMounted
--   ms <- holdDyn Mounting (Mounted <$ mev)
--   performEvent_ $ logIt (wName <> ": postBuild dyn ") <$> tag (current ms) pb
--   performEvent_ $ logIt (wName <> ": getMounted") <$> updated ms
--   pbDyn <- holdDyn notReady . ffor pb $ \ _ -> do
--     text $ wName <> ": postBuild"
--     dumpMount $ wName <> ": pb block"
--   void $ dyn pbDyn
--   delayedPb <- delay 5 pb
--   delayDyn <- holdDyn notReady . ffor delayedPb $ \ _ -> do
--     text $ wName <> ": delayed"
--     dumpMount $ wName <> ": delay block"
--   void $ dyn delayDyn
--   return ms

type DMK = Const2 Int Text
type DMP = PatchDMap DMK Identity
type DMPM = PatchDMapWithMove DMK Identity

dmapItemId :: Int -> Text
dmapItemId i = "thingId" <> (tshow i)

dmapConstToList :: DMap k (Const a) -> [a]
dmapConstToList = map (\ (_ :=> Const v) -> v) . DMap.toList

dmapWithAdjust :: forall js t m. (TestWidget js t m, HasMountStatus t m) => m ()
dmapWithAdjust = do
  newThing <- button "new thing!"
  index :: Dynamic t Int <- count newThing
  let newThings :: Event t DMP
      newThings =
        ffor (tag (current index) newThing) $ \ i ->
          PatchDMap $ DMap.singleton (Const2 i) (ComposeMaybe (Just (Identity (T.pack $ show i))))
  let renderThing :: Const2 Int Text a -> Identity a -> m (Const (Event t DMP) a)
      renderThing k@(Const2 i) (Identity t) = do
        let myId = dmapItemId i
        captureDomMountSnapshot myId False
        divId myId $ case even i of
          False -> text $ "thing odd " <> (T.pack $ show i)
          _ -> void $ text $ "thing even " <> (T.pack $ show i)
        el "div" $ do
          text $ "I'm thing " <> t
          delete <- button "delete!"
          update <- button "update!"
          pure . Const $ PatchDMap . DMap.singleton (Const2 i) . ComposeMaybe
            <$> leftmost [ delete $> Nothing
                         , update $> (Just . Identity $ t <> "z")
                         ]

  rec
    (_, permuteThingsPatches :: Event t (PatchDMap DMK (Const (Event t DMP)))) <- el "div" $
      traverseDMapWithKeyWithAdjust renderThing DMap.empty (newThings <> permuteThings)
    permuteThingsDMap :: Dynamic t (DMap DMK (Const (Event t DMP))) <-
      foldDyn applyAlways DMap.empty permuteThingsPatches
    let activePermutations :: Behavior t (Event t DMP)
        activePermutations = leftmost . dmapConstToList <$> current permuteThingsDMap
        permuteThings :: Event t DMP
        permuteThings = switch activePermutations

  pure ()

----------------------------------------------------------------------------------------------------------
-- Other APIs
tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- |Type representing the current mount status of a DOM structure. Mount status refers to whether the DOM structure is currently within the document tree, not
-- in the document tree, or transitioning.
data MountState
  -- note: order of these constructors is important, because Ord is derived and employed when combining parent and child mount states
  = Unmounted
  -- ^DOM structures have been removed from the document.
  | Mounting
  -- ^DOM structures are not yet installed in the document.
  | Mounted
  -- ^DOM structures are now in the document.
  deriving (Eq, Ord, Show)

data DomMountSnapshot = DomMountSnapshot
  { _domMountSnapshot_idTag :: Text
  , _domMountSnapshot_count :: Int
  , _domMountSnapshot_state :: MountState
  , _domMountSnapshot_contents :: Maybe Text
  }
  deriving (Show, Eq)

domMountSnapshotShowResult :: DomMountSnapshot -> Text
-- domMountSnapshotShowResult (DomMountSnapshot t c s) = tshow (t, c, s)
domMountSnapshotShowResult = tshow

checkDomSnapshots expected = checkTextInId domSnapshotResultsDivId (tshow (fmap (fmap domMountSnapshotShowResult) expected))

domSnapshotResultsDivId = "domSnapshotResultsDivId"

captureDomMountSnapshot
  :: ( HasMountStatus t m, PerformEvent t m, MonadJSM (Performable m)
     , EventWriter t [DomMountSnapshot] m, MonadHold t m,
       MonadFix m) => Text -> Bool -> m ()
captureDomMountSnapshot tag captureInnerHtml = do
  mEv <- numberOccurrences =<< getMounted
  ms <- holdDyn (0, Mounting) ((\(i,_) -> (i, Mounted)) <$> mEv)
  snapshotEv <- performEvent $ ffor (updated ms) $ \(i,m) -> liftJSM $ do
    contents <- if captureInnerHtml
      then do
        doc <- currentDocumentUnchecked
        html <- getInnerHTML =<< getElementByIdUnchecked doc tag
        return (Just $ textFromJSString html)
      else return Nothing
    return [DomMountSnapshot tag i m contents]
  tellEvent snapshotEv

data Selenium = Selenium
  { _selenium_portNumber :: PortNumber
  , _selenium_stopServer :: IO ()
  }

startSeleniumServer :: PortNumber -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  return $ terminateProcess ph

withSeleniumServer :: (Selenium -> IO ()) -> IO ()
withSeleniumServer f = do
  port <- getFreePort
  stopServer <- startSeleniumServer port
  threadDelay $ 1000 * 1000 * 2 -- TODO poll or wait on a a signal to block on
  f $ Selenium
    { _selenium_portNumber = port
    , _selenium_stopServer = stopServer
    }

assertAttr :: WD.Element -> Text -> Maybe Text -> WD ()
assertAttr e k v = liftIO . assertEqual "Incorrect attribute value" v =<< WD.attr e k

elementShouldBeRemoved :: WD.Element -> WD ()
elementShouldBeRemoved e = do
  try (WD.getText e) >>= \case
    Left (WD.FailedCommand WD.StaleElementReference _) -> return ()
    Left err -> throwM err
    Right !_ -> liftIO $ assertFailure "Expected element to be removed, but it still exists"

shouldContainText :: Text -> WD.Element -> WD ()
shouldContainText t = flip shouldBe t <=< WD.getText

checkBodyText :: Text -> WD ()
checkBodyText = checkTextInTag "body"

checkTextInTag :: Text -> Text -> WD ()
checkTextInTag t expected = WD.findElem (WD.ByTag t) >>= shouldContainText expected

checkTextInId :: Text -> Text -> WD ()
checkTextInId i expected = WD.findElem (WD.ById i) >>= shouldContainText expected

divId :: DomBuilder t m => Text -> m a -> m a
divId i = elAttr "div" ("id" =: i)

type TestWidget n t m =
  (DomBuilder t m, HasMountStatus t m, MonadHold t m, PostBuild t m
  , Prerender n m, PerformEvent t m, TriggerEvent t m, MonadFix m
  , MonadJSM (Performable m), MonadIO (Performable m), MonadIO m
  , EventWriter t [DomMountSnapshot] m)

testWidget
  :: WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidget testCheck bodyWidget = maybe (error "test timed out") pure <=< timeout testTimeLimit $ do
  let staticApp = do
        el "head" $ pure ()
        el "body" $ do
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  ((), html) <- liftIO $ renderStatic staticApp
  let entryPoint = do
        Reflex.Dom.Core.mainWidget $ do
          (_, snapshotEvs) <- divId "test-body" $ runEventWriterT bodyWidget
          -- performEvent $ ffor snapshotEvs $ liftIO . print
          snapshots <- foldDyn (\a b -> b ++ [a]) [] snapshotEvs
          divId domSnapshotResultsDivId $ display $ fmap (fmap (fmap domMountSnapshotShowResult)) snapshots
        syncPoint
  application <- liftIO $ jsaddleOr defaultConnectionOptions entryPoint $ \_ sendResponse ->
    sendResponse $ responseLBS status200 [] $ "<!doctype html>\n" <> LBS.fromStrict html
  port <- getFreePort
  waitJSaddle <- liftIO newEmptyMVar
  let settings = foldr ($) Warp.defaultSettings
        [ Warp.setPort $ fromIntegral (toInteger port)
        , Warp.setBeforeMainLoop $ putMVar waitJSaddle ()
        ]
      -- hSilence to get rid of ConnectionClosed logs
      jsaddleWarp = forkIO $ hSilence [stderr] $ Warp.runSettings settings application
  jsaddleTid <- liftIO jsaddleWarp
  liftIO $ takeMVar waitJSaddle
  WD.openPage $ "http://localhost:" <> show port
  liftIO $ threadDelay 500000 --wait a bit
  b <- testCheck
  liftIO $ killThread jsaddleTid
  return b

-- TODO: Should this be part of more widely used module?
-- TODO: Ensure port is not taken
getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock

data Key2 a where
  Key2_Int :: Int -> Key2 Int
  Key2_Char :: Char -> Key2 Char

deriveGEq ''Key2
deriveGCompare ''Key2
deriveGShow ''Key2

instance ShowTag Key2 Identity where
  showTaggedPrec = \case
    Key2_Int _ -> showsPrec
    Key2_Char _ -> showsPrec

instance EqTag Key2 Identity where
  eqTagged (Key2_Int _) (Key2_Int _) = (==)
  eqTagged (Key2_Char _) (Key2_Char _) = (==)

instance ArgDict Key2 where
  type ConstraintsFor Key2 c = (c Int, c Char)
  argDict = \case
    Key2_Int _ -> Dict
    Key2_Char _ -> Dict

