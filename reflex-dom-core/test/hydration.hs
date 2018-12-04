{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Language.Javascript.JSaddle (JSException(..), syncPoint, liftJSM, jsg, js1)
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Core
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace)
import System.Random
import System.Timeout
import Test.Hspec
import Test.HUnit
import Test.WebDriver (WD)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.File as File

--import System.IO.Silently
import System.IO.Temp
import System.Directory
import qualified System.FilePath as FilePath

import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.WebDriver as WD

testTimeLimit :: Int
testTimeLimit = 1 * 1000 * 1000

chromeConfig :: WD.WDConfig
chromeConfig = WD.useBrowser (WD.chrome { WD.chromeBinary = Just "/run/current-system/sw/bin/chromium", WD.chromeOptions = ["--headless"]}) WD.defaultConfig

-- TODO list
-- use only available ports
-- parallel (requires port fix)

main :: IO ()
main = hspec $ parallel $ do

  describe "text" $ parallel $ do
    it "works" $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        text "hello world"
    it "works with postBuild" $ do
      testWidgetStatic (checkBodyText "pb") $ do
        pb <- getPostBuild
        void $ textNode $ TextNodeConfig "" $ Just $ "pb" <$ pb
    it "works for adjacent text nodes" $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        text "hello "
        text "world"
    it "works for empty adjacent text nodes" $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        pb <- getPostBuild
        text ""
        text ""
        _ <- textNode $ TextNodeConfig "" $ Just $ "hello " <$ pb
        _ <- textNode $ TextNodeConfig "abc" $ Just $ "" <$ pb
        _ <- textNode $ TextNodeConfig "" $ Just $ "world" <$ pb
        text ""
    it "works when empty text nodes are the only children of an element" $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        el "div" $ do
          text ""
          text ""
        text "hello world"
    it "works when an empty text node is the first child before text" $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          text ""
          text "hello world"
    it "works when an empty text node is the first child before element" $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          text ""
          el "span" $ text "hello world"
    it "works when an empty text node is the last child" $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          el "span" $ text "hello world"
          text ""

    it "updates after postBuild" $ do
      testWidget (checkBodyText "initial") (checkBodyText "after") $ do
        after <- delay 0 =<< getPostBuild
        void $ textNode $ TextNodeConfig "initial" $ Just $ "after" <$ after
    it "updates immediately after postBuild" $ do
      testWidget (checkBodyText "pb") (checkBodyText "after") $ do
        pb <- getPostBuild
        after <- delay 0 pb
        void $ textNode $ TextNodeConfig "initial" $ Just $ leftmost ["pb" <$ pb, "after" <$ after]
    it "updates in immediate mode" $ do
      let checkUpdated = do
            checkBodyText "initial"
            WD.click =<< WD.findElem (WD.ByTag "button")
            liftIO $ threadDelay 100000 -- wait for update
            checkBodyText "after"
      testWidget (pure ()) checkUpdated $ prerender (pure ()) $ do
        click <- button ""
        void $ textNode $ TextNodeConfig "initial" $ Just $ "after" <$ click

    it "works when given differing results of prerender" $ do
      testWidgetStatic (pure ()) $ do
        text =<< prerender (pure "One") (pure "Two")


  describe "element" $ parallel $ do
    it "works with domEvent Click" $ do
      clickedRef <- newIORef False
      testWidget' (WD.findElem $ WD.ByTag "div") WD.click $ do
        (e, _) <- el' "div" $ text "hello world"
        performEvent_ $ liftIO (writeIORef clickedRef True) <$ domEvent Click e
      clicked <- readIORef clickedRef
      assertEqual "Not clicked" True clicked
    it "works with eventFlags stopPropagation" $ do
      firstClickedRef <- newIORef False
      secondClickedRef <- newIORef False
      let clickBoth = do
            WD.findElem (WD.ById "first") >>= WD.click
            WD.findElem (WD.ById "second") >>= WD.click
      testWidget (pure ()) clickBoth $ do
        (firstDivEl, _) <- el' "div" $ prerender (pure ()) $ do
          void $ elAttr "span" ("id" =: "first") $ text "hello world"
        performEvent_ $ liftIO (writeIORef firstClickedRef True) <$ domEvent Click firstDivEl
        (secondDivEl, _) <- el' "div" $ prerender (pure ()) $ do
          let conf :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace
              conf = (def :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace)
                & initialAttributes .~ "id" =: "second"
                & elementConfig_eventSpec .~ (addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (\_ -> stopPropagation) def)
          void $ element "span" conf $ text "hello world"
        performEvent_ $ liftIO (writeIORef secondClickedRef True) <$ domEvent Click secondDivEl
      firstClicked <- readIORef firstClickedRef
      secondClicked <- readIORef secondClickedRef
      assertEqual "Click propagated when it should have stopped" (True, False) (firstClicked, secondClicked)
    it "works with eventFlags preventDefault" $ do
      let click = do
            e <- WD.findElem $ WD.ByTag "input"
            s0 <- WD.isSelected e
            WD.click e
            s1 <- WD.isSelected e
            pure (s0, s1)
      clicked <- testWidget (pure ()) click $ prerender (pure ()) $ do
        let conf :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace
            conf = (def :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace)
              & elementConfig_eventSpec .~ (addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (\_ -> preventDefault) def)
              & initialAttributes .~ "type" =: "checkbox"
        void $ element "input" conf $ text "hello world"
      assertEqual "Click not prevented" (False, False) clicked

    it "can add/update/remove attributes" $ do
      let checkInitialAttrs = do
            e <- WD.findElem $ WD.ByTag "div"
            assertAttr e "const" (Just "const")
            assertAttr e "delete" (Just "delete")
            assertAttr e "init" (Just "init")
            assertAttr e "click" Nothing
            pure e
          checkModifyAttrs e = do
            WD.click e
            liftIO $ threadDelay 100000
            assertAttr e "const" (Just "const")
            assertAttr e "delete" Nothing
            assertAttr e "init" (Just "click")
            assertAttr e "click" (Just "click")
      testWidget' checkInitialAttrs checkModifyAttrs $ mdo
        let conf = def
              & initialAttributes .~ "const" =: "const" <> "delete" =: "delete" <> "init" =: "init"
              & modifyAttributes .~ (("delete" =: Nothing <> "init" =: Just "click" <> "click" =: Just "click") <$ click)
        (e, ()) <- element "div" conf $ text "hello world"
        let click = domEvent Click e
        return ()

    -- TODO check this is the correct solution
    it "has ssr attribute, removes ssr attribute" $ do
      let checkSSRAttr = do
            e <- WD.findElem $ WD.ByTag "div"
            assertAttr e "ssr" (Just "")
            pure e
      testWidget' checkSSRAttr (\e -> assertAttr e "ssr" Nothing) $ el "div" $ text "hello world"

    it "can't misuse raw elements" $ do
      clicked <- newIORef False
      let check = do
            e <- WD.findElem $ WD.ByTag "div"
            liftIO $ readIORef clicked >>= flip shouldBe True
            WD.click e
            liftIO $ do
              threadDelay 100000
              readIORef clicked >>= flip shouldBe False
      testWidget (pure ()) check $ do
        (e, _) <- el' "div" $ text "hello"
        prerender (pure ()) $ do
          let e' = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw e)
          liftJSM $ e' `EventM.on` Events.click $ do
            liftIO $ writeIORef clicked True
          pure ()

  describe "inputElement" $ parallel $ do
    describe "hydration" $ do
      it "doesn't wipe user input when switching over" $ do
        inputRef <- newIORef ""
        testWidget'
          (do
            e <- WD.findElem $ WD.ByTag "input"
            WD.sendKeys "hello world" e
            pure e)
          (\e -> do
            t <- WD.attr e "value"
            liftIO $ t `shouldBe` Just "hello world"
            WD.click <=< WD.findElem $ WD.ByTag "button"
            input <- liftIO $ readIORef inputRef
            liftIO $ input `shouldBe` "hello world"
          ) $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeIORef inputRef <$> tag (current (value e)) click
      it "captures user input after switchover" $ do
        inputRef <- newIORef ""
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "input"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- liftIO $ readIORef inputRef
              liftIO $ input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeIORef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ do
        focusRef <- newIORef False
        let checkValue = do
              liftIO $ readIORef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              liftIO $ threadDelay 100000
              liftIO $ readIORef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeIORef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets focus when focus occurs before hydration" $ do
        focusRef <- newIORef False
        let setup = do
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              hasFocus <- (== e) <$> WD.activeElem
              liftIO $ do
                hasFocus `shouldBe` True
                readIORef focusRef >>= flip shouldBe False
            check = liftIO $ readIORef focusRef >>= flip shouldBe True
        testWidget setup check $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeIORef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets value appropriately" $ do
        valueByUIRef <- newIORef ""
        valueRef <- newIORef ""
        setValueChan :: Chan Text <- newChan
        let checkValue = do
              liftIO $ readIORef valueByUIRef >>= flip shouldBe ""
              liftIO $ readIORef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "input"
              WD.sendKeys "hello" e
              liftIO $ do
                threadDelay 100000
                readIORef valueByUIRef >>= flip shouldBe "hello"
                readIORef valueRef >>= flip shouldBe "hello"
                writeChan setValueChan "world"
                threadDelay 100000
                readIORef valueByUIRef >>= flip shouldBe "hello"
                readIORef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ do
          (setValue, triggerSetValue) <- newTriggerEvent
          prerender (pure ()) $ liftIO $ void $ forkIO $ forever $ do
            triggerSetValue =<< readChan setValueChan
          e <- inputElement $ def & inputElementConfig_setValue .~ setValue
          performEvent_ $ liftIO . writeIORef valueByUIRef <$> _inputElement_input e
          performEvent_ $ liftIO . writeIORef valueRef <$> updated (value e)
      it "sets checked appropriately" $ do
        checkedByUIRef <- newIORef False
        checkedRef <- newIORef False
        setCheckedChan <- newChan
        let checkValue = do
              liftIO $ readIORef checkedByUIRef >>= flip shouldBe False
              liftIO $ readIORef checkedRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.moveToCenter e
              WD.click e
              liftIO $ do
                threadDelay 100000
                readIORef checkedByUIRef >>= flip shouldBe True
                readIORef checkedRef >>= flip shouldBe True
                writeChan setCheckedChan False
                threadDelay 100000
                readIORef checkedByUIRef >>= flip shouldBe True
                readIORef checkedRef >>= flip shouldBe False
        testWidget (pure ()) checkValue $ do
          (setChecked, triggerSetChecked) <- newTriggerEvent
          prerender (pure ()) $ liftIO $ void $ forkIO $ forever $ do
            triggerSetChecked =<< readChan setCheckedChan
          e <- inputElement $ def
            & initialAttributes .~ "type" =: "checkbox"
            & inputElementConfig_setChecked .~ setChecked
          performEvent_ $ liftIO . writeIORef checkedByUIRef <$> _inputElement_checkedChange e
          performEvent_ $ liftIO . writeIORef checkedRef <$> updated (_inputElement_checked e)
      it "captures file uploads" $ do
        filesRef :: IORef [Text] <- newIORef []
        let uploadFile = do
              e <- WD.findElem $ WD.ByTag "input"
              path <- liftIO $ writeSystemTempFile "testFile" "file contents"
              WD.sendKeys (T.pack path) e
              WD.click <=< WD.findElem $ WD.ByTag "button"
              liftIO $ removeFile path
              input <- liftIO $ readIORef filesRef
              liftIO $ input `shouldBe` [T.pack $ FilePath.takeFileName path]
        testWidget (pure ()) uploadFile $ do
          e <- inputElement $ def & initialAttributes .~ "type" =: "file"
          click <- button "save"
          prerender (pure ()) $ performEvent_ $ ffor (tag (current (_inputElement_files e)) click) $ \fs -> do
            names <- liftJSM $ traverse File.getName fs
            liftIO $ writeIORef filesRef names

    describe "hydration/immediate" $ do
      it "captures user input after switchover" $ do
        inputRef :: IORef Text <- newIORef ""
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "input"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- liftIO $ readIORef inputRef
              liftIO $ input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ prerender (pure ()) $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeIORef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ do
        focusRef <- newIORef False
        let checkValue = do
              liftIO $ readIORef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              liftIO $ threadDelay 100000
              liftIO $ readIORef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ prerender (pure ()) $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeIORef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets value appropriately" $ do
        valueByUIRef :: IORef Text <- newIORef ""
        valueRef :: IORef Text <- newIORef ""
        setValueChan :: Chan Text <- newChan
        let checkValue = do
              liftIO $ readIORef valueByUIRef >>= flip shouldBe ""
              liftIO $ readIORef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "input"
              WD.sendKeys "hello" e
              liftIO $ do
                threadDelay 100000
                readIORef valueByUIRef >>= flip shouldBe "hello"
                readIORef valueRef >>= flip shouldBe "hello"
                writeChan setValueChan "world"
                threadDelay 100000
                readIORef valueByUIRef >>= flip shouldBe "hello"
                readIORef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ prerender (pure ()) $ do
          (setValue, triggerSetValue) <- newTriggerEvent
          prerender (pure ()) $ liftIO $ void $ forkIO $ forever $ do
            triggerSetValue =<< readChan setValueChan
          e <- inputElement $ def & inputElementConfig_setValue .~ setValue
          performEvent_ $ liftIO . writeIORef valueByUIRef <$> _inputElement_input e
          performEvent_ $ liftIO . writeIORef valueRef <$> updated (value e)
      it "sets checked appropriately" $ do
        checkedByUIRef <- newIORef False
        checkedRef <- newIORef False
        setCheckedChan <- newChan
        let checkValue = do
              liftIO $ readIORef checkedByUIRef >>= flip shouldBe False
              liftIO $ readIORef checkedRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.moveToCenter e
              WD.click e
              liftIO $ do
                threadDelay 100000
                readIORef checkedByUIRef >>= flip shouldBe True
                readIORef checkedRef >>= flip shouldBe True
                writeChan setCheckedChan False
                threadDelay 100000
                readIORef checkedByUIRef >>= flip shouldBe True
                readIORef checkedRef >>= flip shouldBe False
        testWidget (pure ()) checkValue $ prerender (pure ()) $ do
          (setChecked, triggerSetChecked) <- newTriggerEvent
          prerender (pure ()) $ liftIO $ void $ forkIO $ forever $ do
            triggerSetChecked =<< readChan setCheckedChan
          e <- inputElement $ def
            & initialAttributes .~ "type" =: "checkbox"
            & inputElementConfig_setChecked .~ setChecked
          performEvent_ $ liftIO . writeIORef checkedByUIRef <$> _inputElement_checkedChange e
          performEvent_ $ liftIO . writeIORef checkedRef <$> updated (_inputElement_checked e)
      it "captures file uploads" $ do
        filesRef :: IORef [Text] <- newIORef []
        let uploadFile = do
              e <- WD.findElem $ WD.ByTag "input"
              path <- liftIO $ writeSystemTempFile "testFile" "file contents"
              WD.sendKeys (T.pack path) e
              WD.click <=< WD.findElem $ WD.ByTag "button"
              liftIO $ removeFile path
              input <- liftIO $ readIORef filesRef
              liftIO $ input `shouldBe` [T.pack $ FilePath.takeFileName path]
        testWidget (pure ()) uploadFile $ prerender (pure ()) $ do
          e <- inputElement $ def & initialAttributes .~ "type" =: "file"
          click <- button "save"
          prerender (pure ()) $ performEvent_ $ ffor (tag (current (_inputElement_files e)) click) $ \fs -> do
            names <- liftJSM $ traverse File.getName fs
            liftIO $ writeIORef filesRef names

  describe "prerender" $ parallel $ do
    it "works in simple case" $ do
      testWidget (checkBodyText "One") (checkBodyText "Two") $ do
        prerender (text "One") (text "Two")
    it "removes element correctly" $ do
      testWidget' (WD.findElem $ WD.ByTag "span") elementShouldBeRemoved $ do
        prerender (el "span" $ text "One") (text "Two")
    it "can be nested in server widget" $ do
      testWidget (checkBodyText "One") (checkBodyText "Three") $ do
        prerender (prerender (text "One") (text "Two")) (text "Three")
    it "can be nested in client widget" $ do
      testWidget (checkBodyText "One") (checkBodyText "Three") $ do
        prerender (text "One") (prerender (text "Two") (text "Three"))
    it "works with prerender siblings" $ do
      testWidget
        (checkTextInId "a1" "One" >> checkTextInId "b1" "Three" >> checkTextInId "c1" "Five")
        (checkTextInId "a2" "Two" >> checkTextInId "b2" "Four" >> checkTextInId "c2" "Six") $ do
          prerender (divId "a1" $ text "One") (divId "a2" $ text "Two")
          prerender (divId "b1" $ text "Three") (divId "b2" $ text "Four")
          prerender (divId "c1" $ text "Five") (divId "c2" $ text "Six")
    it "works inside other element" $ do
      testWidget (checkTextInTag "div" "One") (checkTextInTag "div" "Two") $ do
        el "div" $ prerender (text "One") (text "Two")
    it "places fences and removes them" $ do
      testWidget'
        (do
          scripts <- WD.findElems $ WD.ByTag "script"
          filterM (\s -> maybe False (\t -> "prerender" `T.isPrefixOf` t) <$> WD.attr s "type") scripts
        )
        (traverse_ elementShouldBeRemoved)
        (el "span" $ prerender (text "One") (text "Two"))

assertAttr :: WD.Element -> Text -> Maybe Text -> WD ()
assertAttr e k v = liftIO . assertEqual "Incorrect attribute value" v =<< WD.attr e k

elementShouldBeRemoved :: WD.Element -> WD ()
elementShouldBeRemoved e = do
  try (WD.getText e) >>= \case
    Left (WD.FailedCommand WD.StaleElementReference _) -> return ()
    Left e -> throwM e
    Right !_ -> liftIO $ assertFailure "Expected element to be removed, but it still exists"

shouldContainText :: WD.Element -> Text -> WD ()
shouldContainText e t = liftIO . flip shouldBe t =<< WD.getText e

checkBodyText :: Text -> WD ()
checkBodyText = checkTextInTag "body"

checkTextInTag :: Text -> Text -> WD ()
checkTextInTag tag expected = do
  body <- WD.findElem $ WD.ByTag tag
  body `shouldContainText` expected

checkTextInId :: Text -> Text -> WD ()
checkTextInId i expected = do
  e <- WD.findElem $ WD.ById i
  e `shouldContainText` expected

divId :: DomBuilder t m => Text -> m a -> m a
divId i = elAttr "div" ("id" =: i)

type TestWidget js t m = (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m, MonadIO m)

testWidgetStatic
  :: WD b
  -- ^ Webdriver commands to run before JS runs and after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> IO b
testWidgetStatic w = testWidget (void w) w

-- | TODO: do something about JSExceptions not causing tests to fail
testWidget
  :: WD ()
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> IO b
testWidget beforeJS afterSwitchover = testWidget' beforeJS (const afterSwitchover)

-- | TODO: do something about JSExceptions not causing tests to fail
testWidget'
  :: WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> IO b
testWidget' beforeJS afterSwitchover bodyWidget = maybe (error "test timed out") pure <=< timeout testTimeLimit $ do
  let staticApp = do
        el "head" $ pure ()
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  ((), html) <- renderStatic staticApp
  waitBeforeJS <- newEmptyMVar -- Empty until JS should be run
  waitUntilSwitchover <- newEmptyMVar -- Empty until switchover
  let entryPoint = do
        liftIO $ takeMVar waitBeforeJS
        mainHydrationWidgetWithSwitchoverAction (putMVar waitUntilSwitchover ()) (pure ()) $ bodyWidget
        syncPoint
  application <- jsaddleOr defaultConnectionOptions entryPoint $ \_ sendResponse -> sendResponse $ responseLBS status200 [] $ "<!doctype html>\n" <> LBS.fromStrict html
  --port <- randomRIO (3000, 50000)
  let port = 3911 -- TODO
  let settings = Warp.setPort port Warp.defaultSettings
      -- hSilence to get rid of ConnectionClosed logs
      jsaddleWarp = forkIO $ Warp.runSettings settings application
  bracket jsaddleWarp killThread $ \_ -> do
    WD.runSession chromeConfig . WD.finallyClose $ do
      WD.openPage $ "http://localhost:" <> show port
      a <- beforeJS
      liftIO $ putMVar waitBeforeJS ()
      liftIO $ takeMVar waitUntilSwitchover
      liftIO $ threadDelay 100000 -- wait a bit
      afterSwitchover a
