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
import Language.Javascript.JSaddle (syncPoint, liftJSM)
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace)
import Reflex.Dom.Core
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
  let session' = sessionWith wdConfig "" . using (map (flip (,) "") caps)
      runWD = runWDOptions (WdOptions False)
  describe "text" $ session' $ do
    it "works" $ runWD $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        text "hello world"
    it "works with postBuild" $ runWD $ do
      testWidgetStatic (checkBodyText "pb") $ do
        pb <- getPostBuild
        void $ textNode $ TextNodeConfig "" $ Just $ "pb" <$ pb
    it "works for adjacent text nodes" $ runWD $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        text "hello "
        text "world"
    it "works for empty adjacent text nodes" $ runWD $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        pb <- getPostBuild
        text ""
        text ""
        _ <- textNode $ TextNodeConfig "" $ Just $ "hello " <$ pb
        _ <- textNode $ TextNodeConfig "abc" $ Just $ "" <$ pb
        _ <- textNode $ TextNodeConfig "" $ Just $ "world" <$ pb
        text ""
    it "works when empty text nodes are the only children of an element" $ runWD $ do
      testWidgetStatic (checkBodyText "hello world") $ do
        el "div" $ do
          text ""
          text ""
        text "hello world"
    it "works when an empty text node is the first child before text" $ runWD $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          text ""
          text "hello world"
    it "works when an empty text node is the first child before element" $ runWD $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          text ""
          el "span" $ text "hello world"
    it "works when an empty text node is the last child" $ runWD $ do
      testWidgetStatic (checkTextInTag "div" "hello world") $ do
        el "div" $ do
          el "span" $ text "hello world"
          text ""
    it "updates after postBuild" $ runWD $ do -- TODO I think these two tests are broken
      testWidget (checkBodyText "initial") (checkBodyText "after") $ do
        after <- delay 0 =<< getPostBuild
        void $ textNode $ TextNodeConfig "initial" $ Just $ "after" <$ after
    it "updates immediately after postBuild" $ runWD $ do
      testWidget (checkBodyText "pb") (checkBodyText "after") $ do
        pb <- getPostBuild
        after <- delay 0 pb
        void $ textNode $ TextNodeConfig "initial" $ Just $ leftmost ["pb" <$ pb, "after" <$ after]
    it "updates in immediate mode" $ runWD $ do
      let checkUpdated = do
            checkBodyText "initial"
            WD.click =<< WD.findElem (WD.ByTag "button")
            liftIO $ threadDelay 100000 -- wait for update
            checkBodyText "after"
      testWidget (pure ()) checkUpdated $ prerender_ (pure ()) $ do
        click <- button ""
        void $ textNode $ TextNodeConfig "initial" $ Just $ "after" <$ click

  describe "element" $ session' $ do
    it "works with domEvent Click" $ runWD $ do
      clickedRef <- liftIO $ newRef False
      testWidget' (WD.findElem $ WD.ByTag "div") WD.click $ do
        (e, _) <- el' "div" $ text "hello world"
        performEvent_ $ liftIO (writeRef clickedRef True) <$ domEvent Click e
      clicked <- liftIO $ readRef clickedRef
      liftIO $ assertEqual "Not clicked" True clicked
    it "works with eventFlags stopPropagation" $ runWD $ do
      firstClickedRef <- newRef False
      secondClickedRef <- newRef False
      let clickBoth = do
            WD.findElem (WD.ById "first") >>= WD.click
            WD.findElem (WD.ById "second") >>= WD.click
      testWidget (pure ()) clickBoth $ do
        (firstDivEl, _) <- el' "div" $ prerender_ (pure ()) $ do
          void $ elAttr "span" ("id" =: "first") $ text "hello world"
        performEvent_ $ liftIO (writeRef firstClickedRef True) <$ domEvent Click firstDivEl
        (secondDivEl, _) <- el' "div" $ prerender_ (pure ()) $ do
          let conf :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace
              conf = (def :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace)
                & initialAttributes .~ "id" =: "second"
                & elementConfig_eventSpec .~ (addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (\_ -> stopPropagation) def)
          void $ element "span" conf $ text "hello world"
        performEvent_ $ liftIO (writeRef secondClickedRef True) <$ domEvent Click secondDivEl
      firstClicked <- readRef firstClickedRef
      secondClicked <- readRef secondClickedRef
      assertEqual "Click propagated when it should have stopped" (True, False) (firstClicked, secondClicked)
    it "works with eventFlags preventDefault" $ runWD $ do
      let click = do
            e <- WD.findElem $ WD.ByTag "input"
            s0 <- WD.isSelected e
            WD.click e
            s1 <- WD.isSelected e
            pure (s0, s1)
      clicked <- testWidget (pure ()) click $ prerender_ (pure ()) $ do
        let conf :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace
            conf = (def :: ElementConfig EventResult (SpiderTimeline Global) GhcjsDomSpace)
              & elementConfig_eventSpec .~ (addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (\_ -> preventDefault) def)
              & initialAttributes .~ "type" =: "checkbox"
        void $ element "input" conf $ text "hello world"
      assertEqual "Click not prevented" (False, False) clicked
    it "can add/update/remove attributes" $ runWD $ do
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
    it "has ssr attribute, removes ssr attribute" $ runWD $ do
      let checkSSRAttr = do
            e <- WD.findElem $ WD.ByTag "div"
            assertAttr e "ssr" (Just "")
            pure e
      testWidget' checkSSRAttr (\e -> assertAttr e "ssr" Nothing) $ el "div" $ text "hello world"

  describe "inputElement" $ do
    describe "hydration" $ session' $ do
      it "doesn't wipe user input when switching over" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        testWidget'
          (do
            e <- WD.findElem $ WD.ByTag "input"
            WD.sendKeys "hello world" e
            pure e)
          (\e -> do
            t <- WD.attr e "value"
            t `shouldBe` Just "hello world"
            WD.click <=< WD.findElem $ WD.ByTag "button"
            input <- liftIO $ readRef inputRef
            input `shouldBe` "hello world"
          ) $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "captures user input after switchover" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "input"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- liftIO $ readRef inputRef
              input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets focus when focus occurs before hydration" $ runWD $ do
        focusRef <- newRef False
        let setup = do
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              hasFocus <- (== e) <$> WD.activeElem
              hasFocus `shouldBe` True
              readRef focusRef >>= flip shouldBe False
            check = readRef focusRef >>= flip shouldBe True
        testWidget setup check $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef <- newRef ("" :: Text)
        valueRef <- newRef ("" :: Text)
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              readRef valueByUIRef >>= flip shouldBe ""
              readRef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "input"
              WD.sendKeys "hello" e
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "hello"
              liftIO $ writeChan setValueChan "world"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ do
          update <- triggerEventWithChan setValueChan
          e <- inputElement $ def & inputElementConfig_setValue .~ update
          performEvent_ $ liftIO . (writeRef valueByUIRef) <$> _inputElement_input e
          performEvent_ $ liftIO . (writeRef valueRef) <$> updated (value e)
      it "sets checked appropriately" $ runWD $ do
        checkedByUIRef <- newRef False
        checkedRef <- newRef False
        setCheckedChan <- liftIO newChan
        let checkValue = do
              readRef checkedByUIRef >>= flip shouldBe False
              readRef checkedRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.moveToCenter e
              WD.click e
              liftIO $ threadDelay 100000
              readRef checkedByUIRef >>= flip shouldBe True
              readRef checkedRef >>= flip shouldBe True
              liftIO $ writeChan setCheckedChan False
              liftIO $ threadDelay 100000
              readRef checkedByUIRef >>= flip shouldBe True
              readRef checkedRef >>= flip shouldBe False
        testWidget (pure ()) checkValue $ do
          setChecked <- triggerEventWithChan setCheckedChan
          e <- inputElement $ def
            & initialAttributes .~ "type" =: "checkbox"
            & inputElementConfig_setChecked .~ setChecked
          performEvent_ $ liftIO . writeRef checkedByUIRef <$> _inputElement_checkedChange e
          performEvent_ $ liftIO . writeRef checkedRef <$> updated (_inputElement_checked e)
      it "captures file uploads" $ runWD $ do
        filesRef :: IORef [Text] <- newRef []
        let uploadFile = do
              e <- WD.findElem $ WD.ByTag "input"
              path <- liftIO $ writeSystemTempFile "testFile" "file contents"
              WD.sendKeys (T.pack path) e
              WD.click <=< WD.findElem $ WD.ByTag "button"
              liftIO $ removeFile path
              input <- readRef filesRef
              input `shouldBe` [T.pack $ FilePath.takeFileName path]
        testWidget (pure ()) uploadFile $ do
          e <- inputElement $ def & initialAttributes .~ "type" =: "file"
          click <- button "save"
          prerender_ (pure ()) $ performEvent_ $ ffor (tag (current (_inputElement_files e)) click) $ \fs -> do
            names <- liftJSM $ traverse File.getName fs
            liftIO $ writeRef filesRef names

    describe "hydration/immediate" $ session' $ do
      it "captures user input after switchover" $ runWD $ do
        inputRef :: IORef Text <- newRef ""
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "input"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- readRef inputRef
              input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          e <- inputElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          e <- inputElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_inputElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef :: IORef Text <- newRef ""
        valueRef :: IORef Text <- newRef ""
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              readRef valueByUIRef >>= flip shouldBe ""
              readRef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "input"
              WD.sendKeys "hello" e
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "hello"
              liftIO $ writeChan setValueChan "world"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ do
          update <- triggerEventWithChan setValueChan
          prerender_ (pure ()) $ do
            e <- inputElement $ def & inputElementConfig_setValue .~ update
            performEvent_ $ liftIO . writeRef valueByUIRef <$> _inputElement_input e
            performEvent_ $ liftIO . writeRef valueRef <$> updated (value e)
      it "sets checked appropriately" $ runWD $ do
        checkedByUIRef <- newRef False
        checkedRef <- newRef False
        setCheckedChan <- liftIO newChan
        let checkValue = do
              readRef checkedByUIRef >>= flip shouldBe False
              readRef checkedRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "input"
              WD.moveToCenter e
              WD.click e
              liftIO $ threadDelay 100000
              readRef checkedByUIRef >>= flip shouldBe True
              readRef checkedRef >>= flip shouldBe True
              liftIO $ writeChan setCheckedChan False
              liftIO $ threadDelay 100000
              readRef checkedByUIRef >>= flip shouldBe True
              readRef checkedRef >>= flip shouldBe False
        testWidget (pure ()) checkValue $ do
          setChecked <- triggerEventWithChan setCheckedChan
          prerender_ (pure ()) $ do
            e <- inputElement $ def
              & initialAttributes .~ "type" =: "checkbox"
              & inputElementConfig_setChecked .~ setChecked
            performEvent_ $ liftIO . writeRef checkedByUIRef <$> _inputElement_checkedChange e
            performEvent_ $ liftIO . writeRef checkedRef <$> updated (_inputElement_checked e)
      it "captures file uploads" $ runWD $ do
        filesRef :: IORef [Text] <- newRef []
        let uploadFile = do
              e <- WD.findElem $ WD.ByTag "input"
              path <- liftIO $ writeSystemTempFile "testFile" "file contents"
              WD.sendKeys (T.pack path) e
              WD.click <=< WD.findElem $ WD.ByTag "button"
              liftIO $ removeFile path
              input <- liftIO $ readRef filesRef
              input `shouldBe` [T.pack $ FilePath.takeFileName path]
        testWidget (pure ()) uploadFile $ prerender_ (pure ()) $ do
          e <- inputElement $ def & initialAttributes .~ "type" =: "file"
          click <- button "save"
          performEvent_ $ ffor (tag (current (_inputElement_files e)) click) $ \fs -> do
            names <- liftJSM $ traverse File.getName fs
            liftIO $ writeRef filesRef names

  describe "textAreaElement" $ do
    describe "hydration" $ session' $ do
      it "doesn't wipe user input when switching over" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        testWidget'
          (do
            e <- WD.findElem $ WD.ByTag "textarea"
            WD.sendKeys "hello world" e
            pure e)
          (\e -> do
            t <- WD.attr e "value"
            t `shouldBe` Just "hello world"
            WD.click <=< WD.findElem $ WD.ByTag "button"
            input <- readRef inputRef
            input `shouldBe` "hello world"
          ) $ do
          e <- textAreaElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "captures user input after switchover" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "textarea"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- readRef inputRef
              input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ do
          e <- textAreaElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "textarea"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ do
          e <- textAreaElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_textAreaElement_hasFocus e)
      it "sets focus when focus occurs before hydration" $ runWD $ do
        focusRef <- newRef False
        let setup = do
              e <- WD.findElem $ WD.ByTag "textarea"
              WD.click e
              hasFocus <- (== e) <$> WD.activeElem
              hasFocus `shouldBe` True
              readRef focusRef >>= flip shouldBe False
            check = readRef focusRef >>= flip shouldBe True
        testWidget setup check $ do
          e <- textAreaElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_textAreaElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef <- newRef ("" :: Text)
        valueRef <- newRef ("" :: Text)
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              readRef valueByUIRef >>= flip shouldBe ""
              readRef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "textarea"
              WD.sendKeys "hello" e
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "hello"
              liftIO $ writeChan setValueChan "world"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ do
          setValue' <- triggerEventWithChan setValueChan
          e <- textAreaElement $ def { _textAreaElementConfig_setValue = Just setValue' }
          performEvent_ $ liftIO . writeRef valueByUIRef <$> _textAreaElement_input e
          performEvent_ $ liftIO . writeRef valueRef <$> updated (value e)

    describe "hydration/immediate" $ session' $ do
      it "captures user input after switchover" $ runWD $ do
        inputRef :: IORef Text <- newRef ""
        let checkValue = do
              WD.sendKeys "hello world" <=< WD.findElem $ WD.ByTag "textarea"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              input <- liftIO $ readRef inputRef
              input `shouldBe` "hello world"
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          e <- textAreaElement def
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "textarea"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          e <- textAreaElement def
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_textAreaElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef :: IORef Text <- newRef ""
        valueRef :: IORef Text <- newRef ""
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              readRef valueByUIRef >>= flip shouldBe ""
              readRef valueRef >>= flip shouldBe ""
              e <- WD.findElem $ WD.ByTag "textarea"
              WD.sendKeys "hello" e
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "hello"
              liftIO $ writeChan setValueChan "world"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "hello"
              readRef valueRef >>= flip shouldBe "world"
        testWidget (pure ()) checkValue $ do
          setValue' <- triggerEventWithChan setValueChan
          prerender_ (pure ()) $ do
            e <- textAreaElement $ def { _textAreaElementConfig_setValue = Just setValue' }
            performEvent_ $ liftIO . writeRef valueByUIRef <$> _textAreaElement_input e
            performEvent_ $ liftIO . writeRef valueRef <$> updated (value e)

  describe "selectElement" $ do
    let options :: DomBuilder t m => m ()
        options = do
          elAttr "option" ("value" =: "one" <> "id" =: "one") $ text "one"
          elAttr "option" ("value" =: "two" <> "id" =: "two") $ text "two"
          elAttr "option" ("value" =: "three" <> "id" =: "three") $ text "three"
    describe "hydration" $ session' $ do
      it "sets initial value correctly" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        let setup = do
              e <- WD.findElem $ WD.ByTag "select"
              assertAttr e "value" (Just "three")
              WD.click <=< WD.findElem $ WD.ById "two"
              pure e
            check e = do
              assertAttr e "value" (Just "two")
              readRef inputRef >>= (`shouldBe` "three")
              WD.click <=< WD.findElem $ WD.ByTag "button"
              assertAttr e "value" (Just "two")
              readRef inputRef >>= (`shouldBe` "two")
        testWidget' setup check $ do
          (e, ()) <- selectElement (def { _selectElementConfig_initialValue = "three" }) options
          click <- button "save"
          liftIO . writeRef inputRef <=< sample $ current $ _selectElement_value e
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (_selectElement_value e)) click
      it "captures user input after switchover" $ runWD $ do
        inputRef <- newRef ("" :: Text)
        let checkValue = do
              e <- WD.findElem $ WD.ByTag "select"
              assertAttr e "value" (Just "one")
              WD.click <=< WD.findElem $ WD.ById "two"
              assertAttr e "value" (Just "two")
              WD.click <=< WD.findElem $ WD.ByTag "button"
              readRef inputRef >>= (`shouldBe` "two")
        testWidget (pure ()) checkValue $ do
          (e, ()) <- selectElement def options
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (_selectElement_value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "select"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ do
          (e, ()) <- selectElement def options
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_selectElement_hasFocus e)
      it "sets focus when focus occurs before hydration" $ runWD $ do
        focusRef <- newRef False
        let setup = do
              e <- WD.findElem $ WD.ByTag "select"
              WD.click e
              hasFocus <- (== e) <$> WD.activeElem
              hasFocus `shouldBe` True
              readRef focusRef >>= flip shouldBe False
            check = readRef focusRef >>= flip shouldBe True
        testWidget setup check $ do
          (e, ()) <- selectElement def options
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_selectElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef <- newRef ("" :: Text)
        valueRef <- newRef ("" :: Text)
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              e <- WD.findElem $ WD.ByTag "select"
              assertAttr e "value" (Just "one")
              readRef valueByUIRef >>= flip shouldBe "one"
              readRef valueRef >>= flip shouldBe "one"
              WD.click <=< WD.findElem $ WD.ById "two"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "two"
              readRef valueRef >>= flip shouldBe "two"
              liftIO $ writeChan setValueChan "three"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "two"
              readRef valueRef >>= flip shouldBe "three"
        testWidget (pure ()) checkValue $ do
          setValue' <- triggerEventWithChan setValueChan
          (e, ()) <- selectElement def { _selectElementConfig_setValue = Just setValue' } options
          performEvent_ $ liftIO . writeRef valueByUIRef <$> _selectElement_change e
          performEvent_ $ liftIO . writeRef valueRef <$> updated (_selectElement_value e)

    describe "hydration/immediate" $ session' $ do
      it "captures user input after switchover" $ runWD $ do
        inputRef :: IORef Text <- newRef ""
        let checkValue = do
              WD.click <=< WD.findElem $ WD.ById "two"
              WD.click <=< WD.findElem $ WD.ByTag "button"
              readRef inputRef >>= flip shouldBe "two"
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          (e, ()) <- selectElement def options
          click <- button "save"
          performEvent_ $ liftIO . writeRef inputRef <$> tag (current (_selectElement_value e)) click
      it "sets focus appropriately" $ runWD $ do
        focusRef <- newRef False
        let checkValue = do
              readRef focusRef >>= flip shouldBe False
              e <- WD.findElem $ WD.ByTag "select"
              WD.click e
              liftIO $ threadDelay 100000
              readRef focusRef >>= flip shouldBe True
        testWidget (pure ()) checkValue $ prerender_ (pure ()) $ do
          (e, ()) <- selectElement def options
          performEvent_ $ liftIO . writeRef focusRef <$> updated (_selectElement_hasFocus e)
      it "sets value appropriately" $ runWD $ do
        valueByUIRef :: IORef Text <- newRef ""
        valueRef :: IORef Text <- newRef ""
        setValueChan :: Chan Text <- liftIO newChan
        let checkValue = do
              readRef valueByUIRef >>= flip shouldBe "one"
              readRef valueRef >>= flip shouldBe "one"
              WD.click <=< WD.findElem $ WD.ById "two"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "two"
              readRef valueRef >>= flip shouldBe "two"
              liftIO $ writeChan setValueChan "three"
              liftIO $ threadDelay 100000
              readRef valueByUIRef >>= flip shouldBe "two"
              readRef valueRef >>= flip shouldBe "three"
        testWidget (pure ()) checkValue $ do
          setValue' <- triggerEventWithChan setValueChan
          prerender_ (pure ()) $ do
            (e, ()) <- selectElement def { _selectElementConfig_setValue = Just setValue' } options
            performEvent_ $ liftIO . writeRef valueByUIRef <$> _selectElement_change e
            performEvent_ $ liftIO . writeRef valueRef <$> updated (_selectElement_value e)

  describe "prerender" $ session' $ do
    it "works in simple case" $ runWD $ do
      testWidget (checkBodyText "One") (checkBodyText "Two") $ do
        prerender_ (text "One") (text "Two")
    it "removes element correctly" $ runWD $ do
      testWidget' (WD.findElem $ WD.ByTag "span") elementShouldBeRemoved $ do
        prerender_ (el "span" $ text "One") (text "Two")
    it "can be nested in server widget" $ runWD $ do
      testWidget (checkBodyText "One") (checkBodyText "Three") $ do
        prerender_ (prerender_ (text "One") (text "Two")) (text "Three")
    it "can be nested in client widget" $ runWD $ do
      testWidget (checkBodyText "One") (checkBodyText "Three") $ do
        prerender_ (text "One") (prerender_ (text "Two") (text "Three"))
    it "works with prerender_ siblings" $ runWD $ do
      testWidget
        (checkTextInId "a1" "One" >> checkTextInId "b1" "Three" >> checkTextInId "c1" "Five")
        (checkTextInId "a2" "Two" >> checkTextInId "b2" "Four" >> checkTextInId "c2" "Six") $ do
          prerender_ (divId "a1" $ text "One") (divId "a2" $ text "Two")
          prerender_ (divId "b1" $ text "Three") (divId "b2" $ text "Four")
          prerender_ (divId "c1" $ text "Five") (divId "c2" $ text "Six")
    it "works inside other element" $ runWD $ do
      testWidget (checkTextInTag "div" "One") (checkTextInTag "div" "Two") $ do
        el "div" $ prerender_ (text "One") (text "Two")
    it "places fences and removes them" $ runWD $ do
      testWidget'
        (do
          scripts <- WD.findElems $ WD.ByTag "script"
          filterM (\s -> maybe False (\t -> "prerender" `T.isPrefixOf` t) <$> WD.attr s "type") scripts
        )
        (traverse_ elementShouldBeRemoved)
        (el "span" $ prerender_ (text "One") (text "Two"))
    it "postBuild works on server side" $ runWD $ do
      lock :: MVar () <- liftIO newEmptyMVar
      testWidget (liftIO $ takeMVar lock) (pure ()) $ do
        prerender_ (do
          pb <- getPostBuild
          performEvent_ $ liftIO (putMVar lock ()) <$ pb) blank
    it "postBuild works on client side" $ runWD $ do
      lock :: MVar () <- liftIO newEmptyMVar
      testWidget (pure ()) (liftIO $ takeMVar lock) $ do
        prerender_ blank $ do
          pb <- getPostBuild
          performEvent_ $ liftIO (putMVar lock ()) <$ pb
    it "result Dynamic is updated *after* switchover" $ runWD $ do
      let static = shouldContainText "PostBuild" =<< WD.findElem (WD.ByTag "body")
          check = shouldContainText "Client" =<< WD.findElem (WD.ByTag "body")
      testWidget static check $ void $ do
        d <- prerender (pure "Initial") (pure "Client")
        pb <- getPostBuild
        initial <- sample $ current d
        textNode $ TextNodeConfig initial $ Just $ leftmost [updated d, "PostBuild" <$ pb]
    -- This essentially checks that the client IO runs *after* switchover/postBuild,
    -- thus can't create conflicting DOM
    it "can't exploit IO to break hydration" $ runWD $ do
      let static = shouldContainText "Initial" =<< WD.findElem (WD.ByTag "body")
      testWidgetStatic static $ void $ do
        ref <- liftIO $ newRef "Initial"
        prerender_ (pure ()) (liftIO $ writeRef ref "Client")
        text <=< liftIO $ readRef ref
    -- As above, so below
    it "can't exploit triggerEvent to break hydration" $ runWD $ do
      let static = shouldContainText "Initial" =<< WD.findElem (WD.ByTag "body")
          check = shouldContainText "Client" =<< WD.findElem (WD.ByTag "body")
      testWidget static check $ void $ do
        (e, trigger) <- newTriggerEvent
        prerender_ (pure ()) (liftIO $ trigger "Client")
        textNode $ TextNodeConfig "Initial" $ Just e

  describe "runWithReplace" $ session' $ do
    it "works" $ runWD $ do
      replaceChan :: Chan Text <- liftIO newChan
      let setup = WD.findElem $ WD.ByTag "div"
          check ssr = do
            -- Check that the original element still exists and has the correct text
            WD.getText ssr >>= flip shouldBe "two"
            liftIO $ do
              writeChan replaceChan "three"
              threadDelay 100000
            elementShouldBeRemoved ssr
            shouldContainText "three" =<< WD.findElem (WD.ByTag "span")
      testWidget' setup check $ do
        replace <- triggerEventWithChan replaceChan
        pb <- getPostBuild
        void $ runWithReplace (el "div" $ text "one") $ leftmost
          [ el "div" (text "two") <$ pb
          , el "span" . text <$> replace
          ]
    it "can be nested in initial widget" $ runWD $ do
      replaceChan :: Chan Text <- liftIO newChan
      let setup = WD.findElem $ WD.ByTag "div"
          check ssr = do
            -- Check that the original element still exists and has the correct text
            WD.getText ssr >>= flip shouldBe "two"
            liftIO $ do
              writeChan replaceChan "three"
              threadDelay 100000
            elementShouldBeRemoved ssr
            shouldContainText "three" =<< WD.findElem (WD.ByTag "span")
      testWidget' setup check $ void $ flip runWithReplace never $ do
        replace <- triggerEventWithChan replaceChan
        pb <- getPostBuild
        void $ runWithReplace (el "div" $ text "one") $ leftmost
          [ el "div" (text "two") <$ pb
          , el "span" . text <$> replace
          ]
    it "can be nested in postBuild widget" $ runWD $ do
      replaceChan <- liftIO newChan
      let setup = WD.findElem $ WD.ByTag "div"
          check ssr = do
            -- Check that the original element still exists and has the correct text
            WD.getText ssr >>= flip shouldBe "two"
            liftIO $ do
              writeChan replaceChan "three"
              threadDelay 100000
            elementShouldBeRemoved ssr
            shouldContainText "three" =<< WD.findElem (WD.ByTag "span")
      testWidget' setup check $ void $ do
        pb <- getPostBuild
        runWithReplace (pure ()) $ ffor pb $ \() -> do
          replace <- triggerEventWithChan replaceChan
          pb' <- getPostBuild
          void $ runWithReplace (el "div" $ text "one") $ leftmost
            [ el "div" (text "two") <$ pb'
            , el "span" . text <$> replace
            ]
    it "postBuild works in replaced widget" $ runWD $ do
      replaceChan <- liftIO newChan
      pbLock <- liftIO newEmptyMVar
      let check = do
            liftIO $ do
              writeChan replaceChan ()
              threadDelay 100000
              takeMVar pbLock
      testWidget (pure ()) check $ void $ do
        replace <- triggerEventWithChan replaceChan
        runWithReplace (pure ()) $ ffor replace $ \() -> do
          pb <- getPostBuild
          performEvent_ $ liftIO (putMVar pbLock ()) <$ pb

    it "can be nested in event widget" $ runWD $ do -- Test is broken, I think
      replaceChan1 :: Chan Text <- liftIO newChan
      replaceChan2 :: Chan Text <- liftIO newChan
      lock :: MVar () <- liftIO newEmptyMVar
      let check = do
            shouldContainText "" =<< WD.findElem (WD.ByTag "body")
            liftIO $ do
              writeChan replaceChan1 "one"
              threadDelay 100000
              takeMVar lock
            one <- WD.findElem $ WD.ByTag "div"
            WD.getText one >>= flip shouldBe "pb"
            liftIO $ do
              writeChan replaceChan2 "two"
              threadDelay 100000
            elementShouldBeRemoved one
            shouldContainText "two" =<< WD.findElem (WD.ByTag "span")
      testWidget (pure ()) check $ void $ do
        replace1 <- triggerEventWithChan replaceChan1
        runWithReplace (pure ()) $ ffor replace1 $ \r1 -> do
          replace2 <- triggerEventWithChan replaceChan2
          pb <- getPostBuild
          performEvent_ $ liftIO (putMVar lock ()) <$ pb
          void $ runWithReplace (el "div" $ text r1) $ leftmost
            [ el "span" . text <$> replace2
            , el "div" (text "pb") <$ pb
            ]

    it "works in immediate mode (RHS of prerender)" $ runWD $ do
      replaceChan :: Chan Text <- liftIO newChan
      let check = do
            one <- WD.findElem $ WD.ByTag "div"
            shouldContainText "one" =<< WD.findElem (WD.ByTag "body")
            WD.getText one >>= flip shouldBe "one"
            liftIO $ do
              writeChan replaceChan "pb"
              threadDelay 100000
            elementShouldBeRemoved one
            shouldContainText "pb" =<< WD.findElem (WD.ByTag "span")
      testWidget (pure ()) check $ void $ do
        replace <- triggerEventWithChan replaceChan
        prerender_ blank $ do
          void $ runWithReplace (el "div" $ text "one") $ el "span" . text <$> replace
    it "works with postBuild in immediate mode (RHS of prerender)" $ runWD $ do
      replaceChan :: Chan Text <- liftIO newChan
      let check = do
            two <- WD.findElem $ WD.ByTag "div"
            shouldContainText "two" =<< WD.findElem (WD.ByTag "body")
            WD.getText two >>= flip shouldBe "two"
            liftIO $ do
              writeChan replaceChan "three"
              threadDelay 100000
            elementShouldBeRemoved two
            shouldContainText "three" =<< WD.findElem (WD.ByTag "span")
      testWidget (pure ()) check $ void $ do
        replace <- triggerEventWithChan replaceChan
        prerender_ blank $ do
          pb <- getPostBuild
          void $ runWithReplace (el "div" $ text "one") $ leftmost
            [ el "div" (text "two") <$ pb
            , el "span" . text <$> replace
            ]

  describe "traverseDMapWithKeyWithAdjust" $ session' $ do
    let widget :: DomBuilder t m => DKey a -> Identity a -> m (Identity a)
        widget k (Identity v) = elAttr "li" ("id" =: textKey k) $ do
          elClass "span" "key" $ text $ textKey k
          elClass "span" "value" $ text $ T.pack $ has @Show k $ show v
          pure (Identity v)
        checkItem :: WD.Element -> Text -> Text -> WD ()
        checkItem li k v = do
          shouldContainText k =<< WD.findElemFrom li (WD.ByClass "key")
          shouldContainText v =<< WD.findElemFrom li (WD.ByClass "value")
        checkInitialItems dm xs = do
          liftIO $ assertEqual "Wrong amount of items in DOM" (DMap.size dm) (length xs)
          forM_ (zip xs (DMap.toList dm)) $ \(e, k :=> Identity v) -> checkItem e (textKey k) (T.pack $ has @Show k $ show v)
        getAndCheckInitialItems dm = do
          xs <- WD.findElems (WD.ByTag "li")
          checkInitialItems dm xs
          pure xs
        checkRemoval chan k = do
          e <- WD.findElem (WD.ById $ textKey k)
          liftIO $ do
            writeChan chan $ PatchDMap $ DMap.singleton k (ComposeMaybe Nothing)
            threadDelay 100000
          elementShouldBeRemoved e
        checkReplace chan k v = do
          e <- WD.findElem (WD.ById $ textKey k)
          liftIO $ do
            writeChan chan $ PatchDMap $ DMap.singleton k (ComposeMaybe $ Just $ Identity v)
            threadDelay 100000
          elementShouldBeRemoved e
          e' <- WD.findElem (WD.ById $ textKey k)
          checkItem e' (textKey k) (T.pack $ show v)
        checkInsert chan k v = do
          liftIO $ do
            writeChan chan $ PatchDMap $ DMap.singleton k (ComposeMaybe $ Just $ Identity v)
            threadDelay 100000
          e <- WD.findElem (WD.ById $ textKey k)
          checkItem e (textKey k) (T.pack $ show v)
        postBuildPatch = PatchDMap $ DMap.fromList [Key_Char :=> ComposeMaybe Nothing, Key_Bool :=> ComposeMaybe (Just $ Identity True)]
    it "doesn't replace elements at switchover, can delete/update/insert" $ runWD $ do
      chan <- liftIO newChan
      let static :: WD [WD.Element]
          static = getAndCheckInitialItems keyMap
          check :: [WD.Element] -> WD ()
          check xs = do
            checkInitialItems keyMap xs
            checkRemoval chan Key_Int
            checkReplace chan Key_Char 'B'
            checkInsert chan Key_Bool True
      testWidget' static check $ do
        (dmap, _evt) <- traverseDMapWithKeyWithAdjust widget keyMap =<< triggerEventWithChan chan
        liftIO $ dmap `H.shouldBe` keyMap
    it "handles postBuild correctly" $ runWD $ do
      chan <- liftIO newChan
      let static = getAndCheckInitialItems (fromJust $ apply postBuildPatch keyMap)
          check xs = do
            checkInitialItems (fromMaybe undefined $ apply postBuildPatch keyMap) xs
            checkRemoval chan Key_Int
            checkInsert chan Key_Char 'B'
            checkReplace chan Key_Bool True
      testWidget' static check $ void $ do
        pb <- getPostBuild
        replace <- triggerEventWithChan chan
        (dmap, _evt) <- traverseDMapWithKeyWithAdjust widget keyMap $ leftmost [postBuildPatch <$ pb, replace]
        liftIO $ dmap `H.shouldBe` keyMap
    it "can delete/update/insert when built in prerender" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems keyMap
            checkRemoval chan Key_Int
            checkReplace chan Key_Char 'B'
            checkInsert chan Key_Bool True
      testWidget (pure ()) check $ do
        replace <- triggerEventWithChan chan
        prerender_ (pure ()) $ do
          (dmap, _evt) <- traverseDMapWithKeyWithAdjust widget keyMap replace
          liftIO $ dmap `H.shouldBe` keyMap
    it "can delete/update/insert when built in immediate mode" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems keyMap
            checkRemoval chan Key_Int
            checkReplace chan Key_Char 'B'
            checkInsert chan Key_Bool True
      testWidget (pure ()) check $ void $ do
        pb <- getPostBuild
        runWithReplace (pure ()) $ ffor pb $ \() -> void $ do
          (dmap, _evt) <- traverseDMapWithKeyWithAdjust widget keyMap =<< triggerEventWithChan chan
          liftIO $ dmap `H.shouldBe` keyMap
    -- Should be fixed by prerender changes!
    it "handles postBuild correctly in prerender" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems (fromJust $ apply postBuildPatch keyMap)
            checkRemoval chan Key_Int
            checkInsert chan Key_Char 'B'
            checkReplace chan Key_Bool True
      testWidget (pure ()) check $ do
        replace <- triggerEventWithChan chan
        prerender_ (pure ()) $ void $ do
          pb <- getPostBuild
          (dmap, _evt) <- traverseDMapWithKeyWithAdjust widget keyMap $ leftmost [postBuildPatch <$ pb, replace]
          liftIO $ dmap `H.shouldBe` keyMap

  describe "traverseIntMapWithKeyWithAdjust" $ session' $ do
    let textKeyInt k = "key" <> T.pack (show k)
        intMap = IntMap.fromList
          [ (1, "one")
          , (2, "two")
          , (3, "three")
          ]
    let widget :: DomBuilder t m => IntMap.Key -> Text -> m Text
        widget k v = elAttr "li" ("id" =: textKeyInt k) $ do
          elClass "span" "key" $ text $ textKeyInt k
          elClass "span" "value" $ text v
          pure v
        checkItem :: WD.Element -> Text -> Text -> WD ()
        checkItem li k v = do
          shouldContainText k =<< WD.findElemFrom li (WD.ByClass "key")
          shouldContainText v =<< WD.findElemFrom li (WD.ByClass "value")
        checkInitialItems dm xs = do
          liftIO $ assertEqual "Wrong amount of items in DOM" (IntMap.size dm) (length xs)
          forM_ (zip xs (IntMap.toList dm)) $ \(e, (k, v)) -> checkItem e (textKeyInt k) v
        getAndCheckInitialItems dm = do
          xs <- WD.findElems (WD.ByTag "li")
          checkInitialItems dm xs
          pure xs
        checkRemoval chan k = do
          e <- WD.findElem (WD.ById $ textKeyInt k)
          liftIO $ do
            writeChan chan $ PatchIntMap $ IntMap.singleton k Nothing
            threadDelay 100000
          elementShouldBeRemoved e
        checkReplace chan k v = do
          e <- WD.findElem (WD.ById $ textKeyInt k)
          liftIO $ do
            writeChan chan $ PatchIntMap $ IntMap.singleton k $ Just v
            threadDelay 100000
          elementShouldBeRemoved e
          e' <- WD.findElem (WD.ById $ textKeyInt k)
          checkItem e' (textKeyInt k) v
        checkInsert chan k v = do
          liftIO $ do
            writeChan chan $ PatchIntMap $ IntMap.singleton k $ Just v
            threadDelay 100000
          e <- WD.findElem (WD.ById $ textKeyInt k)
          checkItem e (textKeyInt k) v
        postBuildPatch = PatchIntMap $ IntMap.fromList [(2, Nothing), (3, Just "trois"), (4, Just "four")]
    it "doesn't replace elements at switchover, can delete/update/insert" $ runWD $ do
      chan <- liftIO newChan
      let static = getAndCheckInitialItems intMap
          check xs = do
            checkInitialItems intMap xs
            checkRemoval chan 1
            checkReplace chan 2 "deux"
            checkInsert chan 4 "four"
      testWidget' static check $ void $ do
        (im, _evt) <- traverseIntMapWithKeyWithAdjust widget intMap =<< triggerEventWithChan chan
        liftIO $ im `H.shouldBe` intMap
    it "handles postBuild correctly" $ runWD $ do
      chan <- liftIO newChan
      let static = getAndCheckInitialItems (fromJust $ apply postBuildPatch intMap)
          check xs = do
            checkInitialItems (fromMaybe undefined $ apply postBuildPatch intMap) xs
            checkRemoval chan 1
            checkInsert chan 2 "deux"
            checkReplace chan 3 "trois"
      testWidget' static check $ void $ do
        pb <- getPostBuild
        replace <- triggerEventWithChan chan
        (dmap, _evt) <- traverseIntMapWithKeyWithAdjust widget intMap $ leftmost [postBuildPatch <$ pb, replace]
        liftIO $ dmap `H.shouldBe` intMap
    it "can delete/update/insert when built in prerender" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems intMap
            checkRemoval chan 1
            checkReplace chan 2 "deux"
            checkInsert chan 3 "trois"
      testWidget (pure ()) check $ do
        replace <- triggerEventWithChan chan
        prerender_ (pure ()) $ do
          (dmap, _evt) <- traverseIntMapWithKeyWithAdjust widget intMap replace
          liftIO $ dmap `H.shouldBe` intMap
    it "can delete/update/insert when built in immediate mode" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems intMap
            checkRemoval chan 1
            checkReplace chan 2 "deux"
            checkInsert chan 3 "trois"
      testWidget (pure ()) check $ void $ do
        pb <- getPostBuild
        runWithReplace (pure ()) $ ffor pb $ \() -> void $ do
          (dmap, _evt) <- traverseIntMapWithKeyWithAdjust widget intMap =<< triggerEventWithChan chan
          liftIO $ dmap `H.shouldBe` intMap
    -- Should be fixed by prerender changes!
    it "handles postBuild correctly in prerender" $ runWD $ do
      chan <- liftIO newChan
      let check = do
            _ <- getAndCheckInitialItems (fromJust $ apply postBuildPatch intMap)
            checkRemoval chan 1
            checkInsert chan 2 "deux"
            checkReplace chan 3 "trois"
      testWidget (pure ()) check $ do
        replace <- triggerEventWithChan chan
        prerender_ (pure ()) $ void $ do
          pb <- getPostBuild
          (dmap, _evt) <- traverseIntMapWithKeyWithAdjust widget intMap $ leftmost [postBuildPatch <$ pb, replace]
          liftIO $ dmap `H.shouldBe` intMap

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

triggerEventWithChan :: (Reflex t, TriggerEvent t m, Prerender t m) => Chan a -> m (Event t a)
triggerEventWithChan chan = do
  (e, trigger) <- newTriggerEvent
  prerender_ (pure ()) $ void $ liftIO $ forkIO $ forever $ trigger =<< readChan chan
  pure e

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

type TestWidget n t m = (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender t m, PerformEvent t m, TriggerEvent t m, MonadFix m, MonadIO (Performable m), MonadIO m)

testWidgetStatic
  :: WD b
  -- ^ Webdriver commands to run before JS runs and after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> WD b
testWidgetStatic w = testWidget (void w) w

-- | TODO: do something about JSExceptions not causing tests to fail
testWidget
  :: WD ()
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> WD b
testWidget beforeJS afterSwitchover = testWidget' beforeJS (const afterSwitchover)

-- | TODO: do something about JSExceptions not causing tests to fail
testWidget'
  :: WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidget' beforeJS afterSwitchover bodyWidget = maybe (error "test timed out") pure <=< timeout testTimeLimit $ do
  let staticApp = do
        el "head" $ pure ()
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  ((), html) <- liftIO $ renderStatic staticApp
  waitBeforeJS <- liftIO newEmptyMVar -- Empty until JS should be run
  waitUntilSwitchover <- liftIO newEmptyMVar -- Empty until switchover
  let entryPoint = do
        liftIO $ takeMVar waitBeforeJS
        mainHydrationWidgetWithSwitchoverAction (putMVar waitUntilSwitchover ()) (pure ()) $ bodyWidget
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
  a <- beforeJS
  liftIO $ putMVar waitBeforeJS ()
  liftIO $ takeMVar waitUntilSwitchover
  liftIO $ threadDelay 100000 -- wait a bit
  b <- afterSwitchover a
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

testWidgetDebug :: (forall m js. TestWidget js (SpiderTimeline Global) m => m ()) -> IO ()
testWidgetDebug bodyWidget = do
  let staticApp = do
        el "head" $ pure ()
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  ((), html) <- renderStatic staticApp
  let entryPoint = do
        mainHydrationWidgetWithSwitchoverAction (pure ()) (pure ()) $ bodyWidget
        syncPoint
  application <- jsaddleOr defaultConnectionOptions entryPoint $ \_ sendResponse -> sendResponse $ responseLBS status200 [] $ "<!doctype html>\n" <> LBS.fromStrict html
  Warp.runSettings (Warp.setPort 3911 Warp.defaultSettings) application
