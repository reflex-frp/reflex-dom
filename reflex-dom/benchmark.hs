{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-rules -ddump-spec -ddump-to-file -dsuppress-coercions -dsuppress-idinfo #-}
module Main (main) where

import Control.Monad.State
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import System.Random
import qualified Data.Map as Map
import Data.Dependent.Map (DMap (..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Misc
import Data.Functor.Identity
import Data.Functor.Compose
import Data.IORef
import GHCJS.DOM.Types (JSM)
import qualified Data.Some as Some
import Data.Functor.Constant
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.FastMutableIntMap

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Types as DOM

--{-
main :: IO ()
main = mainWidget' $ do
  create1000 <- button "Create 1,000 rows"
  let row k _ = el "li" $ text $ T.pack $ show k
  el "ul" $ traverseIntMapWithKeyWithAdjust row IntMap.empty $ PatchIntMap (IntMap.fromDistinctAscList [(x, Just ()) | x <- [1 :: Int .. 1000]]) <$ create1000
  return ()
--}


{-
main :: IO ()
main = do
  mainWidget' $ do
    create1000 <- button "Create 1,000 rows"
    partialUpdate <- button "Update every 10th row"
    elClass "table" "table table-hover table-striped test-data" $ el "tbody" $ do
      --runWithReplace blank $ forM_ [1..1000] row <$ create1000
      rec let u = leftmost
                [ PatchIntMap (IntMap.fromDistinctAscList [(x, Just ()) | x <- [1..1000]]) <$ create1000
                , PatchIntMap (IntMap.fromDistinctAscList [(x, Just ()) | x <- [1, 11..1000]]) <$ partialUpdate
                , PatchIntMap <$> deletes
                ]
          (_, deletes) <- runEventWriterT $ traverseIntMapWithKeyWithAdjust (\k _ -> row k) mempty u
      return ()

row :: MonadWidget t m => Int -> EventWriterT t (IntMap (Maybe ())) m ()
row rowNum = el "tr" $ do
  elClass "td" "col-md-1" $ text $ T.pack $ show rowNum
  (sel, _) <- elClass' "td" "col-md-4" $ el "a" $ text "asdf"
  (del, _) <- elClass' "td" "col-md-1" deleteButton
  tellEvent $ IntMap.singleton rowNum Nothing <$ domEvent Click del
  elClass "td" "col-md-6" blank

deleteButton :: MonadWidget t m => m ()
deleteButton = el "a" $ elAttr "span" ("aria-hidden" =: "true" <> "class" =: "glyphicon glyphicon-remove") $ text "X"

--}

--
{-
type RNG = StdGen
type Entropy     = Int
type RowPosition = Int
type RowNumber   = Int
data Row   = Row { pos :: RowPosition, num :: RowNumber , txt :: Text } deriving (Eq, Show)
type Table = Map RowPosition Row
data Model = Model { rng :: RNG, table :: Table, nextNum :: RowNumber, selected :: Maybe Row } deriving Show

{- Widgets -}

titleW :: MonadWidget t m => m ()
titleW = text "Reflex-Dom"

headW :: MonadWidget t m => m ()
headW = do
  el "title" titleW
  elAttr "link" ("href" =: "/css/currentStyle.css" <> "rel" =: "stylesheet") blank

bodyW :: MonadWidget t m => Entropy -> m ()
bodyW seed = divClass "main" $ divClass "container" $ mdo
  let initial = Model { rng = mkStdGen seed, table = empty, nextNum = 1, selected = Nothing }
  dynModel <- foldDyn ($) initial $ mergeWith (.) $ rowEvents : buttonEvents

  buttonEvents :: [Event t (Model -> Model)] <- divClass "jumbotron" $ divClass "row" $ do
    divClass "col-md-6" $ el "h1" titleW
    divClass "col-md-6" $ divClass "row" $ sequence [
      buttonW "run"      "Create 1,000 rows"     $ createRows 1000,
      buttonW "runlots"  "Create 10,000 rows"    $ createRows 10000,
      buttonW "add"      "Append 1,000 rows"     $ appendRows 1000,
      buttonW "update"   "Update every 10th row" $ updateRows (\p -> mod p 10 == 0) (<> " !!!"),
      buttonW "clear"    "Clear"                 $ clearRows,
      buttonW "swaprows" "Swap Rows"             $ swapRows (4, 9)
      ]

  rowEvents' <- tableW dynModel
  let rowEvents = foldl (.) id <$> rowEvents'

  blank

buttonW :: MonadWidget t m => Text -> Text -> a -> m (Event t a)
buttonW id txt val = divClass "col-sm-6 smallpad" $ buttonW' ("class" =: "btn btn-primary btn-block" <> "id" =: id)
  where
    buttonW' attrs = do
      (b, _) <- elAttr' "button" attrs $ text txt
      pure $ val <$ domEvent Click b

tableW :: MonadWidget t m => Dynamic t Model -> m (Event t (Map RowPosition (Model -> Model)))
tableW dynM =
  elClass "table" "table table-hover table-striped test-data"
  $ el "tbody"
  $ listViewWithKey (table <$> dynM) (\_ dynRow -> rowW (selectedIs <$> dynM <*> dynRow) dynRow)
  where
    selectedIs m r = selected m == Just r

rowW :: MonadWidget t m => Dynamic t Bool -> Dynamic t Row -> m (Event t (Model -> Model))
rowW dynSelected dynRow = elDynClass "tr" trClass $
  do
    elClass "td" "col-md-1". dynText $ pack . show . num <$> dynRow
    (sel, _) <- elClass' "td" "col-md-4". el "a" . dynText $ txt <$> dynRow
    (del, _) <- elClass' "td" "col-md-1" deleteW
    elClass "td" "col-md-6" blank
    pure $ mergeWith (.) $ zipWith tagClick [selectRow, deleteRow] [sel, del]
  where
    trClass = (\b -> if b then "danger" else "") <$> dynSelected
    tagClick f el = (tag . current) (f <$> dynRow) (domEvent Click el)


{- Domain logic -}

clearRows :: Model -> Model
clearRows (Model {..}) = Model { table = empty, ..  }

updateRows :: (RowPosition -> Bool) -> (Text -> Text) -> Model -> Model
updateRows p f (Model {..}) = Model { table = (update <$> targets) <> table, ..  }
  where
    targets = filterWithKey (const . p) table
    update  = \(Row{..}) -> Row { txt = f txt, ..}

swapRows :: (RowPosition, RowPosition) -> Model -> Model
swapRows (a,b) (Model {..}) = Model { table = (a =: val b) <> (b =: val a) <> table, .. }
  where val = (table !)

selectRow, deleteRow :: Row -> Model -> Model
selectRow r m = m { selected = Just r }
deleteRow r (Model {..}) = Model { table = shifted table, .. }
  where
    shifted = fromList . zipWith update [0..] . elems . delete (pos r)
    update p (Row{..}) = (p, Row { pos = p, .. })

createRows, appendRows :: Int -> Model -> Model
createRows = newRows False
appendRows = newRows True

newRows :: Bool -> Int -> Model -> Model
newRows keepOld count (Model {..}) = Model { rng = rng', table = table', nextNum = nextNum', .. }
  where
    nextPos = if keepOld then length table else 0
    rowsST = sequence (rowST <$ [0..count-1])
    (rows, (nextPos', nextNum', rng')) = runState rowsST (nextPos, nextNum, rng)
    combine = if keepOld then (<>) else \_ new -> new
    table' = fromList . zip [0..] $ combine (elems table) rows

rowST :: State (RowPosition, RowNumber, RNG) Row
rowST  = state (\(p,n,g) -> let (e, g') = next g in (Row {pos = p, num = n, txt = randomName e}, (p+1,n+1,g')))

randomName :: Entropy -> Text
randomName e = intercalate " " $ (!!% e) <$> [adjectives, colours, nouns]
  where xs !!% n = xs !! mod n (length xs)

adjectives,colours,nouns :: [Text]
adjectives = ["pretty", "large", "big", "small", "tall", "short", "long", "handsome", "plain", "quaint", "clean", "elegant", "easy", "angry", "crazy", "helpful", "mushy", "odd", "unsightly", "adorable", "important", "inexpensive", "cheap", "expensive", "fancy"]
colours = ["red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange"]
nouns = ["table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse", "keyboard"]

-}
