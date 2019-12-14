{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fmax-simplifier-iterations=5 -ddump-simpl -ddump-to-file -dsuppress-coercions -dsuppress-idinfo #-}
import Control.Monad.State
import Data.Monoid
import Data.IntMap (IntMap, assocs, elems, empty, fromList, size, singleton)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Reflex.Dom
import Data.FastMutableIntMap
import System.Random

type RNG = StdGen
type Entropy   = Int
type RowNumber = Int
type RowIndex  = Int
data Row   = Row { num :: RowNumber, txt :: Text, selected :: Bool } deriving (Eq, Show)
type Table = IntMap Row
data Model = Model { rng :: RNG, nextNum :: RowNumber, selection :: Maybe Row } deriving Show
type TableDiff = PatchIntMap Row

main :: IO ()
main = do
  seed <- randomIO
  mainWidgetWithHead' (\_ -> headW, \_ -> bodyW seed)

titleW :: DomBuilder t m => m ()
titleW = text "Reflex-dom keyed"

headW :: DomBuilder t m => m ()
headW = do
  el "title" titleW
  elAttr "link" ("href" =: "/css/currentStyle.css" <> "rel" =: "stylesheet") blank

bodyW :: forall t m. (MonadFix m, MonadHold t m, DomBuilder t m) => Entropy -> m ()
bodyW seed = divClass "main" $ divClass "container" $ mdo
  buttonEvents <- divClass "jumbotron" $ divClass "row" $ do
    divClass "col-md-6" $ el "h1" titleW
    divClass "col-md-6" $ divClass "row" $ sequence
      [ buttonW "run"      "Create 1,000 rows"     $ resetRows 1000
      , buttonW "runlots"  "Create 10,000 rows"    $ resetRows 10000
      , buttonW "add"      "Append 1,000 rows"     $ appendRows 1000
      , buttonW "update"   "Update every 10th row" $ updateRows (\i -> mod i 10 == 0) (<> " !!!")
      , buttonW "clear"    "Clear"                 $ clearRows
      , buttonW "swaprows" "Swap Rows"             $ swapRows (1, 998)
      ]

  let initial = Model { rng = mkStdGen seed, nextNum = 1, selection = Nothing }
  let events = leftmost $ fmap (foldl1 sequenceSteps) rowEvents : buttonEvents

  rowEvents <- tableW dynMT

  dynMT <- mapAccum_ step (initial, empty) events

  elAttr "span" ("class" =: "preloadicon glyphicon glyphicon-remove" <> "aria-hidden" =: "true") blank

  blank

type Step = (Model, Table) -> (Model, TableDiff)

sequenceSteps :: Step -> Step -> Step
sequenceSteps a b (m, t) =
  let (ma, tda) = a (m, t)
      ta = applyAlways tda t
      (mb, tdb) = b (ma, ta)
  in (mb, tdb <> tda)

step :: (Model, Table) -> Step -> ((Model, Table), TableDiff)
step (m, t) f = ((m', t'), dt)
  where
    (m', dt) = f (m, t)
    t' = applyAlways dt t

buttonW :: DomBuilder t m => Text -> Text -> a -> m (Event t a)
buttonW id txt val = divClass "col-sm-6 smallpad" $ buttonW' ("class" =: "btn btn-primary btn-block" <> "id" =: id)
  where
    buttonW' attrs = do
      (b, _) <- elAttr' "button" attrs $ text txt
      pure $ val <$ domEvent Click b

tableW :: (MonadHold t m, DomBuilder t m) => Event t TableDiff -> m (Event t (IntMap Step))
tableW diff = do
  elClass "table" "table table-hover table-striped test-data" $ do
    el "tbody" $ do
      (v0, v') <- traverseIntMapWithKeyWithAdjust (\_ -> rowW) empty diff
      rowEvents <- holdIncremental v0 v'
      return $ mergeIntIncremental rowEvents

rowW :: DomBuilder t m => Row -> m (Event t Step)
rowW row = elClass "tr" (if selected row then "danger" else "") $
  do
    elClass "td" "col-md-1" $ do
      text $ T.pack $ show $ num row
    (sel, _) <- elClass' "td" "col-md-4". el "a" . text $ txt row
    (del, _) <- elClass' "td" "col-md-1" deleteW
    elClass "td" "col-md-6" blank
    pure . leftmost $ zipWith tagClick [selectRow, deleteRow] [sel, del]
  where
    tagClick f el = f row <$ (domEvent Click el)

deleteW :: DomBuilder t m => m ()
deleteW = el "a" $ elAttr "span" ("aria-hidden" =: "true" <> "class" =: "glyphicon glyphicon-remove") blank


{- Domain logic -}
clearRows :: (Model, Table) -> (Model, TableDiff)
clearRows (m, t) = (m { selection = Nothing }, PatchIntMap $ Nothing <$ t)

updateRows :: (RowIndex -> Bool) -> (Text -> Text) -> (Model, Table) -> (Model, TableDiff)
updateRows p f (m, t) = (m, PatchIntMap $ Just . update <$> targets)
  where
    targets = fromList . filterByIndex p . assocs $ t
    update  = \r -> r { txt = f $ txt r }

filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex p = map snd . filter (\(i, _) -> p i) . zip [0..]

swapRows :: (RowIndex, RowIndex) -> (Model, Table) -> (Model, TableDiff)
swapRows (a, b) (m, t) = (m, PatchIntMap $ if max a b < length t then swap else empty)
  where
    swap = point a b <> point b a
    point x y = singleton (fst (val x)) $ Just $ snd $ val y
    val = (assocs t !!)

selectRow :: Row -> (Model, Table) -> (Model, TableDiff)
selectRow r (m, t) = (m { selection = Just r}, PatchIntMap $ dr <> ds)
  where
    dr = sel True r
    ds = maybe empty (sel False) $ selection m
    sel b r' = singleton (num r') $ Just r' { selected = b }

deleteRow :: Row -> (Model, Table) -> (Model, TableDiff)
deleteRow r (m, t) = (m { selection = mfilter (/= r) (selection m) }, PatchIntMap $ singleton (num r) Nothing)

resetRows :: Int -> (Model, Table) -> (Model, TableDiff)
resetRows n (m, t) = (m'', dt <> dt')
  where
    (m',  dt ) = addRows n (m, t)
    (m'', dt') = clearRows (m', t)

appendRows :: Int -> (Model, Table) -> (Model, TableDiff)
appendRows n (m, t) = addRows n (m, t)

addRows :: Int -> (Model, Table) -> (Model, TableDiff)
addRows count (m, t) = (m { rng = rng', nextNum = nextNum' }, PatchIntMap diff)
  where
    rowsST = sequence (rowST <$ [0..count-1])
    (rows, (nextNum', rng')) = runState rowsST (nextNum m, rng m)
    diff = fromList . fmap (\r -> (num r, Just r)) $ rows

rowST :: State (RowNumber, RNG) Row
rowST  = state (\(n, g) -> let (e, g') = next g in (Row {num = n, txt = randomName e, selected = False}, (n+1, g')))

randomName :: Entropy -> Text
randomName e = T.intercalate " " $ (`modIndex` e) <$> [adjectives, colours, nouns]

-- | Index into a Vector, modulo its length
--
-- WARNING: The vector must have length >= 1, and the index must be >= 0.
modIndex :: Vector a -> Int -> a
modIndex v n = Vector.unsafeIndex v $ n `mod` Vector.length v

adjectives, colours, nouns :: Vector Text
adjectives = Vector.fromList
  [ "pretty"
  , "large"
  , "big"
  , "small"
  , "tall"
  , "short"
  , "long"
  , "handsome"
  , "plain"
  , "quaint"
  , "clean"
  , "elegant"
  , "easy"
  , "angry"
  , "crazy"
  , "helpful"
  , "mushy"
  , "odd"
  , "unsightly"
  , "adorable"
  , "important"
  , "inexpensive"
  , "cheap"
  , "expensive"
  , "fancy"
  ]
colours = Vector.fromList
  [ "red"
  , "yellow"
  , "blue"
  , "green"
  , "pink"
  , "brown"
  , "purple"
  , "brown"
  , "white"
  , "black"
  , "orange"
  ]
nouns = Vector.fromList
  [ "table"
  , "chair"
  , "house"
  , "bbq"
  , "desk"
  , "car"
  , "pony"
  , "cookie"
  , "sandwich"
  , "burger"
  , "pizza"
  , "mouse"
  , "keyboard"
  ]
