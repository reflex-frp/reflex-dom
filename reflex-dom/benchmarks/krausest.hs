{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fmax-simplifier-iterations=5 -ddump-simpl -ddump-to-file -dsuppress-coercions -dsuppress-idinfo #-}
import Control.Monad.State
import Data.Semigroup (First(..), (<>))
import Data.IntMap (IntMap, assocs, empty, fromList, singleton)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Reflex.Dom.Core
import System.Random

type RNG = StdGen
type Entropy   = Int
type RowNumber = Int
type RowIndex  = Int
data Row   = Row { num :: RowNumber, txt :: Text } deriving (Eq, Show)
type Table = IntMap Row
data Model = Model { rng :: RNG, nextNum :: RowNumber } deriving Show
type TableDiff = PatchIntMap Row

main :: IO ()
main = do
  seed <- randomIO
  mainWidgetWithHead' (\_ -> headW, \_ -> bodyW seed)

titleW :: MonadWidget t m => m ()
titleW = text "Reflex-dom keyed"

headW :: MonadWidget t m => m ()
headW = do
  el "title" titleW
  elAttr "link" ("href" =: "/css/currentStyle.css" <> "rel" =: "stylesheet") blank

bodyW :: forall t m. MonadWidget t m => Entropy -> m ()
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

  let initial = Model { rng = mkStdGen seed, nextNum = 1 }
  let events = leftmost $ fmap (foldl1 sequenceSteps) rowEvents : buttonEvents

  n <- holdDyn (First Nothing) sels
  (rowEvents, sels) <- runEventWriterT $ tableW n dynMT

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

buttonW :: MonadWidget t m => Text -> Text -> a -> m (Event t a)
buttonW bid t val = divClass "col-sm-6 smallpad" $ buttonW' ("class" =: "btn btn-primary btn-block" <> "id" =: bid)
  where
    buttonW' attrs = do
      (b, _) <- elAttr' "button" attrs $ text t
      pure $ val <$ domEvent Click b

tableW :: MonadWidget t m => Dynamic t (First (Maybe RowNumber)) -> Event t TableDiff -> EventWriterT t (First (Maybe RowNumber)) m (Event t (IntMap Step))
tableW n diff = do
  elClass "table" "table table-hover table-striped test-data" $ do
    el "tbody" $ do
      let demuxN = demuxed (demux n)
      (v0 , v') <- traverseIntMapWithKeyWithAdjust (\_ rn -> rowW (demuxN (First $ Just $ num rn)) rn) empty diff
      rowEvents <- holdIncremental v0 v'
      return $ mergeIntIncremental rowEvents

rowW :: MonadWidget t m => Dynamic t Bool -> Row -> EventWriterT t (First (Maybe RowNumber)) m (Event t Step)
rowW selected row = elDynAttr "tr" attrs $ do
    elClass "td" "col-md-1" $ do
      text $ T.pack $ show $ num row
    (sel, _) <- elClass' "td" "col-md-4". el "a" . text $ txt row
    (del, _) <- elClass' "td" "col-md-1" deleteW
    elClass "td" "col-md-6" blank
    tellEvent $ First (Just $ num row) <$ domEvent Click sel
    tellEvent $ attachWithMaybe (\p _ -> if p then Just (First Nothing) else Nothing) (current selected) $ domEvent Click del
    return $ deleteRow row <$ domEvent Click del
  where
    attrs = ffor selected $ \p -> if p then "class" =: "danger" else mempty

deleteW :: MonadWidget t m => m ()
deleteW = el "a" $ elAttr "span" ("aria-hidden" =: "true" <> "class" =: "glyphicon glyphicon-remove") blank

{- Domain logic -}
clearRows :: (Model, Table) -> (Model, TableDiff)
clearRows (m, t) = (m, PatchIntMap $ Nothing <$ t)

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

deleteRow :: Row -> (Model, Table) -> (Model, TableDiff)
deleteRow r (m, _) = (m, PatchIntMap $ singleton (num r) Nothing)

resetRows :: Int -> (Model, Table) -> (Model, TableDiff)
resetRows n (m, t) = (m'', dt <> dt')
  where
    (m',  dt ) = addRows n (m, t)
    (m'', dt') = clearRows (m', t)

appendRows :: Int -> (Model, Table) -> (Model, TableDiff)
appendRows n (m, t) = addRows n (m, t)

addRows :: Int -> (Model, Table) -> (Model, TableDiff)
addRows n (m, _) = (m { rng = rng', nextNum = nextNum' }, PatchIntMap diff)
  where
    rowsST = sequence (rowST <$ [0..n-1])
    (rows, (nextNum', rng')) = runState rowsST (nextNum m, rng m)
    diff = fromList . fmap (\r -> (num r, Just r)) $ rows

rowST :: State (RowNumber, RNG) Row
rowST  = state (\(n, g) -> let (e, g') = next g in (Row {num = n, txt = randomName e}, (n+1, g')))

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
