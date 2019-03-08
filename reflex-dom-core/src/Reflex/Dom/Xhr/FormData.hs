{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Xhr.FormData
  ( postForms
  , FormValue (..)
  , fileToFormValue
  )
  where

import Control.Lens
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable
import qualified GHCJS.DOM.FormData as FD
import Foreign.JavaScript.TH
import GHCJS.DOM.File (getName)
import GHCJS.DOM.Types (File, IsBlob)
import Language.Javascript.JSaddle.Monad (MonadJSM, liftJSM)
import Reflex
import Reflex.Dom.Xhr

-- | A FormData value may be a blob/file or a string. The file can optionally be provided with filename.
data FormValue blob = FormValue_Text Text
                    | FormValue_File blob (Maybe Text) -- maybe filename

-- | Performs a POST request with the provided FormData payload
postForms
  :: ( IsBlob blob, HasJSContext (Performable m), MonadJSM (Performable m)
     , PerformEvent t m, TriggerEvent t m
     , Traversable f)
  => Text -- ^ The target url
  -> Event t (f (Map Text (FormValue blob))) -- ^ Maps of text keys and values that will be sent as "FormData"
  -> m (Event t (f XhrResponse))
postForms url payload = do
  performMkRequestsAsync $ ffor payload $ \fs -> for fs $ \u -> liftJSM $ do
    fd <- FD.newFormData Nothing
    iforM_ u $ \k v -> case v of
      FormValue_Text t -> FD.append fd k t
      FormValue_File b fn -> FD.appendBlob fd k b fn
    return $ xhrRequest "POST" url $ def & xhrRequestConfig_sendData .~ fd

-- | Converts a File (e.g., the output of a 'FileInput') into a 'FormValue'. The filename will be included if it is available.
fileToFormValue :: MonadJSM m => File -> m (FormValue File)
fileToFormValue f = do
  fn <- getName f
  return $ FormValue_File f $ Just fn
