module Reflex.Dom.Storage where

type Key = Text

type Value = Text

data StorageConfig t = StorageConfig
  { _storageConfig_getLength :: Event t () --TODO: Traversable functor on each of these
  , _storageConfig_getKey :: Event t Integer
  , _storageConfig_getItem :: Event t Key
  , _storageConfig_setItem :: Event t (Key, Value)
  , _storageConfig_removeItem :: Event t Key
  , _storageConfig_clear :: Event t ()
  }

data Storage t = Storage
  { _storage_length :: Event t Integer
  , _storage_key :: Event t (Maybe Key)
  , _storage_item :: Event t (Maybe Value)
  , _storage_itemWasSet :: Event t ()
  , _storage_itemWasRemoved :: Event t ()
  , _storage_wasCleared :: Event t ()
  , _storage_changed :: Event t StorageEvent -- Only fires when an *external* event causes a change, not one of the ones in our StorageConfig
  }

data StorageEvent = StorageEvent
  { _storageEvent_key :: Key
  , _storageEvent_oldValue :: Value
  , _storageEvent_newValue :: Value
  , _storageEvent_url :: Text --URL
  }
