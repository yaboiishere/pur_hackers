module PurHackers.Logger where

import Control.Category ((<<<))
import Data.Unit (Unit)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger

domain :: List Atom
domain = (atom "PurHackers") : nil

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo str meta = Logger.info (Logger.traceMetadata domain str) meta

logWarning :: forall report. String -> { | report } -> Effect Unit
logWarning = Logger.warning <<< Logger.traceMetadata domain
