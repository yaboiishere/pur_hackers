module PurHackers.Application where

import PurHackers.Supervisor as PurHackersSupervisor
import Effect.Uncurried (EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List as ErlList
import Foreign (Foreign)
import Pinto.App as Application

start :: forall args. EffectFn2 Atom (ErlList.List args) Foreign
start = Application.simpleStart PurHackersSupervisor.startLink
