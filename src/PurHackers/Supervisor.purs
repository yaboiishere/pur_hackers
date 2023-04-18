module PurHackers.Supervisor
  ( startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List as ErlList
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), SupervisorPid, spec)
import Pinto.Supervisor as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)
import PurHackers.TcpEchoServer (TcpPort(..))
import PurHackers.TcpEchoServer as TcpEchoServer

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom supervisorName) $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  supervisorName = "PurHackers.Supervisor"
  childSpecs = ErlList.fromFoldable
    [ spec
        { id: "tcp_echo_server"
        , start: TcpEchoServer.startLink (TcpPort 3000)
        , restartStrategy: RestartTransient
        , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
        , childType: Worker
        }
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

