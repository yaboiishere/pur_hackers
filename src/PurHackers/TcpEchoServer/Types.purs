module PurHackers.TcpEchoServer.Types where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Erl.Kernel.Inet (ListenSocket, PassiveSocket, Port)
import Erl.Kernel.Tcp (TcpSocket)

type State =
  { port :: Port
  , connection :: TcpSocket PassiveSocket ListenSocket
  }

data Messages = Accept

derive instance eqMessages :: Eq Messages
derive instance genericMessages :: Generic Messages _

instance showMessages :: Show Messages where
  show = genericShow
