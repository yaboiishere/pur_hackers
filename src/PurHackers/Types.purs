module PurHackers.Types where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype TcpPort = TcpPort Int

derive instance eqTcpPort :: Eq TcpPort
derive instance genericTcpPort :: Generic TcpPort _

instance showTcpPort :: Show TcpPort where
  show = genericShow
