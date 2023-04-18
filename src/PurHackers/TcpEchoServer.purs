module PurHackers.TcpEchoServer where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (fromBinary)
import Erl.Data.List (List, fromFoldable)
import Erl.Kernel.Inet (ActiveError(..), ConnectedSocket, ListenSocket, PassiveSocket, Port(..))
import Erl.Kernel.Tcp (TcpSocket, listenPassive)
import Erl.Kernel.Tcp as Tcp
import Erl.Process.Raw (spawn)
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InfoFn, InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import PurHackers.Logger (logInfo)

newtype TcpPort = TcpPort Int

derive instance eqTcpPort :: Eq TcpPort
derive instance genericTcpPort :: Generic TcpPort _

instance showTcpPort :: Show TcpPort where
  show = genericShow

type State =
  { port :: Port
  , connection :: TcpSocket PassiveSocket ListenSocket
  }

data Messages = Accept

derive instance eqMessages :: Eq Messages
derive instance genericMessages :: Generic Messages _

instance showMessages :: Show Messages where
  show = genericShow

serverName :: RegistryName (ServerType Unit Unit Messages State)
serverName = Local $ atom "echo_server"

startLink :: TcpPort -> Effect (StartLinkResult (ServerPid Unit Unit Messages State))
startLink port = GenServer.startLink (GenServer.defaultSpec $ init port)
  { name = Just serverName
  , handleInfo = Just handleInfo
  }

init :: TcpPort -> GenServer.InitFn Unit Unit Messages State
init (TcpPort port) = do
  eitherSocket <- liftEffect $ listenPassive (Port port) {}

  case eitherSocket of
    Right socket -> do
      _timer <- Timer.sendAfter (wrap 0.0) Accept
      liftEffect $ logInfo "Listening on port" { port: port }

      pure $ InitOk { port: Port port, connection: socket }

    Left err -> pure $ InitStop $ Foreign.unsafeToForeign err

handleInfo :: InfoFn Unit Unit Messages State
handleInfo Accept state = do
  liftEffect $ logInfo "Accepting connection" { message: "Accepting connection" }
  received <- liftEffect $ Tcp.acceptPassive state.connection (Timeout $ Milliseconds 600000.0)
  case received of
    Right socket -> do
      _pid <- liftEffect $ spawn $ receiveSocket socket
      _timer <- Timer.sendAfter (wrap 0.0) Accept

      pure $ GenServer.return state
    Left err -> do
      _ <- liftEffect $ logInfo "Error accepting connection" { message: "Error accepting connection", data: err }
      pure $ GenServer.return state

receiveSocket :: TcpSocket PassiveSocket ConnectedSocket -> Effect (Unit)
receiveSocket socket = do
  _maybeListBs <- doRecv socket $ fromFoldable []
  pure unit

doRecv :: TcpSocket PassiveSocket ConnectedSocket -> List Binary -> Effect (Maybe (List Binary))
doRecv socket bs = do
  received <- Tcp.recv socket 0 (Timeout $ Milliseconds 600000.0)
  case received of
    Right receivedBinary -> do
      _ <- liftEffect $ Tcp.send socket $ fromBinary receivedBinary
      doRecv socket $ bs <> fromFoldable [ receivedBinary ]
    Left ActiveClosed -> do
      _ <- logInfo "Connection closed" { message: "Connection closed" } # liftEffect
      pure $ Just bs
    Left err -> do
      _ <- logInfo "Error receiving message" { message: "Error receiving messages", data: err } # liftEffect
      _ <- Tcp.close socket # liftEffect
      pure $ Nothing
