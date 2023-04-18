module PurHackers.TcpEchoServer where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as IOData
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.Time (Millisecond)
import Data.Time.Duration (Milliseconds(..))
import Debug as Process.Raw
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary)
import Erl.Data.Binary.IOData as Binary
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List (List, foldM, fromFoldable, (:))
import Erl.Data.List.NonEmpty (foldr)
import Erl.Kernel.Inet (ActiveError(..), ActiveSocket, ConnectedSocket, ListenSocket, PassiveSocket, Port(..))
import Erl.Kernel.Inet as Process
import Erl.Kernel.Tcp (TcpMessage(..), TcpSocket, listen, listenPassive)
import Erl.Kernel.Tcp as Tcp
import Erl.Process (receive, self)
import Erl.Process.Raw (send, spawn)
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.GenServer (InfoFn, InitResult(..), ServerPid, ServerType, CastFn)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import PurHackers.Logger (logInfo)
import PurHackers.Logger as PurHackers.Logger

newtype TcpPort = TcpPort Int

derive instance eqTcpPort :: Eq TcpPort
derive instance genericTcpPort :: Generic TcpPort _

instance showTcpPort :: Show TcpPort where
  show = genericShow

type State =
  { port :: Port
  , connection :: TcpSocket PassiveSocket ListenSocket
  }

data Messages = TcpMessage TcpMessage | Accept

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

handleInfo _ state = do
  _ <- logInfo "Received unknown message" { message: "Received unknown message" } # liftEffect
  _ <- Tcp.close state.connection # liftEffect
  pure $ GenServer.return state

receiveSocket :: TcpSocket PassiveSocket ConnectedSocket -> Effect (Unit)
receiveSocket socket = do
  maybeListBs <- doRecv socket $ fromFoldable []
  case maybeListBs of
    Just listBs -> do
      _ <- logInfo "Received message" { message: "Received message", data: listBs } # liftEffect
      let ioData = map fromBinary listBs
      binary <- mempty $ foldr (\acc b -> IOData.append_ acc b) IOData.empty ioData
      _ <- liftEffect $ Tcp.send socket binary
      pure unit
    Nothing -> pure unit

doRecv :: TcpSocket PassiveSocket ConnectedSocket -> List Binary -> Effect (Maybe (List Binary))
doRecv socket bs = do
  received <- Tcp.recv socket 0 (Timeout $ Milliseconds 600000.0)
  case received of
    Right receivedBinary -> doRecv socket $ bs <> fromFoldable [ receivedBinary ]
    Left ActiveClosed -> do
      _ <- logInfo "Connection closed" { message: "Connection closed" } # liftEffect
      pure $ Just bs
    Left err -> do
      _ <- logInfo "Error receiving message" { message: "Error receiving messages", data: err } # liftEffect
      _ <- Tcp.close socket # liftEffect
      pure $ Nothing
