module PurHackers.PrimeTime where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, concat, fromBinary, fromString)
import Erl.Data.List (List, fromFoldable)
import Erl.Kernel.Inet (ActiveError(..), ConnectedSocket, PassiveSocket, Port(..))
import Erl.Kernel.Tcp (TcpSocket, listenPassive)
import Erl.Kernel.Tcp as Tcp
import Erl.Process.Raw (spawn)
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Pinto (RegistryName(..))
import Pinto.GenServer (InitResult(..), ServerPid, ServerType, InfoFn)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import Pinto.Types (RegistryName, StartLinkResult)
import PurHackers.Logger (logInfo)
import PurHackers.PrimeTime.Types (Messages(..), State)
import PurHackers.Types (TcpPort(..))
import Unsafe.Coerce as UnsafeCoerce

serverName :: RegistryName (ServerType Unit Unit Messages State)
serverName = Local $ atom "prime_time"

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

receiveSocket :: TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
receiveSocket socket = do
  _maybeIOData <- doRecv socket $ fromFoldable []
  pure unit

-- case maybeIOData of
--   Just ioData ->
--     ioData
--       # parseMessage
--       # (\x -> doMessages x socket)
--
--   Nothing ->
--     pure unit

doMessages :: Array String -> TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
doMessages messages socket =
  traverse_ (\m -> doMessage m socket) messages

doMessage :: String -> TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
doMessage message socket =
  message
    # encodeMessage
    # Tcp.send socket
    >>=
      ( \x -> case x of
          Right _ -> pure unit
          Left err -> do
            _ <- logInfo "Error sending message" { message: "Error sending message", data: err } # liftEffect
            _ <- Tcp.close socket # liftEffect
            pure unit
      )

doRecv :: TcpSocket PassiveSocket ConnectedSocket -> List Binary -> Effect (Maybe (IOData))
doRecv socket bs = do
  received <- Tcp.recv socket 0 (Timeout $ Milliseconds 600000.0)
  case received of
    Right receivedBinary -> do
      _ <- (receivedBinary # fromBinary # parseMessage # (\x -> doMessages x socket))
      doRecv socket $ bs <> fromFoldable [ receivedBinary ]
    Left ActiveClosed -> do
      _ <- logInfo "Connection closed" { message: "Connection closed" } # liftEffect
      pure $ Just $ listBinaryToIOData bs
    Left err -> do
      _ <- logInfo "Error receiving message" { message: "Error receiving messages", data: err } # liftEffect
      _ <- Tcp.close socket # liftEffect
      pure $ Nothing

listBinaryToIOData :: List Binary -> IOData
listBinaryToIOData = concat <<< map fromBinary

parseMessage :: IOData -> Array String
parseMessage rawData = String.split (Pattern "\n") $ UnsafeCoerce.unsafeCoerce rawData

encodeMessage :: String -> IOData
encodeMessage = fromString
