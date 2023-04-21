module PurHackers.PrimeTime where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (fromNumber)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, concat, fromBinary, fromString, toBinary)
import Erl.Data.List (List, fromFoldable)
import Erl.Kernel.Inet (ActiveError(..), ConnectedSocket, PassiveSocket, Port(..))
import Erl.Kernel.Tcp (ListenError(..), TcpSocket, listenPassive)
import Erl.Kernel.Tcp as Tcp
import Erl.Process.Raw (spawn)
import Erl.Types (Timeout(..))
import Foreign (ForeignError)
import Foreign as Foreign
import Pinto (RegistryName(..))
import Pinto.GenServer (InitResult(..), ServerPid, ServerType, InfoFn)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import Pinto.Types (RegistryName, StartLinkResult)
import PurHackers.Logger (logInfo)
import PurHackers.PrimeTime.Types (Message, MessageResponse, Messages(..), State)
import PurHackers.Types (TcpPort(..))
import Simple.JSON as SimpleJson
import Unsafe.Coerce as UnsafeCoerce

foreign import isPrime :: Int -> Boolean

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

    Left (ListenPosix err) -> do
      _ <- logInfo "Error listening on port" { message: "Error listening on port", data: err } # liftEffect
      init $ TcpPort $ port

    Left err -> pure $ InitStop $ Foreign.unsafeToForeign err

handleInfo :: InfoFn Unit Unit Messages State
handleInfo Accept state = do
  liftEffect $ logInfo "Accepting connection" { message: "Accepting connection" }
  received <- liftEffect $ Tcp.acceptPassive state.connection InfiniteTimeout
  case received of
    Right socket -> do
      _pid <- liftEffect $ spawn $ receiveSocket socket
      _timer <- Timer.sendAfter (wrap 0.0) Accept

      pure $ GenServer.return state
    Left err -> do
      _ <- liftEffect $ logInfo "Error accepting connection" { message: "Error accepting connection", data: err }
      pure $ GenServer.return state

receiveSocket :: TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
receiveSocket socket =
  do
    maybeListBinary <- doRecv socket $ fromFoldable []
    case maybeListBinary of
      Just listBinary
        | listBinary == fromFoldable [] -> Tcp.close socket
        | otherwise -> do
            _ <- logInfo "Received message" { message: "Received message", data: listBinary }
            let eitherMessage = listBinary # listBinaryToIOData # parseMessage
            traverse_ (\x -> doMessage x socket) eitherMessage
            receiveSocket socket
      Nothing ->
        Tcp.close socket

doRecv :: TcpSocket PassiveSocket ConnectedSocket -> List Binary -> Effect (Maybe (List Binary))
doRecv socket bs = do
  received <- Tcp.recv socket 0 (Timeout $ wrap 1000.0)
  case received of
    Right receivedBinary ->
      doRecv socket $ bs <> fromFoldable [ receivedBinary ]
    Left ActiveClosed -> do
      _ <- logInfo "Connection closed" { message: "Connection closed" } # liftEffect
      pure $ Just bs
    Left ActiveTimeout -> do
      _ <- logInfo "Connection timed out" { message: "Connection timed out" } # liftEffect
      pure $ Just bs
    Left err -> do
      _ <- logInfo "Error receiving message" { message: "Error receiving messages", data: err } # liftEffect
      _ <- Tcp.close socket # liftEffect
      pure $ Nothing

doMessage :: Either (NonEmptyList ForeignError) Message -> TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
doMessage (Left err) socket = do
  _ <- logInfo "Error parsing message" { message: "Error parsing message", error: err }
  _ <- err # show # fromString # Tcp.send socket
  pure unit
doMessage (Right message) socket =
  message
    # calculateMessage
    # eitherEncodeMessageResponse
    # Tcp.send socket
    >>=
      ( \x -> case x of
          Right _ -> pure unit
          Left err -> do
            _ <- logInfo "Error sending message" { message: "Error sending message", data: err } # liftEffect
            _ <- Tcp.close socket # liftEffect
            pure unit
      )

calculateMessage :: Message -> Either String MessageResponse
calculateMessage { method: "isPrime", number: number } =
  case fromNumber number of
    Just num -> Right { method: "isPrime", prime: isPrime num }
    Nothing -> Right { method: "isPrime", prime: false }
calculateMessage {} =
  Left "malformed message"

listBinaryToIOData :: List Binary -> IOData
listBinaryToIOData = concat <<< map fromBinary

parseMessage :: IOData -> Array (Either (NonEmptyList ForeignError) Message)
parseMessage rawData = do
  rawData
    # toBinary
    # UnsafeCoerce.unsafeCoerce
    # (trim >>> split (Pattern "\n"))
    # map SimpleJson.readJSON

eitherEncodeMessageResponse :: Either String MessageResponse -> IOData
eitherEncodeMessageResponse (Right resp) = resp # SimpleJson.writeJSON # (\x -> x <> "\n") # fromString
eitherEncodeMessageResponse (Left err) = err # fromString

