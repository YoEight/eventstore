{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Database.EventStore.Internal.Manager.Operation
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Database.EventStore.Internal.Manager.Operation
    ( Decision(..)
    , OperationParams(..)
    , operationNetwork
    ) where

--------------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Word

--------------------------------------------------------------------------------
import Data.ProtocolBuffers
import Data.Serialize
import Data.UUID
import FRP.Sodium
import System.Random

--------------------------------------------------------------------------------
import Database.EventStore.Internal.Types hiding (Event, newEvent)
import Database.EventStore.Internal.Util.Sodium

--------------------------------------------------------------------------------
newtype Manager = Manager (M.Map UUID Operation)

--------------------------------------------------------------------------------
initManager :: Manager
initManager = Manager M.empty

--------------------------------------------------------------------------------
-- Operation
--------------------------------------------------------------------------------
data Decision
    = DoNothing
    | EndOperation
    | Retry
    | Reconnection
    | Subscribed

--------------------------------------------------------------------------------
data Operation
    = Operation
      { operationCreatePackage :: UUID    -> IO Package
      , operationInspect       :: Package -> IO Decision
      }

--------------------------------------------------------------------------------
data OperationParams
    = forall req resp. (Encode req, Decode resp) =>
      OperationParams
      { opSettings    :: !Settings
      , opRequestCmd  :: !Word8
      , opResponseCmd :: !Word8

      , opRequest     :: IO req
      , opSuccess     :: resp -> IO Decision
      , opFailure     :: OperationException -> IO Decision
      }

--------------------------------------------------------------------------------
createOperation :: OperationParams -> Operation
createOperation params =
    Operation
    { operationCreatePackage = createPackage params
    , operationInspect       = inspection params
    }

--------------------------------------------------------------------------------
createPackage :: OperationParams -> UUID -> IO Package
createPackage OperationParams{..} uuid = do
    req <- opRequest

    let pack = Package
               { packageCmd         = opRequestCmd
               , packageCorrelation = uuid
               , packageFlag        = None
               , packageData        = runPut $ encodeMessage req
               }

    return pack

--------------------------------------------------------------------------------
inspection :: OperationParams -> Package -> IO Decision
inspection params@OperationParams{..} pack
    | found == exp_v = deeperInspection params pack
    | otherwise      = failed (InvalidServerResponse exp_v found)
  where
    exp_v  = opResponseCmd
    failed = opFailure
    found  = packageCmd pack

--------------------------------------------------------------------------------
deeperInspection :: OperationParams -> Package -> IO Decision
deeperInspection OperationParams{..} pack =
    case runGet decodeMessage bytes of
        Left e    -> failed (ProtobufDecodingError e)
        Right msg -> succeed msg
  where
    failed  = opFailure
    succeed = opSuccess
    bytes   = packageData pack

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------
data Register = Register UUID Operation

newtype Remove = Remove UUID

data Response = Response !Package !Operation

--------------------------------------------------------------------------------
operationNetwork :: (Package -> Reactive ())
                 -> Reactive ()
                 -> Event Package
                 -> Reactive (OperationParams -> Reactive ())
operationNetwork push_pkg push_reco e_pkg = do
    (on_new, push_new) <- newEvent
    (on_reg, push_reg) <- newEvent
    (on_rem, push_rem) <- newEvent
    (on_ret, push_ret) <- newEvent

    let mgr_e = fmap register on_reg <>
                fmap remove on_rem

    mgr_b <- accum initManager mgr_e

    let resp_e = filterJust $ snapshot response e_pkg mgr_b

        on_new_op = fmap createOperation on_new <> on_ret

        push_reg_io   = pushAsync2 $ \uuid op -> push_reg $ Register uuid op
        push_rem_io   = pushAsync (push_rem . Remove)
        push_retry_io = pushAsync2 $ \uuid op -> do
            push_rem $ Remove uuid
            push_ret op
        push_reco_io  = pushAsync2 $ \uuid op -> do
            push_reco
            push_rem $ Remove uuid
            push_ret op
        push_send_io  = pushAsync push_pkg

    _ <- listen on_new_op $ \op -> do
             uuid <- randomIO
             push_reg_io uuid op

    _ <- listen resp_e $ \(Response pkg op) -> do
             decision <- operationInspect op pkg
             let corr_id = packageCorrelation pkg

             case decision of
                 DoNothing    -> return ()
                 EndOperation -> push_rem_io corr_id
                 Retry        -> push_retry_io corr_id op
                 Reconnection -> push_reco_io corr_id op
                 _            -> fail unexpectedDecision

    _ <- listen on_reg $ \(Register uuid op) ->
             operationCreatePackage op uuid >>= push_send_io

    return push_new

--------------------------------------------------------------------------------
unexpectedDecision :: String
unexpectedDecision = "Unexpected decision Processor.handlingOperation"

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------
register :: Register -> Manager -> Manager
register (Register uuid op) (Manager m) = Manager $ M.insert uuid op m

--------------------------------------------------------------------------------
remove :: Remove -> Manager -> Manager
remove (Remove uuid) (Manager m) = Manager $ M.delete uuid m

--------------------------------------------------------------------------------
-- Snapshot
--------------------------------------------------------------------------------
response :: Package -> Manager -> Maybe Response
response pkg (Manager m) = fmap (Response pkg) $ M.lookup corr_id m
  where
    corr_id = packageCorrelation pkg
