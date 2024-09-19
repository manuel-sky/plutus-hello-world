{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module ClientTest where

import Cardano.Node.Emulator.Internal.Node (pNetworkId)
import Control.Monad (forever, void)
import Control.Monad.Freer.Extras.Log (LogLevel (Debug, Info))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Text qualified as T
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Ledger (CardanoAddress, PaymentPubKeyHash (unPaymentPubKeyHash), toPlutusAddress)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, Promise, endpoint, getParams, logInfo, selectList, submitTxConstraints,
                        submitTxConstraintsSpending, type (.\/), utxosAt)
import Plutus.Contract.Test (w1, w2)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (Address, ScriptContext (ScriptContext, scriptContextTxInfo), TxInfo (txInfoOutputs),
                             TxOut (TxOut, txOutAddress, txOutValue), Value)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, Maybe (Just, Nothing), Semigroup ((<>)), mapMaybe, mconcat, ($), (&&), (-), (.), (==),
                         (>=))
import Prelude (IO, (<$>), (>>))
import Prelude qualified as Haskell
import Wallet.Emulator.Stream (filterLogLevel)
import Wallet.Emulator.Wallet (Wallet, mockWalletAddress)
