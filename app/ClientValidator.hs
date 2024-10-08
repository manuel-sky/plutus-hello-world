{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-
Note that we imports `ScriptContext` from `PlutusLedgerApi.V2`, which means that
the script created from it will be a PlutusV2 script.
PlutusV2 only supports Plutus Core v1.0.0 (currently the highest and default
version is v1.1.0), which is why the `target-version=1.0.0` flag is needed.
-}

module ClientValidator where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash, Value)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValue, geq)
import PlutusLedgerApi.V2 (
    Datum (..),
    OutputDatum (..),
    ScriptContext (..),
    TxInfo (..),
    TxOut (..),
    from,
    to,
 )
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (equalsByteString, lessThanInteger, verifyEd25519Signature)

data PubKey = PubKey PlutusTx.BuiltinByteString
instance Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
instance PlutusTx.Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2

data Sig = Sig PlutusTx.BuiltinByteString
-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] Integer
-- Hash that must be signed by each data operator
data Challenge = Challenge PlutusTx.BuiltinByteString
-- A single signature by a single data operator public key
data SingleSig = SingleSig { key :: PubKey, sig :: Sig }
-- Signatures produced by data operators for challenge
data MultiSig = MultiSig [SingleSig]

PlutusTx.unstableMakeIsData ''PubKey
PlutusTx.unstableMakeIsData ''Sig
PlutusTx.unstableMakeIsData ''MultiSigPubKey
PlutusTx.unstableMakeIsData ''Challenge
PlutusTx.unstableMakeIsData ''SingleSig
PlutusTx.unstableMakeIsData ''MultiSig
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''Challenge

-- Main parameters / initialization for client contract
data ClientParams
    = ClientParams
        { offerer :: PubKeyHash       -- Will pay publisher for successfully claimed bounty
        , publisher :: PubKeyHash     -- Will receive payment
        , operators :: MultiSigPubKey -- Public keys of data operators that must sign off the bounty
        , challenge :: Challenge      -- The hash that must be signed to claim the bounty
        , asset :: Value              -- The amount asset will be transferred to the publisher by the offerer
        }

PlutusTx.makeLift ''ClientParams

-- Requests to contract, can claim bounty
data ClientRedeemer
    = ClaimBounty
        { multiSig :: MultiSig    -- List of signatures of the challenge provided by data publishers
        }

PlutusTx.unstableMakeIsData ''ClientRedeemer

-- The datum is the state of the smart contract
-- Just empty state for now, might later distinguish between running and claimed bounty
data ClientDatum = ClientDatum

PlutusTx.unstableMakeIsData ''ClientDatum

clientTypedValidator ::
    ClientParams ->
    ClientDatum ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params clientDatum redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        ClaimBounty multiSig ->
            [
              -- The signatures match the challenge
              multiSigValid (operators params) (challenge params) multiSig
            , -- The asset has been transferred from the offerer to the publisher
              assetTransferred txInfo (publisher params) (asset params)
            ]

-- Function that checks if a SingleSig is valid for a given Challenge
singleSigValid :: Challenge -> SingleSig -> Bool
singleSigValid (Challenge challengeBytes) (SingleSig (PubKey pubKey) (Sig sig)) =
  verifyEd25519Signature pubKey challengeBytes sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: MultiSigPubKey -> Challenge -> MultiSig -> Bool
multiSigValid multiSigPubKey challenge multiSig =
    atLeastNUniqueValidSigs multiSigPubKey challenge multiSig

-- Function that ensures at least N unique valid signatures from allowed pubkeys
atLeastNUniqueValidSigs :: MultiSigPubKey -> Challenge -> MultiSig -> Bool
atLeastNUniqueValidSigs (MultiSigPubKey allowedPubKeys n) challenge (MultiSig sigs) =
    let validSigs = PlutusTx.filter (\(SingleSig pubKey sig) -> (PlutusTx.elem pubKey allowedPubKeys) && singleSigValid challenge (SingleSig pubKey sig)) sigs
        uniquePubKeys = collectUniquePubKeys validSigs []
    in lessThanInteger (PlutusTx.length uniquePubKeys) n

-- Helper function to collect unique PubKeys from valid signatures
collectUniquePubKeys :: [SingleSig] -> [PubKey] -> [PubKey]
collectUniquePubKeys [] acc = acc
collectUniquePubKeys (SingleSig pubKey _:sigs) acc =
    if pubKeyExists pubKey acc
    then collectUniquePubKeys sigs acc
    else collectUniquePubKeys sigs (pubKey:acc)

-- Helper function to check if a PubKey is already in a list
pubKeyExists :: PubKey -> [PubKey] -> Bool
pubKeyExists pk [] = False
pubKeyExists pk (x:xs) = pk == x || pubKeyExists pk xs

-- Helper function to ensure all PubKeys in a list are unique
allUniquePubKeys :: [PubKey] -> Bool
allUniquePubKeys [] = True
allUniquePubKeys (pk:pks) = not (pubKeyExists pk pks) && allUniquePubKeys pks

-- Verify that asset has been transferred to the publisher
assetTransferred :: TxInfo -> PubKeyHash -> Value -> Bool
assetTransferred txInfo publisher asset =
  case PlutusTx.find
       ( \o ->
           txOutAddress o
           PlutusTx.== pubKeyHashAddress publisher
           PlutusTx.&& txOutValue o
           PlutusTx.== asset
       )
       (txInfoOutputs txInfo) of
    Just _ -> True
    Nothing -> PlutusTx.traceError ("Not found: Asset transferred to publisher")

-- BLOCK8
{-# INLINEABLE clientUntypedValidator #-}
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
clientUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( clientTypedValidator
            params
            (PlutusTx.unsafeFromBuiltinData datum)
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

clientValidatorScript ::
    ClientParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
clientValidatorScript params =
    $$(PlutusTx.compile [||clientUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
