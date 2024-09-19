{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClientValidator
import Data.ByteString qualified as B
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as B
import PlutusLedgerApi.V2 qualified as V2

main :: IO ()
main = B.writeFile "validator.uplc" . Base16.encode $ B.fromShort serialisedScript
  where
    script = clientValidatorScript params
    serialisedScript = V2.serialiseCompiledCode script
    params =
        ClientParams
            { offerer = V2.PubKeyHash "addr_test1vqe09nt0rxgwn83upxuhqzs4aqrzdjqmhrh5l4g5hh4kc6qsncmku"
            , publisher = V2.PubKeyHash "addr_test1vqe09nt0rxgwn83upxuhqzs4aqrzdjqmhrh5l4g5hh4kc6qsncmku"
            , operators = MultiSigPubKey [] 0
            , asset = V2.singleton V2.adaSymbol V2.adaToken 10000
            , challenge = Challenge (stringToBuiltinByteString "foo")
            }
