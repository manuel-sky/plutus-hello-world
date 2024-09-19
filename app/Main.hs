{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClientValidator
import Data.ByteString qualified as B
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as B
import PlutusLedgerApi.V2 qualified as V2
import Text.Hex (Text, ByteString, decodeHex)

{-
challenge: "hello world" (UTF-8)

priv, pub, sig:

E2C7780283201AECD3E17DF4649D36EE9D60F650DCFB79EFB38A588F37F5C471
43004B8F43FAF0E3EAAAF55BB41DC53CBF09E42884267BAF8C1EA6903819122C
B48CD89C3CCA7DD334647C3F54816E52DA0002618DA55803CD524D7ABF272EE8CE771D30243772FBA0032B550A4004B0D0A978E863BCC85B02C94726EBBA970E

03A4BEF77A4904CF25EC8F815FCC6D5EF91609644187926D1F28DCDED7DE89C0
DE3B4832CC5DD4B8412926C23BDCDFD4503E808A37B069A7C42DE8749AF23D81
6D00BEE751616589F9E243B83E49218F23158E8878A6FD62500A3E10A967A2A4ED41B24E7DC96E088CAC5AB1EEEAA5EFE5A36B864EE6FF0FCFD5E2D710E3F509
-}

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

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
