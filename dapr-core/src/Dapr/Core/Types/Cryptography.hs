{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Cryptography
-- Description : Defines the types used by Cryptography module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Cryptography module.
module Dapr.Core.Types.Cryptography where

import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'ComponentName' represents the name of the cryptography component
newtype ComponentName = ComponentName {getComponentName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'KeyName' represents the name or name/version of a key
newtype KeyName = KeyName {getKeyName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'KeyFormat' represents the format for key responses
data KeyFormat
  = KeyFormatPEM  -- ^ PEM (PKIX) format (default)
  | KeyFormatJSON -- ^ JSON (JSON Web Key) as string
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'SubtleGetKeyRequest' is the request for getting a public key
data SubtleGetKeyRequest = SubtleGetKeyRequest
  { -- | Name of the component
    getKeyComponentName :: ComponentName,
    -- | Name (or name/version) of the key to use in the key vault
    getKeyName :: KeyName,
    -- | Response format
    getKeyFormat :: KeyFormat
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleGetKeyRequest where
  toJSON = customToJSON 6

-- | 'SubtleGetKeyResponse' is the response for getting a public key
data SubtleGetKeyResponse = SubtleGetKeyResponse
  { -- | Name (or name/version) of the key
    responseKeyName :: KeyName,
    -- | Public key, encoded in the requested format
    responsePublicKey :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleGetKeyResponse where
  parseJSON = customParseJSON 8

-- | 'SubtleEncryptRequest' is the request for encrypting data
data SubtleEncryptRequest = SubtleEncryptRequest
  { -- | Name of the component
    encryptComponentName :: ComponentName,
    -- | Message to encrypt (base64 encoded)
    encryptPlaintext :: Text,
    -- | Algorithm to use, as in the JWA standard
    encryptAlgorithm :: Text,
    -- | Name (or name/version) of the key
    encryptKeyName :: KeyName,
    -- | Nonce / initialization vector (ignored with asymmetric ciphers)
    encryptNonce :: Maybe Text,
    -- | Associated Data when using AEAD ciphers (optional)
    encryptAssociatedData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleEncryptRequest where
  toJSON = customToJSON 7

-- | 'SubtleEncryptResponse' is the response for encrypting data
data SubtleEncryptResponse = SubtleEncryptResponse
  { -- | Encrypted ciphertext (base64 encoded)
    encryptedCiphertext :: Text,
    -- | Authentication tag (nil when not using an authenticated cipher)
    encryptedTag :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleEncryptResponse where
  parseJSON = customParseJSON 9

-- | 'SubtleDecryptRequest' is the request for decrypting data
data SubtleDecryptRequest = SubtleDecryptRequest
  { -- | Name of the component
    decryptComponentName :: ComponentName,
    -- | Message to decrypt (base64 encoded)
    decryptCiphertext :: Text,
    -- | Algorithm to use, as in the JWA standard
    decryptAlgorithm :: Text,
    -- | Name (or name/version) of the key
    decryptKeyName :: KeyName,
    -- | Nonce / initialization vector (ignored with asymmetric ciphers)
    decryptNonce :: Maybe Text,
    -- | Authentication tag (nil when not using an authenticated cipher)
    decryptTag :: Maybe Text,
    -- | Associated Data when using AEAD ciphers (optional)
    decryptAssociatedData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleDecryptRequest where
  toJSON = customToJSON 7

-- | 'SubtleDecryptResponse' is the response for decrypting data
data SubtleDecryptResponse = SubtleDecryptResponse
  { -- | Decrypted plaintext (base64 encoded)
    decryptedPlaintext :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleDecryptResponse where
  parseJSON = customParseJSON 9

-- | 'SubtleWrapKeyRequest' is the request for wrapping a key
data SubtleWrapKeyRequest = SubtleWrapKeyRequest
  { -- | Name of the component
    wrapKeyComponentName :: ComponentName,
    -- | Key to wrap (base64 encoded)
    wrapPlaintextKey :: Text,
    -- | Algorithm to use, as in the JWA standard
    wrapKeyAlgorithm :: Text,
    -- | Name (or name/version) of the key
    wrapKeyName :: KeyName,
    -- | Nonce / initialization vector (ignored with asymmetric ciphers)
    wrapKeyNonce :: Maybe Text,
    -- | Associated Data when using AEAD ciphers (optional)
    wrapKeyAssociatedData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleWrapKeyRequest where
  toJSON = customToJSON 7

-- | 'SubtleWrapKeyResponse' is the response for wrapping a key
data SubtleWrapKeyResponse = SubtleWrapKeyResponse
  { -- | Wrapped key (base64 encoded)
    wrappedKey :: Text,
    -- | Authentication tag (nil when not using an authenticated cipher)
    wrapKeyTag :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleWrapKeyResponse where
  parseJSON = customParseJSON 7

-- | 'SubtleUnwrapKeyRequest' is the request for unwrapping a key
data SubtleUnwrapKeyRequest = SubtleUnwrapKeyRequest
  { -- | Name of the component
    unwrapKeyComponentName :: ComponentName,
    -- | Wrapped key (base64 encoded)
    unwrapWrappedKey :: Text,
    -- | Algorithm to use, as in the JWA standard
    unwrapKeyAlgorithm :: Text,
    -- | Name (or name/version) of the key
    unwrapKeyName :: KeyName,
    -- | Nonce / initialization vector (ignored with asymmetric ciphers)
    unwrapKeyNonce :: Maybe Text,
    -- | Authentication tag (nil when not using an authenticated cipher)
    unwrapKeyTag :: Maybe Text,
    -- | Associated Data when using AEAD ciphers (optional)
    unwrapKeyAssociatedData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleUnwrapKeyRequest where
  toJSON = customToJSON 9

-- | 'SubtleUnwrapKeyResponse' is the response for unwrapping a key
data SubtleUnwrapKeyResponse = SubtleUnwrapKeyResponse
  { -- | Key in plaintext (base64 encoded)
    unwrappedPlaintextKey :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleUnwrapKeyResponse where
  parseJSON = customParseJSON 9

-- | 'SubtleSignRequest' is the request for signing data
data SubtleSignRequest = SubtleSignRequest
  { -- | Name of the component
    signComponentName :: ComponentName,
    -- | Digest to sign (base64 encoded)
    signDigest :: Text,
    -- | Algorithm to use, as in the JWA standard
    signAlgorithm :: Text,
    -- | Name (or name/version) of the key
    signKeyName :: KeyName
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleSignRequest where
  toJSON = customToJSON 4

-- | 'SubtleSignResponse' is the response for signing data
data SubtleSignResponse = SubtleSignResponse
  { -- | The signature that was computed (base64 encoded)
    computedSignature :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleSignResponse where
  parseJSON = customParseJSON 9

-- | 'SubtleVerifyRequest' is the request for verifying a signature
data SubtleVerifyRequest = SubtleVerifyRequest
  { -- | Name of the component
    verifyComponentName :: ComponentName,
    -- | Digest of the message (base64 encoded)
    verifyDigest :: Text,
    -- | Algorithm to use, as in the JWA standard
    verifyAlgorithm :: Text,
    -- | Name (or name/version) of the key
    verifyKeyName :: KeyName,
    -- | Signature to verify (base64 encoded)
    verifySignature :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubtleVerifyRequest where
  toJSON = customToJSON 6

-- | 'SubtleVerifyResponse' is the response for verifying a signature
data SubtleVerifyResponse = SubtleVerifyResponse
  { -- | True if the signature is valid
    signatureValid :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubtleVerifyResponse where
  parseJSON = customParseJSON 9

-- | 'EncryptRequestOptions' contains options for high-level encryption
data EncryptRequestOptions = EncryptRequestOptions
  { -- | Name of the component (required)
    encryptOptionsComponentName :: ComponentName,
    -- | Name (or name/version) of the key (required)
    encryptOptionsKeyName :: KeyName,
    -- | Key wrapping algorithm to use (required)
    encryptOptionsKeyWrapAlgorithm :: Text,
    -- | Cipher used to encrypt data (optional): "aes-gcm" (default) or "chacha20-poly1305"
    encryptOptionsDataEncryptionCipher :: Maybe Text,
    -- | If true, the encrypted document does not contain a key reference
    encryptOptionsOmitDecryptionKeyName :: Bool,
    -- | Key reference to embed in the encrypted document
    encryptOptionsDecryptionKeyName :: Maybe KeyName
  }
  deriving (Eq, Show, Generic)

instance ToJSON EncryptRequestOptions where
  toJSON = customToJSON 14

-- | 'DecryptRequestOptions' contains options for high-level decryption
data DecryptRequestOptions = DecryptRequestOptions
  { -- | Name of the component
    decryptOptionsComponentName :: ComponentName,
    -- | Name (or name/version) of the key to decrypt the message
    decryptOptionsKeyName :: Maybe KeyName
  }
  deriving (Eq, Show, Generic)

instance ToJSON DecryptRequestOptions where
  toJSON = customToJSON 14
