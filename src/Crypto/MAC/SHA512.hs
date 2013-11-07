{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Crypto.MAC.SHA512
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- This module implements minimal bindings to HMAC-SHA-512-256,
-- i.e. the first 256 bits of HMAC-SHA-512. The underlying
-- implementation is the @ref@ code of @hmacsha512256@ from SUPERCOP,
-- and should be relatively fast.
--
module Crypto.MAC.SHA512
       ( Key          -- :: *
       , key          -- :: *
       , Auth(..)     -- :: *
       , authenticate -- :: ByteString -> ByteString -> Maybe Auth
       , verify       -- :: ByteString -> Auth -> ByteString -> Maybe Bool
       ) where
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           System.IO.Unsafe         (unsafePerformIO)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import           Data.ByteString.Internal (create)
import           Data.ByteString.Unsafe

newtype Key = Key ByteString
  deriving (Eq, Show, Ord)

key :: ByteString -> Maybe Key
key xs | S.length xs /= hmacsha512256KEYBYTES = Nothing
       | otherwise = Just (Key xs)

-- | An authenticator.
newtype Auth = Auth { unAuth :: ByteString }
  deriving (Eq, Show, Ord)

authenticate :: Key
             -- ^ Secret key
             -> ByteString
             -- ^ Message
             -> Auth
             -- ^ Authenticator
authenticate (Key k) msg = Auth $
  unsafePerformIO . create hmacsha512256BYTES $ \out ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk ->
        c_crypto_hmacsha512256 out cstr (fromIntegral clen) pk >> return ()
{-# INLINEABLE authenticate #-}

verify :: Key
       -- ^ Secret key
       -> Auth
       -- ^ Authenticator returned via 'authenticateOnce'
       -> ByteString
       -- ^ Message
       -> Bool
       -- ^ Result: @True@ if verified, @False@ otherwise
verify (Key k) (Auth auth) msg =
  unsafePerformIO . unsafeUseAsCString auth $ \pauth ->
    unsafeUseAsCStringLen msg $ \(cstr, clen) ->
      unsafeUseAsCString k $ \pk -> do
        b <- c_crypto_hmacsha512256_verify pauth cstr (fromIntegral clen) pk
        return (b == 0)
{-# INLINE verify #-}

--
-- FFI mac binding
--

hmacsha512256KEYBYTES :: Int
hmacsha512256KEYBYTES = 32

hmacsha512256BYTES :: Int
hmacsha512256BYTES = 32

foreign import ccall unsafe "sha512256_hmac"
  c_crypto_hmacsha512256 :: Ptr Word8 -> Ptr CChar -> CULLong ->
                          Ptr CChar -> IO Int

foreign import ccall unsafe "sha512256_hmac_verify"
  c_crypto_hmacsha512256_verify :: Ptr CChar -> Ptr CChar -> CULLong ->
                                 Ptr CChar -> IO Int
