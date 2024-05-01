-- | Useful orphan instances
--
-- This module is not re-exported from NejlaCommon to avoid
-- clashing instance definitions

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NejlaCommon.Orphans where

import           Control.Lens

import           Data.Data                  ( Proxy(..) )
import qualified Data.HashMap.Strict.InsOrd as InsOrdHMap
import qualified Data.OpenApi               as OpenApi

import           Servant                    ( (:>) )
import           Servant.Multipart          ( MultipartForm )
import           Servant.OpenApi            ( HasOpenApi(..), toOpenApi )

instance (HasOpenApi sub) => HasOpenApi (MultipartForm t k :> sub) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy sub)
    & OpenApi.allOperations . OpenApi.requestBody ?~ OpenApi.Inline reqBody
    where
      reqBody =
        mempty & OpenApi.required ?~ True & OpenApi.content
        .~ InsOrdHMap.fromList [ ("multipart/form-data", mempty) ]
