{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ghengin.Core.Shader
  ( module Ghengin.Core.Shader
  , module Ghengin.Core.Shader.Canonical
  , module Ghengin.Core.Shader.Pipeline
  , type (FIR.:->), M.Value
  )
  where

import Graphics.Gl.Block
import Data.Kind
import GHC.TypeLits
import Ghengin.Core.Shader.Canonical
import Ghengin.Core.Shader.Pipeline
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Mat4
import Foreign.Ptr.Diff (pokeDiffOff, peekDiffOff)
import Foreign.Storable as Store

import Data.Word (Word16)
import Ghengin.Core.Prelude (Bool, Word32, Float, Generic, ($), undefined)

import Control.Monad.IO.Class (liftIO)

import Math.Linear
import qualified FIR
import qualified Data.Type.Map as M

import Ghengin.Core.Shader.Data

type VertexShaderModule defs
  = FIR.ShaderModule "main" FIR.VertexShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[] FIR.Vertex) ': CanonicalizeDefs defs)

type FragmentShaderModule defs
  = FIR.ShaderModule "main" FIR.FragmentShader
                     (("out_colour" 'FIR.:-> FIR.Output '[ FIR.Location 0 ] (V 4 Float)) ': ("main" 'FIR.:-> FIR.EntryPoint '[ FIR.OriginUpperLeft ] FIR.Fragment) ': CanonicalizeDefs defs)


-- * Instances for ShaderData
--
-- | The following instances are supposed to be used with deriving via:
--
-- Example
--
-- @
-- -- Internal type will be Struct [ "v" :-> V 3 Float ]
-- newtype CameraPos = CP Vec3 deriving ShaderData via (InStruct "v" Vec3)
-- @
--
-- There is also an instance of Syntactic for n-ary products of syntactic things like Mat and Vec,
-- so we can easily create instances for compound structs! (We need to use
-- generic here I think, since deriving via won't coerce between SOP and datatypes)
--
-- Example
-- 
-- @
--
-- @

instance ShaderData Bool where
  type FirType Bool = Bool

-- This needs to be merged for Word16 to work https://gitlab.com/dpwiz/gl-block/-/merge_requests/1
instance ShaderData Word16 where
  type FirType Word16 = Word16

instance ShaderData Word32 where
  type FirType Word32 = Word32

instance ShaderData Float where
  type FirType Float = Float

instance ShaderData Vec2 where
  type FirType Vec2 = V 2 Float

instance ShaderData Vec3 where
  type FirType Vec3 = V 3 Float

instance ShaderData Vec4 where
  type FirType Vec4 = V 4 Float

instance ShaderData Mat4 where
  type FirType Mat4 = M 4 4 Float

-- FIR Vector
instance (KnownNat n, Storable x, Block x) => Block (V n x) where
  type PackedSize (V n x) = n * (PackedSize x)
  isStruct _ = FIR.False

  alignment140 _ = Store.alignment (undefined :: V n x)
  alignment430 = alignment140

  sizeOf140 _ = Store.sizeOf (undefined :: V n x)
  sizeOf430 = sizeOf140
  sizeOfPacked = sizeOf140

  read140 p o = liftIO $ peekDiffOff p o
  read430 = read140
  readPacked = read140

  write140 p o a = liftIO $ pokeDiffOff p o a
  write430 = write140
  writePacked = write140

instance (KnownNat n, Block x, Storable x) => ShaderData (V n x) where
  type FirType (V n x) = V n x

-- FIR Matrix
instance (KnownNat m, KnownNat n, Storable x, Block x) => Block (M m n x) where
  type PackedSize (M m n x) = m * n * (PackedSize x)
  isStruct _ = FIR.False

  alignment140 _ = Store.alignment (undefined :: M m n x)
  alignment430 = alignment140

  sizeOf140 _ = Store.sizeOf (undefined :: M m n x)
  sizeOf430 = sizeOf140
  sizeOfPacked = sizeOf140

  read140 p o = liftIO $ peekDiffOff p o
  read430 = read140
  readPacked = read140

  write140 p o a = liftIO $ pokeDiffOff p o a
  write430 = write140
  writePacked = write140

instance (KnownNat m, KnownNat n, Block x, Storable x) => ShaderData (M m n x) where
  type FirType (M m n x) = M m n x
