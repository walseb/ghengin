{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints, DeriveAnyClass, UndecidableInstances #-}
-- | This module defines the main class which powers all of the serialization
-- and compatibility logic between CPU and GPU data types.
module Ghengin.Core.Shader.Data
  ( ShaderData(..)
  , InStruct(..)
    -- ** Re-exports
  , Poke(..), Layout(..)
  ) where

import Prelude
import GHC.TypeLits
import GHC.Generics
import Data.Kind
import Graphics.Gl.Block as Bl
import qualified FIR as FIR
import FIR (Array)
import FIR.Layout as L

import Foreign.Storable as Store
import Foreign.Ptr.Diff (pokeDiffOff, peekDiffOff, Diff(Diff))
import qualified Data.Vector.Sized as V
import qualified Data.Vector as Vu
import Foreign.Ptr (plusPtr, castPtr, Ptr)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

import Data.Word (Word16, Word32)
import Data.Int (Int32)
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Mat4
import Geomancy.Transform
import Math.Linear (M, V, unfold)
import Control.Monad.IO.Class
import qualified Data.Vector.Generic as VG

-- | The class which powers all of the serialization and compatibility logic
-- between CPU and GPU data types.
--
-- If a type instances 'ShaderData', it means it can be serialized according to
-- the [Shader Memory Layout](https://docs.vulkan.org/guide/latest/shader_memory_layout.html) into
-- a shader datatype such as @V 3 Float@, which has a matching
-- [SPIRV type](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types).
class ShaderData ty where

  -- | The primitive FIR shader type whose memory representation matches the result
  -- of serializing this datatype using 'Poke'. This is the promise that if
  -- your shader expects @FirType ty@ in a uniform/storage location, writing @ty@
  -- into the buffer will be sound, and the shader will find @ty@'s laid out 
  -- in memory according to @FirType ty@'s expected memory layout.
  --
  -- === _Example_
  --
  -- @
  -- instance FirType Vec3 where
  --   type FirType Vec3 = V 3 Float
  --
  -- instance FirType Mat4 where
  --   type FirType Mat4 = M 4 4 Float
  --
  -- instance FirType ... where
  --   type FirType ... = Struct ...
  -- @
  type family FirType (ty :: Type) :: Type

  -- ROMES:TODO: Perhaps we could instead have a family whose return kind is
  -- lifted 'SPIRV.PrimTy', and comparing that is easy (as long as we implement
  -- ShaderData for the common shader datatypes such as V 3 Float).
  -- Though that is quite considerably more burdensome (e.g. images, decorations...)
  -- Not sure if would be better.
  -- type family SpirType ty :: 'SPIRV.PrimTy

  -- | A proof that that the @Poke/SizeOf@ of the "primitive" FIR shader type
  -- matches the @Block/PackedSize@ of the CPU-level datatype. Even though this
  -- isn't a full blown proof that the types are compatible with respect to the
  -- expected memory layout by the graphics pipeline/shader, it nudges use of
  -- 'ShaderData' in the right direction.
  --
  -- Possibly, the greatest benefit for now is guaranteeing the type result of
  -- 'FirType' is actually a FIR primitive shader type, since we only have Poke
  -- instances for those.
  --
  -- ROMES:TODO: I'm not sure how it will work with images... they don't instance 'Poke'.
  -- proofSameSize :: PackedSize ty :~: PackedSize (FirType ty)

--------------------------------------------------------------------------------
-- * 'InStruct'
--------------------------------------------------------------------------------

-- | A utility to wrap any type @a@ in a Struct with a single @field@ mapping to @FirType a@
newtype InStruct (field :: Symbol) a = InStruct a
  deriving stock Generic

deriving anyclass instance (KnownNat (PackedSize a), Block a) => Block (InStruct field a)

instance (KnownNat (PackedSize a), Block a) => ShaderData (InStruct field a) where
  type FirType (InStruct field a) = FIR.Struct '[ field 'FIR.:-> FirType a ]

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

instance ShaderData Int32 where
  type FirType Int32 = Int32

instance ShaderData Float where
  type FirType Float = Float

instance ShaderData Double where
  type FirType Double = Double

instance ShaderData Vec2 where
  type FirType Vec2 = V 2 Float

instance ShaderData Vec3 where
  type FirType Vec3 = V 3 Float

instance ShaderData Vec4 where
  type FirType Vec4 = V 4 Float

instance ShaderData Mat4 where
  type FirType Mat4 = M 4 4 Float

instance ShaderData Transform where
  type FirType Transform = M 4 4 Float


-- -- FIR Vector
-- instance (KnownNat n, Block x) => Block (V n x) where
--   type PackedSize (V n x) = n * (PackedSize x)
--   isStruct _ = FIR.False

--   alignment140 _ = alignment140 (Proxy @x)
--   alignment430 _ = alignment430 (Proxy @x)

--   sizeOf140 _ = Bl.roundUp (sizeOf140 (Proxy :: Proxy x)) (alignment140 (Proxy :: Proxy x)) * n
--     where n = fromIntegral $ natVal (Proxy @n)
--   sizeOf430 _ = Bl.roundUp (sizeOf430 (Proxy :: Proxy x)) (alignment430 (Proxy :: Proxy x)) * n
--     where n = fromIntegral $ natVal (Proxy @n)
--   sizeOfPacked = sizeOf140

--   read140 p (Diff o) =
--     liftIO $ traverse (\o -> (read140 p (Diff o))) ixVec
--           where ixVec :: V n Int
--                 ixVec = unfold pred (fromIntegral $ natVal (Proxy @n))
--   read430 = read140
--   readPacked = read140

--   write140 p o a =
--     liftIO $ do
--       traverse ((\o -> ((write140 p (Diff o) a) >> pure ()))) ixVec
--       pure ()
--           where ixVec :: V n Int
--                 ixVec = unfold pred (fromIntegral $ natVal (Proxy @n))
--   write430 = write140
--   writePacked = write140

instance (KnownNat n, Storable x, Block x) => Block (V n x) where
  type PackedSize (V n x) = n * (PackedSize x)
  isStruct _ = FIR.False

  alignment140 _ = alignment140 (Proxy @x)
  alignment430 _ = alignment430 (Proxy @x)

  sizeOf140 _ = Bl.roundUp (sizeOf140 (Proxy :: Proxy x)) (alignment140 (Proxy :: Proxy x)) * n
    where n = fromIntegral $ natVal (Proxy @n)
  sizeOf430 _ = Bl.roundUp (sizeOf430 (Proxy :: Proxy x)) (alignment430 (Proxy :: Proxy x)) * n
    where n = fromIntegral $ natVal (Proxy @n)
  sizeOfPacked = sizeOf140

  read140 p o = liftIO $ peekDiffOff p o
  read430 = read140
  readPacked = read140

  write140 p o a = liftIO $ pokeDiffOff p o a
  write430 = write140
  writePacked = write140

instance (KnownNat n, Block x, Storable x) => ShaderData (V n x) where
  type FirType (V n x) = V n (FirType x)

-- FIR Array
-- It's a newtype of the FIR Vector. It differs in that it generates different SPIRV code, allowing for types such as 'Array 50 (M 4 4 Float)', which Vectors don't support (as they only support scalars, meaning Float, Int, etc, not complex types)
-- See the 'layoutable' function in FIR.Layout for more information, as well as the SPIRV docs for Array
-- WARNING: Because the layoutable function seems to has some funky alignment code depending for arrays, deriving the storable instance of Vector like this might not be correct, but it seems to work fine.
deriving anyclass instance (Generic a) => Generic (Array n a)
-- deriving newtype instance (KnownNat n, Block a) => Block (Array n a) 
-- deriving via (V n a) instance (KnownNat n, Block a) => Block (Array n a)


-- deriving newtype instance (Storable a, KnownNat n) => Storable (FIR.Array n a)

instance (KnownNat n, Block x) => Block (FIR.Array n x) where
  type PackedSize (FIR.Array n x) = n * (PackedSize x)
  isStruct _ = False

  alignment140 _ = alignment140 (Proxy @x)
  alignment430 _ = alignment430 (Proxy @x)

  sizeOf140 _ = Bl.roundUp (sizeOf140 (Proxy :: Proxy x)) (alignment140 (Proxy :: Proxy x)) * n
    where n = fromIntegral $ natVal (Proxy @n)
  sizeOf430 _ = Bl.roundUp (sizeOf430 (Proxy :: Proxy x)) (alignment430 (Proxy :: Proxy x)) * n
    where n = fromIntegral $ natVal (Proxy @n)
  sizeOfPacked = sizeOf140

  read140 p (Diff o) =
    liftIO $ traverse (\o' -> ((read140 :: MonadIO m => Ptr a -> Diff a x -> m x) p (Diff (o + o')))) ixVec
          where ixVec :: Array n Int
                ixVec = FIR.MkArray (fromMaybe (fromMaybe (error "Impossible!") (V.fromList [])) (V.fromList [0..((fromIntegral $ natVal (Proxy @n)) - 1)]))
  read430 = read140
  readPacked = read140

  -- write140 p (Diff o) a = liftIO $ (Store.poke (p `plusPtr` o)) a
  write140 p (Diff o) v = liftIO $ Vu.iforM_ (V.fromSized (((FIR.coerce) v) :: V.Vector n x)) \i -> write140 p (Diff (o + i*d)) where

    d = Bl.roundUp (sizeOf140 (Proxy :: Proxy x)) (alignment140 (Proxy :: Proxy x))
  write430 = write140
  writePacked = write140

-- instance (KnownNat n, Block x) => Block (Array n x) where
--   type PackedSize (Array n x) = n * (PackedSize x)
--   isStruct _ = False

--   alignment140 _ = alignment140 (Proxy @x)
--   alignment430 = alignment140

--   sizeOf140 _ = sizeOf140 (Proxy @x)
--   sizeOf430 = sizeOf140
--   sizeOfPacked = sizeOf140

--   read140 p (Diff o) =
--     liftIO $ traverse (\o -> (read140 p (Diff o))) ixVec
--           where ixVec :: Array n Int
--                 ixVec = FIR.MkArray (fromMaybe (fromMaybe (error "Impossible!") (V.fromList [])) (V.fromList [0..((fromIntegral $ natVal (Proxy @n)) - 1)]))
--   read430 = read140
--   readPacked = read140

--   write140 p o a =
--     liftIO $ do
--       traverse ((\o -> ((write140 p (Diff o) a) >> pure ()))) ixVec
--       pure ()
--           where ixVec :: Array n Int
--                 ixVec = FIR.MkArray (fromMaybe (fromMaybe (error "Impossible!") (V.fromList [])) (V.fromList [0..((fromIntegral $ natVal (Proxy @n)) - 1)]))
--   write430 = write140
--   writePacked = write140

instance (KnownNat n, Block x) => ShaderData (Array n x) where
  type FirType (Array n x) = Array n (FirType x)
  
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
  type FirType (M m n x) = M m n (FirType x)
