module Chunk 
  ( Chunk(..)
  , LOD(..)
  , setup
  , draw
  , chunkSize
  )
  where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Numeric.Noise (Noise, noiseValue)
import qualified Numeric.Noise.Perlin as Perlin
import qualified Util
import Graphics.GL

data Chunk = Chunk
  { chunkX :: Int
  , chunkZ :: Int
  , chunkVertexArray :: GLuint
  , chunkVertexBuffer :: GLuint
  , chunkIndexBuffer :: GLuint
  , chunkIndexCount :: Int
  } deriving (Show)

type Vertex = (Float, Float, Float)

type Triangle = (Int, Int, Int)

data LOD
  = LOD'High
  | LOD'Medium
  | LOD'Low
  | LOD'VeryLow
  deriving (Show, Enum)

withLen :: (Storable a, Num b) => Vector a -> ((Ptr a, b) -> IO c) -> IO c
withLen vec f = V.unsafeWith vec $ \ptr -> f (ptr, len)
  where
    len = fromIntegral $ sizeOf (vec V.! 0) * V.length vec 

setup :: LOD -> Int -> Int -> IO Chunk
setup lod cx cz = do
  vao <- Util.makeVAO
  glBindVertexArray vao

  vertexBuffer <- Util.makeBuffer
  glBindBuffer GL_ARRAY_BUFFER vertexBuffer

  withLen (verts :: Vector GLfloat) $ \(ptr, len) -> do
    glBufferData GL_ARRAY_BUFFER len ptr GL_STATIC_DRAW

  indexBuffer <- Util.makeBuffer
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer

  withLen (indices :: Vector GLuint) $ \(ptr, len) ->
    glBufferData GL_ELEMENT_ARRAY_BUFFER len ptr GL_STATIC_DRAW

  return $ Chunk 
    { chunkX = cx
    , chunkZ = cz
    , chunkVertexArray = vao
    , chunkVertexBuffer = vertexBuffer
    , chunkIndexBuffer = indexBuffer
    , chunkIndexCount = V.length indices
    }
  where
    noise = Perlin.perlin 35318 5 0.05 0.4
    flatten = foldl (\l (a, b, c) -> a : b : c : l) []
    verts = V.fromList . flatten $ vertices noise lod cx cz
    indices = V.fromList . map fromIntegral . flatten $ triangles lod

draw :: Chunk -> IO ()
draw chunk = do
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES indexCount GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0
  where
    vao = chunkVertexArray chunk
    indexCount = fromIntegral $ chunkIndexCount chunk

chunkSize :: Num a => a
chunkSize = 16

stepSize :: Fractional a => LOD -> a
stepSize LOD'High = 0.5
stepSize lod = stepSize (pred lod) * 2

stepper :: (Fractional a, Enum a) => LOD -> [a]
stepper lod = [0, stepSize lod .. chunkSize]

vertices :: Noise n => n -> LOD -> Int -> Int -> [Vertex]
vertices noise lod cx cz = concat $ map f xs
  where
    f x = map (vertexAt noise x) zs
    offsetX = fromIntegral $ cx * chunkSize
    offsetZ = fromIntegral $ cz * chunkSize
    xs = map (+offsetX) $ stepper lod
    zs = map (+offsetZ) $ stepper lod

vertexAt :: Noise n => n -> Float -> Float -> Vertex
vertexAt noise x z = (x, realToFrac height, z)
  where
    x' = realToFrac x
    z' = realToFrac z
    height = noiseValue noise (x', 0, z')

triangles :: LOD -> [Triangle]
triangles lod = foldl (\l (a, b) -> a : b : l) [] . concat . map f $ xs
  where
    f x = map (trianglesAt lod x) zs
    w = chunkSize / stepSize lod - 1
    xs = [0 .. w]
    zs = [0 .. w]

trianglesAt :: LOD -> Float -> Float -> (Triangle, Triangle)
trianglesAt lod x z = 
  ( (floor first, floor $ first + 1, floor $ first + w)
  , (floor $ first + 1, floor $ first + w + 1, floor $ first + w)
  )
  where
    w = chunkSize / stepSize lod + 1
    first = x + z * w
