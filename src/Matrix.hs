module Matrix where

import Foreign.Ptr (Ptr)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

newtype Matrix =
  Matrix (Vector Float)
  deriving (Show)

identity :: Matrix
identity = Matrix . V.fromList $
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 1, 0
  , 0, 0, 0, 1
  ]

perspective :: Float -> Float -> Float -> Float -> Matrix
perspective fovy aspect near far = Matrix . V.fromList $
  [ f / aspect, 0,                   0,  0
  ,          0, f,                   0,  0
  ,          0, 0,   (far + near) * nf, -1
  ,          0, 0, 2 * far * near * nf,  0
  ]
  where
    f = 1 / tan (fovy / 2)
    nf = 1 / (near - far)

translate :: Float -> Float -> Float -> Matrix -> Matrix
translate x y z (Matrix mat) = Matrix . V.fromList $
  [ a00, a01, a02, a03
  , a10, a11, a12, a13
  , a20, a21, a22, a23
  , a00 * x + a10 * y + a20 * z + a30
  , a01 * x + a11 * y + a21 * z + a31
  , a02 * x + a12 * y + a22 * z + a32
  , a03 * x + a13 * y + a23 * z + a33
  ]
  where
    (a00, a01, a02, a03) = (mat V.!  0, mat V.!  1, mat V.!  2, mat V.!  3)
    (a10, a11, a12, a13) = (mat V.!  4, mat V.!  5, mat V.!  6, mat V.!  7)
    (a20, a21, a22, a23) = (mat V.!  8, mat V.!  9, mat V.! 10, mat V.! 11)
    (a30, a31, a32, a33) = (mat V.! 12, mat V.! 13, mat V.! 14, mat V.! 15)

unsafeWith :: Matrix -> (Ptr Float -> IO b) -> IO b
unsafeWith (Matrix mat) f = V.unsafeWith mat f