module Util where

import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (malloc)
import Graphics.GL

makeBuffer :: IO GLuint
makeBuffer = do
  bufPtr <- malloc
  glGenBuffers 1 bufPtr
  peek bufPtr

makeVAO :: IO GLuint
makeVAO = do
  vaoPtr <- malloc
  glGenVertexArrays 1 vaoPtr
  peek vaoPtr