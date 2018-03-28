module Shader
  ( Program
  , loadProgram
  , withProgram
  ) where

import qualified Data.ByteString as BS
import Foreign.C.String (peekCStringLen)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Graphics.GL

newtype Program = Program GLuint

getShaderIV :: Num a => GLuint -> GLenum -> IO a
getShaderIV shader info = alloca $ \ptr -> do
  glGetShaderiv shader info ptr
  fromIntegral <$> peek ptr

getProgramIV :: Num a => GLuint -> GLenum -> IO a
getProgramIV program info = alloca $ \ptr -> do
  glGetProgramiv program info ptr
  fromIntegral <$> peek ptr

getShaderLog :: GLuint -> IO String
getShaderLog shader = do
  len <- getShaderIV shader GL_INFO_LOG_LENGTH
  allocaBytes len $ \ptr -> do
    glGetShaderInfoLog shader (fromIntegral len) nullPtr ptr
    peekCStringLen (ptr, len)

shaderExtension :: GLenum -> String
shaderExtension GL_VERTEX_SHADER = "vert"
shaderExtension GL_FRAGMENT_SHADER = "frag"
shaderExtension type' = error $ "Unknwon shader type " ++ show type'

loadShader :: GLenum -> FilePath -> IO GLuint
loadShader type' path = do
  shader <- glCreateShader type'
  src <- BS.readFile path
  BS.useAsCStringLen src $ \(ptr', len) ->
    with ptr' $ \ptr ->
      with (fromIntegral len) $ \lenPtr ->
        glShaderSource shader 1 ptr lenPtr
  glCompileShader shader
  return shader

loadProgram :: String -> IO Program
loadProgram name = do
  program <- glCreateProgram

  vertShader <- loadShader GL_VERTEX_SHADER $ makePath GL_VERTEX_SHADER
  fragShader <- loadShader GL_FRAGMENT_SHADER $ makePath GL_FRAGMENT_SHADER

  glAttachShader program vertShader
  glAttachShader program fragShader
  glLinkProgram program

  return $ Program program
  where
    makePath type' = "shaders/" ++ name ++ "." ++ shaderExtension type'

withProgram :: Program -> (GLuint -> IO a) -> IO a
withProgram (Program pid) f = f pid
