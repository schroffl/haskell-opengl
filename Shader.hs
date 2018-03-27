module Shader
  ( Program
  , loadProgram
  , withProgram
  ) where

import Foreign.Marshal.Utils (with)
import qualified Data.ByteString as BS
import Graphics.GL

newtype Program = Program GLuint

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

withProgram :: (GLuint -> IO a) -> Program-> IO a
withProgram f (Program pid) = f pid
