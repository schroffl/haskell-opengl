module Main where

import Control.Monad (guard, unless, when)
import Foreign.Ptr (nullPtr)
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Shader
import qualified Matrix as Mat
import qualified Chunk
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Bits ((.|.))

onResize :: GLFW.Window -> Int -> Int -> IO ()
onResize _ w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  Mat.unsafeWith (projection w h) $ glUniformMatrix4fv 0 1 GL_FALSE

main :: IO ()
main = do
  guard =<< GLFW.init

  let (w, h) = (640, 480)

  Just win <- GLFW.createWindow w h "" Nothing Nothing

  GLFW.makeContextCurrent $ Just win
  GLFW.setWindowSize win w h
  GLFW.setWindowSizeCallback win $ Just onResize

  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS

  glViewport 0 0 (fromIntegral w) (fromIntegral h)

  program <- Shader.loadProgram "Basic"

  chunk <- Chunk.setup Chunk.LOD'Medium 0 0

  Shader.withProgram program $ \p -> do
    glUseProgram p

    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 3 GL_FLOAT 0 0 nullPtr

  Mat.unsafeWith (projection w h) $ glUniformMatrix4fv 0 1 GL_FALSE
  Mat.unsafeWith viewMatrix $ glUniformMatrix4fv 1 1 GL_FALSE
  Mat.unsafeWith modelMatrix $ glUniformMatrix4fv 2 1 GL_FALSE

  glClearColor 0.1 0.1 0.2 1

  draw win chunk =<< getPOSIXTime
  GLFW.terminate

projection :: Int -> Int -> Mat.Matrix
projection w' h' = Mat.perspective (pi / 2) (w / h) 0.01 1000
  where
    w = fromIntegral w'
    h = fromIntegral h'

viewMatrix :: Mat.Matrix
viewMatrix = Mat.translate 0 (-9) (-ts - 10) Mat.identity
  where
    ts = Chunk.chunkSize

modelMatrix :: Mat.Matrix
modelMatrix = Mat.translate 0 0 0 Mat.identity

isKeyDown :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyDown win key = (==GLFW.KeyState'Pressed) <$> GLFW.getKey win key

draw :: GLFW.Window -> Chunk.Chunk -> NominalDiffTime -> IO ()
draw win chunk start = do
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  wireframe <- isKeyDown win GLFW.Key'F
  doTranslate <- isKeyDown win GLFW.Key'Space

  when doTranslate $ do
    time <- realToFrac . (flip (-) start) <$> getPOSIXTime

    let translateX = (sin time / 2 + 0.5) * (-Chunk.chunkSize)
    let translateY = cos time * 5
    let viewMatrix' = Mat.translate translateX translateY 0 viewMatrix

    Mat.unsafeWith viewMatrix' $ glUniformMatrix4fv 1 1 GL_FALSE

  if wireframe
    then glPolygonMode GL_FRONT_AND_BACK GL_LINE
    else glPolygonMode GL_FRONT_AND_BACK GL_FILL

  Chunk.draw chunk

  GLFW.swapBuffers win
  GLFW.pollEvents

  q <- GLFW.windowShouldClose win
  userQ <- isKeyDown win GLFW.Key'Q
  unless (q || userQ) $ draw win chunk start
