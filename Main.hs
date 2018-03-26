module Main where

import Control.Monad (unless, guard)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

import qualified Shader

main :: IO ()
main = do
  guard =<< GLFW.init

  let (w, h) = (640, 480)

  Just win <- GLFW.createWindow w h "" Nothing Nothing

  GLFW.makeContextCurrent $ Just win
  GLFW.setWindowSize win w h

  Shader.withProgram glUseProgram =<< Shader.loadProgram "Basic"

  glClearColor 0.2 0.2 0.5 1.0

  draw win
  GLFW.terminate

draw :: GLFW.Window -> IO ()
draw win = do
  glClear GL_COLOR_BUFFER_BIT

  GLFW.swapBuffers win
  GLFW.pollEvents

  q <- GLFW.windowShouldClose win
  unless q $ draw win
