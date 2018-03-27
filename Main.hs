module Main where

import System.Random (randomIO)
import Control.Monad (guard, unless)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, sizeOf)
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Shader
import qualified Matrix as Mat
import Numeric.Noise (Seed)
import qualified Numeric.Noise.Perlin as Noise
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Bits ((.|.))

makeBuffer :: IO GLuint
makeBuffer = do
  bufPtr <- malloc
  glGenBuffers 1 bufPtr
  peek bufPtr

onResize :: GLFW.Window -> Int -> Int -> IO ()
onResize _ w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)

  let aspect = fromIntegral w / fromIntegral h
  let newProjection = Mat.perspective (pi / 2) aspect 0.01 100

  Mat.unsafeWith newProjection $ glUniformMatrix4fv 0 1 GL_FALSE

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

  vertexBuffer <- makeBuffer

  glBindBuffer GL_ARRAY_BUFFER vertexBuffer

  program <- Shader.loadProgram "Basic"

  Shader.withProgram program $ \p -> do
    glUseProgram p

    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 3 GL_FLOAT 0 0 nullPtr

  Mat.unsafeWith projection $ glUniformMatrix4fv 0 1 GL_FALSE
  Mat.unsafeWith viewMatrix $ glUniformMatrix4fv 1 1 GL_FALSE
  Mat.unsafeWith modelMatrix $ glUniformMatrix4fv 2 1 GL_FALSE

  glClearColor 0 0 0 1

  verts <- generateVerts . (*10000) <$> randomIO

  V.unsafeWith verts $ \ptr ->
    let len = fromIntegral $ sizeOf (0 :: Float) * V.length verts
    in glBufferData GL_ARRAY_BUFFER len ptr GL_STATIC_DRAW

  draw win (V.length verts) =<< getPOSIXTime
  GLFW.terminate

generateVerts :: Seed -> Vector Float
generateVerts seed = V.fromList $ generatePlane 100 100 perlin
  where
    perlin = Noise.perlin seed 5 0.05 0

projection :: Mat.Matrix
projection = Mat.perspective (pi / 2) (640 / 480) 0.01 100

viewMatrix :: Mat.Matrix
viewMatrix = Mat.translate (-50) (-10) (-130) Mat.identity

modelMatrix :: Mat.Matrix
modelMatrix = Mat.translate 0 0 0 Mat.identity

generatePlane :: Int -> Int -> Noise.Perlin -> [Float]
generatePlane _ 0 _ = []
generatePlane width height perlin = generateRow width height perlin ++ generatePlane width (height - 1) perlin

generateRow :: Int -> Int -> Noise.Perlin -> [Float]
generateRow 0 _ _ = []
generateRow width y' perlin = [ x, height, y ] ++ generateRow (width - 1) y' perlin
  where
    x = fromIntegral $ width
    y = fromIntegral $ y'
    height = realToFrac $ Noise.noiseValue perlin (realToFrac x, realToFrac y, 0)

draw :: GLFW.Window -> Int ->NominalDiffTime -> IO ()
draw win vertLength start = do  
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  now <- getPOSIXTime

  let time = fromRational . toRational $ now - start
  let viewMat = Mat.translate (sin time * 50) (cos time * 5 + 2) (time * 3) viewMatrix

  Mat.unsafeWith viewMat $ glUniformMatrix4fv 1 1 GL_FALSE
  glDrawArrays GL_POINTS 0 $ fromIntegral vertLength

  GLFW.swapBuffers win
  GLFW.pollEvents

  q <- GLFW.windowShouldClose win
  unless q $ draw win vertLength start
