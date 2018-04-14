module Balls
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Utils.OpenGL

red :: Color4 GLfloat
red = Color4 1 0 0 1

fballs :: XYZ -> Double
fballs (x,y,z) =
  1/(x2+y2+z2) + 1/(x2+y2'+z2) + 1/(x2'+y2+z2)
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z
  x2' = (x-1)*(x-1)
  y2' = (y-1)*(y-1)

trianglesBalls :: Double -> [NTriangle]
trianglesBalls l =
  let triangles = marchingCubes fballs l ((-1,2),(-1,2),(-1,1)) 100
  in map fromTriangle triangles

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef Double  -- isolevel
        -> IORef Double  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 l zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  l' <- get l
  let triangles = trianglesBalls l'
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), norm) = do
      materialDiffuse FrontAndBack $= red
      normal norm
      vertex v1
      vertex v2
      vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-5+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- isolevel
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 l zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'h' -> l $~! (+ 0.1)
    'n' -> l $~! subtract 0.1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Balls"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  l <- newIORef 11.0
  displayCallback $= display rot1 rot2 rot3 l zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 l zoom)
  idleCallback $= Nothing
  putStrLn "*** Balls ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \"
  mainLoop
