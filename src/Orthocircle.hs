module Orthocircle
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Utils.OpenGL

whitesmoke :: Color4 GLfloat
whitesmoke = Color4 0.96 0.96 0.96 1

fOrthocircle :: Double -> Double -> XYZ -> Double
fOrthocircle a b (x,y,z) =
  ((x2 + y2 - 1)**2 + z2) * ((y2 + z2 - 1)**2 + x2) *
  ((z2 + x2 - 1)**2 + y2) - a**2*(1 + b*(x2 + y2 + z2))
  where
    x2 = x*x
    y2 = y*y
    z2 = z*z

trianglesOrthocircle :: Double -> Double -> Double -> [NTriangle]
trianglesOrthocircle a b l =
  let triangles = marchingCubes (fOrthocircle a b) l ((-1.6,1.6),(-1.6,1.6),(-1.6,1.6)) 100
  in map fromTriangle triangles

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef Double -> IORef Double -- parameters a and b
        -> IORef Double  -- isolevel
        -> IORef Double  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 a b l zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  a' <- get a
  b' <- get b
  l' <- get l
  let triangles = trianglesOrthocircle a' b' l'
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ do
    materialDiffuse Front $= whitesmoke
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), norm) = do
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
  lookAt (Vertex3 0 0 (-4+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -> IORef Double -- parameters a and b
         -> IORef Double -- isolevel
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b l zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'f' -> a $~! (+ 0.025)
    'v' -> a $~! subtract 0.025
    'g' -> b $~! (+ 0.1)
    'b' -> b $~! subtract 0.1
    'h' -> l $~! (+ 0.02)
    'n' -> l $~! subtract 0.02
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Orthocircle"
  windowSize $= Size 400 400
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  cullFace $= Just Back
  clearColor $= black
  materialAmbient Front $= black
  lighting $= Enabled
  -- lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  a <- newIORef 0.075
  b <- newIORef 3.0
  l <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 a b l zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b l zoom)
  idleCallback $= Nothing -- Just idle
  putStrLn "*** Orthocircle ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b, h, n\n\
        \"
  mainLoop
