module Haskgine2d.Render.Shapes where

import qualified Graphics.Rendering.OpenGL as GL

data Shape = Shape {
  vertices :: [Float],
  indices  :: [GL.GLushort]
  } deriving (Show, Eq, Read)

equilategralPolygon :: GL.GLushort -> Shape
equilategralPolygon edges = Shape{
  vertices =  [0,0,0] ++ (angles >>= \angle -> [cos angle, sin angle, 0]),
  indices =  [1..numEdges] >>= \ n -> [0, n + 1 `mod` numEdges, n]
  }

  where
    numEdges = if edges > 3 then edges else 3
    baseAngle = 2 * pi / (fromIntegral numEdges) 
    angles = map ((*baseAngle) . fromIntegral) [0 .. numEdges] 
    
equilategralTriangle :: Shape
equilategralTriangle = equilategralPolygon 3

square :: Shape
square = Shape{
  vertices = [-1, 1, 0,
              1, 1, 0,
              1, -1, 0,
              -1, -1, 0],
  indices = [0,1,2,3]  
}



