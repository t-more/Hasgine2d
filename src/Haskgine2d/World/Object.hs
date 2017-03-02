module Haskgine2d.World.Object where

import qualified Haskgine2d.Render.Object as Render
import Data.Word


type ObjectID = Word64

data Coordinate = Word64

data Position = Position { positionX :: Coordinate, positionY :: Coordinate}

data Object = Object {
  objectID     :: ObjectID,
  positon      :: Position,
  width        :: Word64,
  heigth       :: Word64,
  color        :: (Float,Float,Float),
  renderObject :: Render.Object                 
  }
 
