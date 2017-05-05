module Haskgine2d.World.Object where

import qualified Haskgine2d.Render.Object as Render
import Data.Word


type ObjectID = Word64

data Object = Object {
  objectID     :: ObjectID,  
  renderObjectID :: Render.ObjectID                
  }


              
 
