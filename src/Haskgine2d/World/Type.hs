module Haskgine2d.World.Type where

import Control.Monad.Trans.State (StateT(..))
import Haskgine2d.World.Object as World
import Haskgine2d.Render.Camera

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

type Time = Word64

data WorldData a = WorldData {
  innnerData :: a,
  objects  :: Map ObjectID World.Object,
  camera   :: Camera,

  objectIDSource :: ObjectID,

  time          :: Time,
  deltaTime     :: Time
                    
  }




genObjectID :: World -> (ObjectID, World)
genObjectID world = (newID,  world{ objectIDSource = newID + 1})
   where
     newID = objectIDSource world

updateTime :: Time -> World -> World
updateTime newTime world = world{ time = newTime, deltaTime = newTime -  time world}

addObject :: (ObjectID -> Object) -> World -> World 
addObject objBuilder world = world2{ objects = objBuilder world : objects world}
  where
    (objID, world2) = genObjectID world 
