module Haskgine2d.World.Type where

import Control.Monad.Trans.State (StateT(..))
import Haskgine2d.World.Object as World
import Haskgine2d.Render.Camera

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

type Time = Word64

data World a = World {
  innnerData :: a,
  objects  :: Map ObjectID World.Object,
  camera   :: Camera,

  objectIDSource :: ObjectID,

  time          :: Time,
  deltaTime     :: Time
                    
  }

type SimpleWorld = World ()


genObjectID :: World a -> (ObjectID, World a)
genObjectID world = (newID,  world{ objectIDSource = newID + 1})
   where
     newID = objectIDSource world

updateTime :: Time -> World a -> World a
updateTime newTime world = world{ time = newTime, deltaTime = newTime -  time world}

addObject :: (ObjectID -> Object) -> World a -> World a
addObject objBuilder world = world2{ objects = Map.insert objID (objBuilder objID)  objects world}
  where
    (objID, world2) = genObjectID world
