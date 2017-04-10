module Haskgine2d.World.Type where

import Haskgine2d.World.Object as World
import Haskgine2d.Render.Camera

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map 

-- | Epoch time in miliseconds
type Time = Word64

{-| 
The world record should contain an data necessary to render
and manipulate one specific world.

A game could consist of multiple worlds.

This is an algebraic type as each implementation probably
want to define specific data, such ad defining a player character.
-}
data World a = World {
  innnerData :: a,
  objects  :: Map ObjectID World.Object,
  camera   :: Camera,

  objectIDSource :: ObjectID,

  time           :: Time,
  deltaTime      :: Time,
  timeSinceStart :: Time,
  timeStart      :: Time                    
  }

-- | For the simple case where nothing but basic data need to be saved
type SimpleWorld = World ()


-- | Generates a new object id from the world.
genObjectID :: World a -> (ObjectID, World a)
genObjectID world = (newID,  world{ objectIDSource = newID + 1})
   where
     newID = objectIDSource world

{- |
Updates the current time to a new time.
This updates all time data.

First argument 
-}
updateTime :: Time -> World a -> World a
updateTime newTime world = world{
  time = newTime,
  deltaTime = newTime - time world,
  timeSinceStart = newTime - timeStart world
  }

addObject :: (ObjectID -> Object) -> World a -> World a
addObject objBuilder world = world2{ objects = Map.insert objID (objBuilder objID) (objects world)}
  where
    (objID, world2) = genObjectID world
