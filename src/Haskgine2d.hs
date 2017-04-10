module Haskgine2d where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Haskgine2d.World.Object as World
import qualified Haskgine2d.Render.Object as Render

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Graphics.Rendering.OpenGL as GL



data ObjectTemplate = ObjectTemplate {
  name           :: String,
  fragmentShader :: FilePath,
  vertexShader   :: FilePath,
  image          :: FilePath                    
  }
                      


{- |

prepareObjects :: [ObjectTemplate] -> IO (Map String Render.Object)
prepareObjects templates = map shaderSet (
  where
    shaderSet = Set.fromList $ map (\ e -> (fragmentShader e, vertexShader e))
-}
