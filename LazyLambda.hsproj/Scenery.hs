module Scenery where
  
import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState
  

(groundTexture, groundTileWidth, groundTileHeight) = defineTexture "Land.png"

groundSprites :: [LambdaNode]
groundSprites = [ (spriteWithTexture groundTexture)
                  { nodePosition         = Point x (groundTileHeight / 2)
                  , nodeActionDirectives = [groundMovement]
                  }
                | x <- [0, groundTileWidth..width + groundTileWidth]]
  where
    movementDuration = 0.005 * groundTileWidth
    groundMovement   = runActionSequenceForever
                       [ (moveBy $ Vector (-groundTileWidth) 0)  -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector groundTileWidth 0)     -- reset
                         { actionDuration = 0 }
                       ]

(skyTexture, skyTileWidth, skyTileHeight) = defineTexture "Sky.png"

skySprites :: [LambdaNode]
skySprites = [ (spriteWithTexture skyTexture)
               { nodePosition         = Point x skyHeight
               , nodeZPosition        = -20
               , nodeXScale           = 2
               , nodeYScale           = 2
               , nodeActionDirectives = [skyMovement]
               }
             | x <- [0, skyTileWidth..width + skyTileWidth]]
  where
    skyHeight        = (skyTileHeight / 2) + groundTileHeight
    movementDuration = 0.025 * skyTileWidth
    skyMovement      = runActionSequenceForever
                       [ (moveBy $ Vector (-skyTileWidth) 0)   -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector skyTileWidth 0)      -- reset
                         { actionDuration = 0 }
                       ]

