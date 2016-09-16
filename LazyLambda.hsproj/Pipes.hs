module Pipes where
  
import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState


(pipeUpTexture,   pipeUpWidth,   pipeUpHeight)   = defineTexture "PipeUp.png"
(pipeDownTexture, pipeDownWidth, pipeDownHeight) = defineTexture "PipeDown.png"

spawnPipePair :: GFloat -> LambdaNode -> TimeInterval -> LambdaNode
spawnPipePair birdWidth pipes _dt
  = pipes
    { nodeChildren = pipePair birdWidth pipeUpY : nodeChildren pipes 
    , nodeUserData = pipeState'
    }
  where
    heightVariation = round (height / 4)
    (pipeState', i) = randomInt (nodeUserData pipes)
    pipeUpY         = fromIntegral $ heightVariation + (i `mod` heightVariation) `div` 2

pipePair :: GFloat -> GFloat -> LambdaNode
pipePair birdWidth pipeUpY
  = (node [pipeDown pipeUpY, scoreContact birdWidth, pipeUp pipeUpY])
    { nodePosition         = Point (width + pipeUpWidth) 0
    , nodeZPosition        = -10
    , nodeActionDirectives = [movePipesAndRemove]
    }
  where
    movePipesAndRemove = runAction $ 
                           sequenceActions [ (moveBy $ Vector (-distanceToMove) 0)
                                             { actionDuration = 0.005 * distanceToMove }
                                           , removeFromParent
                                           ]
    distanceToMove     = width + pipeUpWidth
    pipeDown pipeUpY   = pipe pipeDownTexture 
                              (pipeUpY + pipeDownHeight + verticalPipeGap)
                              pipeDownWidth
                              pipeDownHeight
    pipeUp pipeUpY     = pipe pipeUpTexture 
                              pipeUpY
                              pipeUpWidth
                              pipeUpHeight

pipe :: Texture -> GFloat -> GFloat -> GFloat -> LambdaNode
pipe texture y width height
  = (spriteWithTexture texture)
    { nodePosition    = Point 0 y 
    , nodePhysicsBody = Just $ (bodyWithRectangleOfSize (Size width height) Nothing)
                               { bodyIsDynamic          = False
                               , bodyCategoryBitMask    = categoryBitMask [World]
                               , bodyContactTestBitMask = categoryBitMask [Bird]
                               }
    }

scoreContact :: GFloat -> LambdaNode
scoreContact birdWidth
  = (node [])
    { nodePosition    = Point (pipeDownWidth / 2 + birdWidth / 2) (height / 2)
    , nodePhysicsBody = Just $ (bodyWithEdgeFromPointToPoint (Point 0 0) (Point 0 height))
                               { bodyCategoryBitMask    = categoryBitMask [Score]
                               , bodyContactTestBitMask = categoryBitMask [Bird]
                               }
    }
