{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Graphics.SpriteKit

import Actions
import Constants
import Convenience
import GameState
import Pipes
import Random
import Scenery


lazyLambda :: LambdaScene
lazyLambda
  = (sceneWithSize (Size width height))
    { sceneBackgroundColor  = skyColour
    , sceneChildren         = [bird, movingNodes, groundPhysics, score]
    , sceneData             = initialSceneState
    , sceneUpdate           = Just update
    , scenePhysicsWorld     = physicsWorld
                              { worldGravity         = Vector 0 (-5)
                              , worldContactDidBegin = Just contact
                              }
    , sceneHandleEvent      = Just handleEvent
    }

(bird1Texture, birdWidth, birdHeight) = defineTexture "Bird-01.png"
(bird2Texture, _, _)                  = defineTexture "Bird-02.png"
(bird3Texture, _, _)                  = defineTexture "Bird-03.png"

bird :: LambdaNode
bird = (spriteWithTexture bird1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (width * 0.35) (height * 0.6)
       , nodeActionDirectives = [runActionForever flap]
       , nodePhysicsBody      
           = Just $
               (bodyWithCircleOfRadius (birdHeight / 2) Nothing)
               { bodyCategoryBitMask    = categoryBitMask [Bird]
               , bodyCollisionBitMask   = categoryBitMask [World]
               , bodyContactTestBitMask = categoryBitMask [World, Score]
               }
       }
  where
    flap = animateWithTextures 
             [bird1Texture, bird2Texture, bird3Texture, bird2Texture] 0.1

movingNodes :: LambdaNode
movingNodes = (node $ pipes : groundSprites ++ skySprites)
              { nodeName = Just "Moving" }

groundPhysics :: LambdaNode
groundPhysics = (node [])
                { nodePosition    = Point 0 (groundTileHeight / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (groundTileHeight / 2))
                                                  (Point width (groundTileHeight / 2)))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }

pipes :: LambdaNode
pipes = (node [])
        { nodeActionDirectives = [runActionSequenceForever 
                                  [ customAction (spawnPipePair birdWidth)
                                  , waitForDuration{ actionDuration = 1.5 }
                                  ] ]
        , nodeUserData         = PipesState randomNums
        }

score :: LambdaNode
score = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "Score"
        , nodePosition  = Point (width / 2) (3 * height / 4)
        , nodeZPosition = 100
        , labelText     = "0"
        }

update :: LambdaScene -> TimeInterval -> LambdaScene
update scene@Scene{ sceneData = sceneState@SceneState{..} } _dt 
  = case gameState of
      Running 
        | keyPressed -> bumpLambda scene{ sceneData = sceneState{ keyPressed = False } }
        | bumpScore  -> incScore scene{ sceneData = sceneState{ bumpScore = False } }
        | otherwise  -> tiltLambda scene
      Crash          -> crash scene{ sceneData = sceneState{ gameState = Over } }
      Over           -> scene
  
bumpLambda :: LambdaScene -> LambdaScene
bumpLambda scene
  = scene { sceneActionDirectives = [runCustomActionOn "Lambda" bumpAction] }

tiltLambda :: LambdaScene -> LambdaScene
tiltLambda scene 
  = scene{ sceneActionDirectives = [runCustomActionOn "Lambda" tiltAction] }

crash :: LambdaScene -> LambdaScene
crash scene
  = scene { sceneActionDirectives = [ runActionOn "Lambda" crashAction
                                    , runActionOn "Moving" stopMoving
                                    ] }
  where
    crashAction = sequenceActions
                  [ (rotateByAngle (-pi * 4)){ actionDuration = 1 }
                  , fadeOut{ actionDuration = 0.2 }
                  ]
    stopMoving  = sequenceActions
                  [ waitForDuration{ actionDuration = 1 }
                  , customAction $ \node _ -> node{ nodeSpeed = 0 }
                  ] 
    
incScore :: LambdaScene -> LambdaScene 
incScore scene@Scene{ sceneData = sceneState }
  = scene
    { sceneActionDirectives = [runCustomActionOn "Score" setScore]
    , sceneData             = sceneState{ sceneScore = newScore }
    }
  where
    newScore = sceneScore sceneState + 1

    setScore label@Label{} _dt = label{ labelText = show newScore }
    setScore node          _   = node

contact :: SceneState 
        -> PhysicsContact u
        -> (Maybe SceneState, Maybe (Node u), Maybe (Node u))
contact state@SceneState{..} PhysicsContact{..}
  | (isWorld contactBodyA || isWorld contactBodyB) && gameState == Running
  = (Just state{ gameState = Crash }, Nothing, Nothing)
  | isScore contactBodyA || isScore contactBodyB
  = (Just state{ bumpScore = True }, Nothing, Nothing)
  | otherwise
  = (Nothing, Nothing, Nothing)  

handleEvent :: Event -> SceneState -> Maybe SceneState
handleEvent KeyEvent{ keyEventType = KeyDown } state = Just state{ keyPressed = True }
handleEvent _                                  _     = Nothing
