{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           ClassyPrelude                  hiding (union)
import           Control.Lens                   (has, makeLenses, mapped, only,
                                                 (%~), (&), (.~), (^.), (^?),
                                                 _head)
import           Control.Monad.Loops            (whileM_)
import           Data.Foldable                  (asum)
import           Frp.Ord
import           Linear.V2
import           Linear.Vector                  ((*^), (^*))
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.BitmapFont.Render
import           Wrench.BitmapFont.RenderResult
import           Wrench.Color
import           Wrench.CommonGeometry
import           Wrench.Engine
import qualified Wrench.Event                   as WE
import           Wrench.ImageData
import qualified Wrench.Keysym                  as KS
import           Wrench.MediaData
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Rectangle
import           Wrench.Time
import           Wrench.WindowSize

data TickData = TickData { _currentTicks :: TimeTicks, _currentDelta :: TimeDelta }

$(makeLenses ''TickData)

blockSize = V2 40 20
initialBallVelocity = V2 100 (-100)
initialBallPosition = V2 320 240
paddleSize = V2 62 18
ballSize = 14
initialPaddlePosition :: Point
initialPaddlePosition = V2 320 (480-14-(paddleSize ^. _y))
leftBorder = 20
rightBorder = 640-20
topBorder = 32
topBlockBorder=92
bottomBorder = 480

initialBlocks :: [Point]
initialBlocks = [V2 (leftBorder + x * blockSize ^. _x) (topBorder + y * blockSize ^. _y) | x <- [0..14], y <- [0..5]]

data CollisionDirection = CollisionOnLeft
                        | CollisionOnRight
                        | CollisionOnRoof
                        | CollisionOnFloor
                          deriving(Show)

ballRect :: Point -> Rectangle
ballRect p = rectFromOriginAndDim p (V2 ballSize ballSize)

paddleRect :: Point -> Rectangle
paddleRect p = rectFromOriginAndDim p paddleSize

ballBlocksCollision :: [Point] -> Point -> Maybe (Int,CollisionDirection)
ballBlocksCollision blocks ballPos =
  let blockRects :: [Rectangle]
      blockRects = (`rectFromOriginAndDim` blockSize) <$> blocks
      collisions :: [Maybe CollisionDirection]
      collisions = (`detectCollisionRect` ballRect ballPos) <$> blockRects
      collisionWithIndex :: [Maybe (Int,CollisionDirection)]
      collisionWithIndex = sequence <$> (zip [0..] collisions)
      firstCollision = asum collisionWithIndex
  in firstCollision

detectCollisionRect :: Rectangle -> Rectangle -> Maybe CollisionDirection
detectCollisionRect rect ball
  | paddleInRange && (ballVMiddle < rect ^. rectTop) = Just CollisionOnFloor
  | paddleInRange && (ballVMiddle > rect ^. rectBottom) = Just CollisionOnRoof
  | paddleInRange && (ballHMiddle < paddleHMiddle) = Just CollisionOnLeft
  | paddleInRange && (ballHMiddle > paddleHMiddle) = Just CollisionOnRight
  | otherwise = Nothing
  where paddleInRange = ((ball ^. rectRightBottom) `pointG` (rect ^. rectLeftTop)) && ((ball ^. rectLeftTop) `pointL` (rect ^. rectRightBottom))
        ballVMiddle = ball ^. rectTop + ball ^. rectHeight / 2
        ballHMiddle = ball ^. rectLeft + ball ^. rectWidth / 2
        paddleHMiddle = rect ^. rectLeft + rect ^. rectWidth / 2

detectCollisionBorder :: Rectangle -> Maybe CollisionDirection
detectCollisionBorder ball
  | ball ^. rectLeft < leftBorder = Just CollisionOnLeft
  | ball ^. rectRight > rightBorder = Just CollisionOnRight
  | ball ^. rectTop < topBorder = Just CollisionOnRoof
  | ball ^. rectBottom > bottomBorder = Just CollisionOnFloor
  | otherwise = Nothing

detectCollision :: Point -> Point -> Maybe CollisionDirection
detectCollision paddlePos ballPos =
  detectCollisionBorder br <|>
  detectCollisionRect (rectFromOriginAndDim paddlePos paddleSize) br
    where br = ballRect ballPos

createPicture :: SurfaceMap a -> Point -> Point -> [Point] -> Int -> Picture
createPicture images paddle ball blocks score = pictures $ [paddle `pictureTranslated` pictureSpriteTopLeft "paddle",ball `pictureTranslated` pictureSpriteTopLeft "ball"] <> ((`pictureTranslated` pictureSpriteTopLeft "block") <$> blocks) <> [renderScore images score]

transformVelocity :: CollisionDirection -> Point -> Point
transformVelocity CollisionOnLeft v | v ^. _x < 0 = v & _x %~ negate
transformVelocity CollisionOnRight v | v ^. _x > 0 = v & _x %~ negate
transformVelocity CollisionOnRoof v | v ^. _y < 0 = v & _y %~ negate
transformVelocity CollisionOnFloor v | v ^. _y > 0 = v & _y %~ negate
transformVelocity _ v = v

deleteNth :: Int -> [a] -> [a]
deleteNth n = uncurry (++) . second unsafeTail . splitAt n

renderScore :: SurfaceMap a -> Int -> Picture
renderScore images score = (textToPicture images "djvu" 0 (pack (show score))) ^. bfrrPicture

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TickData -> AddHandler WE.Event -> Handler () -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler quitFire = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    mouseXMovement :: Event t Point
    mouseXMovement = filterJust ((mapped . _y .~ 0) . (^? WE._MouseAxis . WE.mouseAxisDelta) <$> eevent)
    ballPaddleCollision :: Event t CollisionDirection
    ballPaddleCollision = filterJust ((detectCollision <$> paddlePosition <*> ballPosition) <@ etick)
    blocks :: Behavior t [Point]
    blocks = accumB initialBlocks ((\(collisionIdx,_) -> deleteNth collisionIdx) <$> ballBlockCollision)
    ballBlockCollision :: Event t (Int,CollisionDirection)
    ballBlockCollision = filterJust ((ballBlocksCollision <$> blocks <*> ballPosition) <@ etick)
    deltaVel :: Point -> TickData -> Point
    deltaVel v td = (realToFrac (toSeconds (td ^. currentDelta))) *^ v
    ballPosition :: Behavior t Point
    ballPosition = accumB initialBallPosition ((+) <$> (deltaVel <$> ballVelocity <@> etick))
    ballVelocity :: Behavior t Point
    ballVelocity = accumB initialBallVelocity (transformVelocity <$> (ballPaddleCollision `union` (snd <$> ballBlockCollision)))
    paddlePosition :: Behavior t Point
    paddlePosition = accumB initialPaddlePosition ((\(V2 x1 y1) (V2 x2 y2) -> V2 (clamp leftBorder (rightBorder - paddleSize ^. _x) (x1+x2)) (y1+y2)) <$> mouseXMovement)
    score :: Behavior t Int
    score = accumB 0 ((+) <$> (1 <$ ballBlockCollision))
    currentPictureEvent = (createPicture <$> pure surfaces <*> paddlePosition <*> ballPosition <*> blocks <*> score) <@ etick
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> currentPictureEvent
  let quitEvent = filterE (has (WE._Keyboard . WE.keySym . only KS.Escape)) eevent
  reactimate $ (\_ -> quitFire ()) <$> quitEvent

main :: IO ()
main = do
  withPlatform (WindowTitle "arkanoid, frp style") (ConstantWindowSize 640 480) MouseGrabYes $ \platform -> do
    (tickAddHandler,tickFire) <- newAddHandler
    (eventAddHandler,eventFire) <- newAddHandler
    (quitAddHandler,quitFire) <- newAddHandler
    md <- liftIO (readMediaFiles (loadImage platform) "media/images")
    quitRef <- newIORef True
    unregisterQuit <- register quitAddHandler (\_ -> writeIORef quitRef False)
    compiledNetwork <- compile (setupNetwork platform (md ^. mdSurfaces) tickAddHandler eventAddHandler quitFire)
    actuate compiledNetwork
    initialTicks <- getTicks
    oldTickRef <- newIORef initialTicks
    whileM_ (readIORef quitRef) $ do
      ticks <- getTicks
      oldTicks <- readIORef oldTickRef
      writeIORef oldTickRef ticks
      events <- pollEvents platform
      mapM_ eventFire events
      tickFire (TickData{_currentTicks=ticks,_currentDelta=(ticks `tickDelta` oldTicks)})
      threadDelay (fromSeconds (1/60))
