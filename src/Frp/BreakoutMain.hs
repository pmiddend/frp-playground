{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           ClassyPrelude                  hiding (union)
import           Control.Lens                   (has, makeLenses, mapped, only,
                                                 view, (%~), (&), (.~), (^.),
                                                 (^?), _2)
import           Control.Monad.Loops            (whileM_)
import           Data.Foldable                  (asum)
import           Frp.Banana
import           Frp.List
import           Frp.Ord
import           Linear.Metric                  (normalize)
import           Linear.V2
import           Linear.Vector                  ((*^))
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.Angular
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
import           Wrench.Rectangle
import           Wrench.Time
import           Wrench.WindowSize

data TickData = TickData { _currentTicks :: TimeTicks, _currentDelta :: TimeDelta }

$(makeLenses ''TickData)

type UnitType = Float
type Point = V2 UnitType

blockSize :: Point
blockSize = V2 40 20
initialBallVelocityNorm :: UnitType
initialBallVelocityNorm = 300
initialBallVelocity :: Point
initialBallVelocity = initialBallVelocityNorm *^ (normalize (V2 1 1))
initialBallPosition :: Point
initialBallPosition = V2 320 240
paddleSize :: Point
paddleSize = V2 62 18
ballSize :: UnitType
ballSize = 14
initialPaddlePosition :: Point
initialPaddlePosition = V2 320 (480-14-(paddleSize ^. _y))
leftBorder :: UnitType
leftBorder = 20
rightBorder :: UnitType
rightBorder = 640-20
topBorder :: UnitType
topBorder = 32
topBlockBorder :: Integer
topBlockBorder=92
bottomBorder :: UnitType
bottomBorder = 480

initialBlocks :: [Point]
initialBlocks = [V2 (leftBorder + x * blockSize ^. _x) (topBorder + y * blockSize ^. _y) | x <- [0..14], y <- [0..5]]

data CollisionDirection = CollisionOnLeft
                        | CollisionOnRight
                        | CollisionOnRoof
                        | CollisionOnFloor
                          deriving(Show,Eq,Bounded,Read,Enum)

data CollisionData = CollisionData {
    _cdDirection    :: CollisionDirection
  , _cdPointOnBall  :: Point
  , _cdPointOnOther :: Point
  }

$(makeLenses ''CollisionData)

ballRect :: Point -> Rectangle UnitType
ballRect p = rectFromOriginAndDim p (V2 ballSize ballSize)

paddleRect :: Point -> Rectangle UnitType
paddleRect p = rectFromOriginAndDim p paddleSize

ballBlocksCollision :: [Point] -> Point -> Maybe (Int,CollisionData)
ballBlocksCollision blocks ballPos =
  let blockRects :: [Rectangle UnitType]
      blockRects = (`rectFromOriginAndDim` blockSize) <$> blocks
      collisions :: [Maybe CollisionData]
      collisions = (`detectCollisionRect` ballRect ballPos) <$> blockRects
      collisionWithIndex :: [Maybe (Int,CollisionData)]
      collisionWithIndex = sequence <$> (zip [0..] collisions)
      firstCollision = asum collisionWithIndex
  in firstCollision

detectCollisionRect :: Rectangle UnitType -> Rectangle UnitType -> Maybe CollisionData
detectCollisionRect rect ball
  | paddleInRange && (ballVMiddle < rect ^. rectTop) = Just (makeCollision CollisionOnFloor (V2 ballHMiddle (rect ^. rectTop)))
  | paddleInRange && (ballVMiddle > rect ^. rectBottom) = Just (makeCollision CollisionOnRoof (V2 ballHMiddle (rect ^. rectBottom)))
  | paddleInRange && (ballHMiddle < paddleHMiddle) = Just (makeCollision CollisionOnLeft (V2 (rect ^. rectLeft) ballVMiddle))
  | paddleInRange && (ballHMiddle > paddleHMiddle) = Just (makeCollision CollisionOnRight (V2 (rect ^. rectRight) ballVMiddle))
  | otherwise = Nothing
  where paddleInRange = ((ball ^. rectRightBottom) `pointG` (rect ^. rectLeftTop)) && ((ball ^. rectLeftTop) `pointL` (rect ^. rectRightBottom))
        ballVMiddle = ball ^. rectTop + ball ^. rectHeight / 2
        ballHMiddle = ball ^. rectLeft + ball ^. rectWidth / 2
        paddleHMiddle = rect ^. rectLeft + rect ^. rectWidth / 2
        makeCollision dir p = CollisionData{_cdDirection=dir,_cdPointOnBall=p - ball ^. rectLeftTop,_cdPointOnOther=p - rect ^. rectLeftTop}

detectCollisionBorder :: Rectangle UnitType -> Maybe CollisionDirection
detectCollisionBorder ball
  | ball ^. rectLeft < leftBorder = Just CollisionOnLeft
  | ball ^. rectRight > rightBorder = Just CollisionOnRight
  | ball ^. rectTop < topBorder = Just CollisionOnRoof
  | ball ^. rectBottom > bottomBorder = Just CollisionOnFloor
  | otherwise = Nothing

createPicture :: SurfaceMap a -> Point -> Point -> [Point] -> Int -> Picture UnitType UnitType
createPicture images paddle ball blocks score = pictures $ [pictureSpriteTopLeft "background",paddle `pictureTranslated` pictureSpriteTopLeft "paddle",ball `pictureTranslated` pictureSpriteTopLeft "ball"] <> ((`pictureTranslated` pictureSpriteTopLeft "block") <$> blocks) <> [renderScore images score]

transformVelocity :: (Maybe CollisionDirection,Maybe CollisionData) -> Point -> Point
transformVelocity (Just dir,_) v = transformVelocityBorder dir v
transformVelocity (_,Just d) v = transformVelocityPaddle d v
transformVelocity _ v = v

transformVelocityPaddle :: CollisionData -> Point -> Point
transformVelocityPaddle cd v =
  let theta = Degrees $ (-70) * (1-alpha) + 70 * alpha
      alpha = traceShowId ((cd ^. cdPointOnOther . _x) / (paddleSize ^. _x))
      rotatedNormal = V2 (sinD theta) (-(cosD theta))
  in
    if v ^. _y > 0 && (cd ^. cdDirection) == CollisionOnFloor
    then initialBallVelocityNorm *^ rotatedNormal
    else
      if v ^. _y > 0
      then transformVelocityBorder (cd ^. cdDirection) v
      else v

transformVelocityBorder :: CollisionDirection -> Point -> Point
transformVelocityBorder CollisionOnLeft v | v ^. _x < 0 = v & _x %~ negate
transformVelocityBorder CollisionOnRight v | v ^. _x > 0 = v & _x %~ negate
transformVelocityBorder CollisionOnRoof v | v ^. _y < 0 = v & _y %~ negate
transformVelocityBorder CollisionOnFloor v | v ^. _y > 0 = v & _y %~ negate
transformVelocityBorder _ v = v

renderScore :: SurfaceMap a -> Int -> Picture UnitType UnitType
renderScore images score = (textToPicture images "djvu" 0 (pack (show score))) ^. bfrrPicture

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TickData -> AddHandler WE.Event -> Handler () -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler quitFire = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    mouseXMovement :: Event t Point
    mouseXMovement = fmap (fmap fromIntegral) (filterJust ((mapped . _y .~ 0) . (^? WE._MouseAxis . WE.mouseAxisDelta) <$> eevent))
    ballBorderCollision :: Event t CollisionDirection
    ballBorderCollision = filterJust ((detectCollisionBorder <$> (ballRect <$> ballPosition)) <@ etick)
    ballPaddleCollision :: Event t CollisionData
    ballPaddleCollision = filterJust ((detectCollisionRect <$> (paddleRect <$> paddlePosition) <*> (ballRect <$> ballPosition)) <@ etick)
    blocks :: Behavior t [Point]
    blocks = accumB initialBlocks ((\(collisionIdx,_) -> deleteNth collisionIdx) <$> ballBlockCollision)
    ballBlockCollision :: Event t (Int,CollisionData)
    ballBlockCollision = filterJust ((ballBlocksCollision <$> blocks <*> ballPosition) <@ etick)
    deltaVel :: Point -> TickData -> Point
    deltaVel v td = (realToFrac (toSeconds (td ^. currentDelta))) *^ v
    ballPosition :: Behavior t Point
    ballPosition = accumB initialBallPosition ((+) <$> (deltaVel <$> ballVelocity <@> etick))
    ballBorderAndBlockCollision :: Event t CollisionDirection
    ballBorderAndBlockCollision = ballBorderCollision `union` (view (_2 . cdDirection) <$> ballBlockCollision)
    ballVelocity :: Behavior t Point
    ballVelocity = accumB initialBallVelocity (merge ballBorderAndBlockCollision ballPaddleCollision transformVelocityBorder transformVelocityPaddle)
    paddlePosition :: Behavior t Point
    paddlePosition = accumB initialPaddlePosition ((\(V2 x1 y1) (V2 x2 y2) -> V2 (clamp leftBorder (rightBorder - paddleSize ^. _x) (x1+x2)) (y1+y2)) <$> mouseXMovement)
    score :: Behavior t Int
    score = accumB 0 ((+) <$> (1 <$ ballBlockCollision))
    currentPictureEvent = (createPicture <$> pure surfaces <*> paddlePosition <*> ballPosition <*> blocks <*> score) <@ etick
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> (first (floor :: UnitType -> Int) <$> currentPictureEvent)
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
    _ <- register quitAddHandler (\_ -> writeIORef quitRef False)
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
