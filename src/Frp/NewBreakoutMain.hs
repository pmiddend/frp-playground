{-# LANGUAGE TemplateHaskell #-}
module Main where

import Wrench.Engine(withPlatform,wrenchRender)
import Wrench.Color(colorsBlack)
import Wrench.MediaData(readMediaFiles,mdSurfaces)
import Wrench.Platform(WindowTitle(..),pollEvents,Platform,loadImage)
import Wrench.WindowSize(WindowSize(..))
import Wrench.Rectangle(Rectangle(..),rectTop,rectBottom,rectRightBottom,rectLeftTop,rectHeight,rectWidth,rectLeft,rectRight,rectFromOriginAndDim,rectDimensions)
import Wrench.MouseGrabMode(MouseGrabMode(..))
import Wrench.Time(TimeTicks,getTicks,threadDelay,fromSeconds,tickDelta,TimeDelta,toSeconds)
import qualified Wrench.Event as WE
import Wrench.CommonGeometry(pointL,pointG)
import Wrench.ImageData(findSurfaceUnsafe)
import Wrench.Picture(Picture,pictureSprite,pictureTranslated,pictures)
import ClassyPrelude
import Frp.Ord(clamp)
import Control.FRPNow.Core(runNowMaster,Behavior,callback,Now,async,planNow,switch)
import Control.FRPNow.Lib(sample,plan,foldB,snapshot)
import qualified Control.FRPNow.Lib(when)
import Control.FRPNow.Time(delayTime)
import Control.FRPNow.EvStream(EvStream,callbackStream,catMaybesEs,foldEs,toChanges)
import Control.Lens(makeLenses,(^.),view,(&),(.~),mapped,(^?),_1)
import Linear.V2(V2(..),_x,_y)
import Linear.Vector((*^))

-- Engine definitions important for game code begin
data TickData = TickData { _currentTicks :: TimeTicks, _currentDelta :: TimeDelta }

$(makeLenses ''TickData)

type PictureType = Picture Float Double
-- Engine definitions important for game code end

-- Begin game code
type UnitType = Float
type Point = V2 UnitType

data PictureData = PictureData{
    _picturePaddle :: (PictureType,Point)
  , _pictureBall :: (PictureType,Point)
  , _pictureBlock :: (PictureType,Point)
  }

$(makeLenses ''PictureData)

data CollisionDirection = CollisionOnLeft
                        | CollisionOnRight
                        | CollisionOnRoof
                        | CollisionOnFloor
                          deriving(Show,Eq,Bounded,Read,Enum)

data CollisionData = CollisionData {
    _cdDirection    :: CollisionDirection
  , _cdPointOnBall  :: Point
  , _cdPointOnOther :: Point
  } deriving(Eq)

$(makeLenses ''CollisionData)

paddleSize :: Point
paddleSize = V2 62 18

bottomBorder :: UnitType
bottomBorder = 480

initialPaddlePosition :: Point
initialPaddlePosition = V2 320 (480-14-(paddleSize ^. _y))

blockSize :: Point
blockSize = V2 40 20

ballSize :: UnitType
ballSize = 14

topBorder :: UnitType
topBorder = 32

initialBlocks :: [Point]
initialBlocks = [V2 (leftBorder + x * blockSize ^. _x) (topBorder + y * blockSize ^. _y) | x <- [0..14], y <- [0..5]]

leftBorder :: UnitType
leftBorder = 20

rightBorder :: UnitType
rightBorder = 640-20

fromIntegralPoint :: V2 Int -> V2 UnitType
fromIntegralPoint = (fromIntegral <$>)

mouseXMovement :: EvStream WE.Event -> EvStream Point
mouseXMovement event = fromIntegralPoint <$> catMaybesEs ((mapped . _y .~ 0) . (^? WE._MouseAxis . WE.mouseAxisDelta) <$> event)

initialBallPosition :: Point
initialBallPosition = V2 320 240

calcPaddlePosition :: EvStream Point -> Behavior (Behavior Point)
calcPaddlePosition xMovement = foldEs ((\(V2 x1 y1) (V2 x2 y2) -> V2 (clamp leftBorder (rightBorder - paddleSize ^. _x) (x1+x2)) (y1+y2))) initialPaddlePosition xMovement

deltaVel :: Point -> TickData -> Point
deltaVel v td = (realToFrac (toSeconds (td ^. currentDelta))) *^ v

integrate :: (Eq (f a), Fractional a, Functor f, Num (f a)) => Behavior TimeTicks -> Behavior (f a) -> Behavior (Behavior (f a))
integrate time v = do
    t <- time
    vp <- delayTime time (t,0) ((,) <$> time <*> v)
    foldB add 0 $ (,) <$> vp <*> time
  where
    add total ((t1,v),t2) = total + (realToFrac (toSeconds (t2 `tickDelta` t1)) *^ v)

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

ballRect :: Point -> Rectangle UnitType
ballRect p = rectFromOriginAndDim p (V2 ballSize ballSize)

paddleRect :: Point -> Rectangle UnitType
paddleRect p = rectFromOriginAndDim p paddleSize

game :: TimeTicks -> EvStream TickData -> EvStream WE.Event -> PictureData -> Now (Behavior PictureType)
game initialTicks time event pictureData = do
  --let changeEvents = V2 1 1 <$ time :: EvStream (V2 Int)
  --s <- sample $ foldEs (+) (V2 0 0) changeEvents
  currentTime <- sample $ foldEs (\_ current -> current) initialTicks (view currentTicks <$> time :: EvStream TimeTicks)
  let
    ballVelocity :: Behavior Point
    ballVelocity = return (V2 0 100)
    ballPosition :: Behavior (Behavior Point)
    ballPosition = do
      pos <- integrate currentTime ballVelocity
      return $ (initialBallPosition +) <$> pos
    gameover :: Behavior (Behavior Bool)
    gameover = do
      pos <- ballPosition
      return (((>bottomBorder) .  view _y) <$> pos)
    paddlePosition = calcPaddlePosition (mouseXMovement event)
    ballPaddleCollision :: Behavior (EvStream CollisionData)
    ballPaddleCollision = do
      paddlePos <- paddlePosition
      ballPos <- ballPosition
      return $ catMaybesEs $ toChanges $ detectCollisionRect <$> (paddleRect <$> paddlePos) <*> (ballRect <$> ballPos)
    blocks :: Behavior [Point]
    blocks = do
      return initialBlocks
    normalPicture :: Behavior (Behavior PictureType)
    normalPicture = do
      pp <- paddlePosition
      bp <- ballPosition
      return (createPicture <$> pure pictureData <*> pp <*> bp <*> blocks)
    gameoverPicture :: PictureType -> Behavior PictureType
    gameoverPicture finalPicture = return (pictures [finalPicture,(pictureData ^. pictureBall . _1)])
    --finalPicture :: Behavior (Behavior PictureType)
    finalPicture = do
      {- TODO
      -- Snapshot normalPicture when gameover happens
      -- gameover has to be an event
      -- say it is (Event ())
      -- Switch to gameover behavior which uses the snapshotted normalPicture plus something else

      np <- normalPicture
      go <- gameover
      (when go) :: Behavior (Event ())
      -}
      -- Vorbedingungen:
      --
      -- gameover :: Behavior Bool
      -- normalPicture :: Behavior (Behavior Picture)
      -- gameoverPicture :: Picture -> Behavior Picture
      go <- gameover
      goEvent <- Control.FRPNow.Lib.when go
      normalPic <- normalPicture
      goPic <- snapshot normalPic goEvent
      let
        result :: Behavior PictureType
        result = switch normalPic (gameoverPicture <$> goPic)
      return result
  --
  --normalPic <- sample $ finalPicture
  --return ((`pictureTranslated` (pictureData ^. pictureBall . _1)) <$> ballPos)
  sample finalPicture
-- End game code

-- [p] -> (p -> m) -> m

createPicture :: PictureData -> Point -> Point -> [Point] -> Picture Float Double
createPicture pictureData paddlePos ballPos blocks =
  ((`pictureTranslated` (pictureData ^. pictureBall . _1)) ballPos) <>
  ((`pictureTranslated` (pictureData ^. picturePaddle . _1)) paddlePos) <>
  (foldMap (`pictureTranslated` (pictureData ^. pictureBlock . _1)) blocks)

data MainLoopState p = MainLoopState {
    _statePlatform :: p
  , _stateTickEventCallback :: TickData -> IO ()
  , _stateEventEvent :: EvStream WE.Event
  , _stateEventEventCallback :: WE.Event -> IO ()
  , _stateQuitEventCallback :: () -> IO ()
  , _stateGameBehavior :: Behavior PictureType
  , _stateRenderFunction :: PictureType -> IO ()
  , _stateCurrentTicks :: TimeTicks
  }

$(makeLenses ''MainLoopState)

newMainLoop :: Platform p => MainLoopState p -> Now ()
newMainLoop state = do
  currentTicks <- liftIO getTicks
  let d = currentTicks `tickDelta` (state ^. stateCurrentTicks)
  events <- liftIO $ pollEvents (state ^. statePlatform)
  mapM_ (liftIO . (state ^. stateEventEventCallback)) events
  when (any WE.isQuitEvent events) (liftIO $ state ^. stateQuitEventCallback $ ())
  liftIO $ state ^. stateTickEventCallback $ TickData{_currentTicks = currentTicks,_currentDelta = d}
  currentPicture <- sample (state ^. stateGameBehavior)
  liftIO $ (state ^. stateRenderFunction) currentPicture
  waitFinished <- async (threadDelay (fromSeconds (1/60)))
  _ <- plan (newMainLoop (state & stateCurrentTicks .~ currentTicks) <$ waitFinished)
  return ()

loadPicture md n =
  let rect = fromIntegral <$> view rectDimensions (snd (findSurfaceUnsafe (md ^. mdSurfaces) n))
  in (pictureSprite n rect,rect)

main :: IO ()
main =
  withPlatform (WindowTitle "arkanoid, frp style") (ConstantWindowSize 640 480) MouseGrabYes $ \platform -> do
    md <- readMediaFiles (loadImage platform) "media/images"
    runNowMaster $ do
      (tickEventStream,tickEventCallback) <- callbackStream
      (eventEventStream,eventEventCallback) <- callbackStream
      (quitEvent,quitEventCallback) <- callback
      --clock <- sample $ foldEs (+) 0 tickEventStream
      initialTicks <- liftIO getTicks
      pictureBehavior <- game initialTicks tickEventStream eventEventStream (PictureData{_picturePaddle = loadPicture md "paddle",_pictureBall = loadPicture md "ball",_pictureBlock = loadPicture md "block"})
      let initialState = MainLoopState{
              _statePlatform = platform
            , _stateTickEventCallback = tickEventCallback
            , _stateEventEvent = eventEventStream
            , _stateEventEventCallback = eventEventCallback
            , _stateQuitEventCallback = quitEventCallback
            , _stateGameBehavior = pictureBehavior
            , _stateRenderFunction = wrenchRender platform (md ^. mdSurfaces) (error "no font specified") (Just colorsBlack) . first floor
            , _stateCurrentTicks = initialTicks
            }
      -- event and tick code here
      waitFinished <- async (threadDelay (fromSeconds (1/60)))
      _ <- planNow (newMainLoop initialState <$ waitFinished)
      return quitEvent
