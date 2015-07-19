{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           ClassyPrelude
import           Control.Lens                (below, has, makeLenses, mapped,
                                              only, (%~), (&), (.~), (^.), (^?),
                                              _Just)
import           Control.Monad.Loops         (whileM_)
import           Data.Composition            ((.:))
import           Linear.V2
import           Linear.Vector               ((*^), (^*))
import qualified Reactive.Banana.Combinators as RBC
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.FloatType
import           Wrench.ImageData
import           Wrench.KeyMovement
import qualified Wrench.Keysym               as KS
import           Wrench.MediaData
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Time
import           Wrench.WindowSize

eventToPosChange :: RBC.Event t Event -> RBC.Event t Point
eventToPosChange event = RBC.filterJust ((\e -> (e ^? _Keyboard) >>= eventToPosChange') <$> event)
  where eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Left}) = Just (V2 (-10) 0)
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Right}) = Just (V2 10 0)
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Up}) = Just (V2 0 (-10))
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Down}) = Just (V2 0 10)
        eventToPosChange' _ = Nothing

data TickData = TickData { _currentTicks :: TimeTicks, _currentDelta :: TimeDelta }

$(makeLenses ''TickData)

blockSize = V2 40 20
initialBallVelocity = V2 100 0
initialBallPosition = V2 320 240
paddleSize = V2 62 18
ballSize = 14
initialPaddlePosition :: Point
initialPaddlePosition = V2 320 (480-14-(paddleSize ^. _y))
leftBorder = 20
rightBorder = 640-20
topBorder = 32
topBlockBorder=92

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV v = min maxV (max minV v)

initialBlocks :: [Point]
initialBlocks = [V2 (leftBorder + x * blockSize ^. _x) (topBorder + y * blockSize ^. _y) | x <- [0..14], y <- [0..5]]

data CollisionDirection = CollisionOnLeft
                        | CollisionOnRight
                        | CollisionOnRoof
                        | CoolisionOnFloor

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TickData -> AddHandler Event -> Handler () -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler quitFire = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    mouseXMovement :: RBC.Event t Point
    mouseXMovement = RBC.filterJust ((mapped . _y .~ 0) . (^? _MouseMotion . mouseMotionDelta) <$> eevent)
    ballCollision :: RBC.Event t CollisionDirection
    ballCollision = RBC.filterJust ((detectCollision <$> paddlePosition <*> ballPosition) RBC.<@ etick)
    detectCollision :: Point -> Point -> Maybe CollisionDirection
    detectCollision paddlePos ballPos | ballPos ^. _x < leftBorder = Just CollisionOnLeft
                                      | ballPos ^. _x + ballSize > rightBorder = Just CollisionOnRight
                                      | ballPos ^. _y < topBorder = Just CollisionOnRoof
                                      | otherwise = Nothing
    deltaVel :: Point -> TickData -> Point
    deltaVel v td = (realToFrac (toSeconds (td ^. currentDelta))) *^ v
    ballPosition :: RBC.Behavior t Point
    ballPosition = RBC.accumB initialBallPosition ((+) <$> (deltaVel RBC.<$> ballVelocity RBC.<@> etick))
    transformVelocity :: CollisionDirection -> Point -> Point
    transformVelocity CollisionOnLeft v | v ^. _x < 0 = v & _x %~ negate
    transformVelocity CollisionOnRight v | v ^. _x > 0 = v & _x %~ negate
    transformVelocity _ v = v
    ballVelocity :: RBC.Behavior t Point
    ballVelocity = RBC.accumB initialBallVelocity (transformVelocity <$> ballCollision)
    createPicture :: Point -> Point -> Picture
    createPicture paddle ball = pictures [paddle `pictureTranslated` pictureSpriteTopLeft "paddle",ball `pictureTranslated` pictureSpriteTopLeft "ball"]
    paddlePosition :: RBC.Behavior t Point
    paddlePosition = RBC.accumB initialPaddlePosition ((\(V2 x1 y1) (V2 x2 y2) -> V2 (clamp leftBorder (rightBorder - paddleSize ^. _x) (x1+x2)) (y1+y2)) <$> mouseXMovement)
--    currentPictureEvent = ((`pictureTranslated` (pictureSpriteTopLeft "paddle")) <$> paddlePosition) RBC.<@ etick
    currentPictureEvent = (createPicture <$> paddlePosition <*> ballPosition) RBC.<@ etick
  --let carPosX = accumB 100 (1 <$ keyDownSyms eevent)
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> currentPictureEvent
  let quitEvent = RBC.filterE (has (_Keyboard . keySym . only KS.Escape)) eevent
  reactimate $ (\_ -> quitFire ()) <$> quitEvent
  reactimate $ (\v -> putStrLn $ "Ah, an event: " <> pack (show v) ) <$> mouseXMovement

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
