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
import           Frp.Ord
import           Linear.V2
import           Linear.Vector               ((*^), (^*))
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.Color
import           Wrench.Engine
import qualified Wrench.Event                as WE
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

eventToPosChange :: Event t WE.Event -> Event t Point
eventToPosChange event = filterJust ((\e -> (e ^? WE._Keyboard) >>= eventToPosChange') <$> event)
  where eventToPosChange' (WE.KeyboardEvent{WE._keyMovement=KeyDown,WE._keySym=KS.Left}) = Just (V2 (-10) 0)
        eventToPosChange' (WE.KeyboardEvent{WE._keyMovement=KeyDown,WE._keySym=KS.Right}) = Just (V2 10 0)
        eventToPosChange' (WE.KeyboardEvent{WE._keyMovement=KeyDown,WE._keySym=KS.Up}) = Just (V2 0 (-10))
        eventToPosChange' (WE.KeyboardEvent{WE._keyMovement=KeyDown,WE._keySym=KS.Down}) = Just (V2 0 10)
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

initialBlocks :: [Point]
initialBlocks = [V2 (leftBorder + x * blockSize ^. _x) (topBorder + y * blockSize ^. _y) | x <- [0..14], y <- [0..5]]

data CollisionDirection = CollisionOnLeft
                        | CollisionOnRight
                        | CollisionOnRoof
                        | CoolisionOnFloor

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TickData -> AddHandler WE.Event -> Handler () -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler quitFire = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    mouseXMovement :: Event t Point
    mouseXMovement = filterJust ((mapped . _y .~ 0) . (^? WE._MouseAxis . WE.mouseAxisDelta) <$> eevent)
    ballCollision :: Event t CollisionDirection
    ballCollision = filterJust ((detectCollision <$> paddlePosition <*> ballPosition) <@ etick)
    detectCollision :: Point -> Point -> Maybe CollisionDirection
    detectCollision paddlePos ballPos | ballPos ^. _x < leftBorder = Just CollisionOnLeft
                                      | ballPos ^. _x + ballSize > rightBorder = Just CollisionOnRight
                                      | ballPos ^. _y < topBorder = Just CollisionOnRoof
                                      | otherwise = Nothing
    deltaVel :: Point -> TickData -> Point
    deltaVel v td = (realToFrac (toSeconds (td ^. currentDelta))) *^ v
    ballPosition :: Behavior t Point
    ballPosition = accumB initialBallPosition ((+) <$> (deltaVel <$> ballVelocity <@> etick))
    transformVelocity :: CollisionDirection -> Point -> Point
    transformVelocity CollisionOnLeft v | v ^. _x < 0 = v & _x %~ negate
    transformVelocity CollisionOnRight v | v ^. _x > 0 = v & _x %~ negate
    transformVelocity _ v = v
    ballVelocity :: Behavior t Point
    ballVelocity = accumB initialBallVelocity (transformVelocity <$> ballCollision)
    createPicture :: Point -> Point -> Picture
    createPicture paddle ball = pictures [paddle `pictureTranslated` pictureSpriteTopLeft "paddle",ball `pictureTranslated` pictureSpriteTopLeft "ball"]
    paddlePosition :: Behavior t Point
    paddlePosition = accumB initialPaddlePosition ((\(V2 x1 y1) (V2 x2 y2) -> V2 (clamp leftBorder (rightBorder - paddleSize ^. _x) (x1+x2)) (y1+y2)) <$> mouseXMovement)
--    currentPictureEvent = ((`pictureTranslated` (pictureSpriteTopLeft "paddle")) <$> paddlePosition) <@ etick
    currentPictureEvent = (createPicture <$> paddlePosition <*> ballPosition) <@ etick
  --let carPosX = accumB 100 (1 <$ keyDownSyms eevent)
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> currentPictureEvent
  let quitEvent = filterE (has (WE._Keyboard . WE.keySym . only KS.Escape)) eevent
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
