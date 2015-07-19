{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           ClassyPrelude
import           Control.Lens                (has, only, (^.), (^?))
import           Control.Monad.Loops         (whileM_)
import           Linear.V2
import qualified Reactive.Banana.Combinators as RBC
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Switch
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.KeyMovement
import qualified Wrench.Keysym               as KS
import           Wrench.MediaData
import           Wrench.Picture
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Time
import           Wrench.WindowSize

initialCarPosition :: Point
initialCarPosition = V2 100 100

eventToPosChange :: RBC.Event t Event -> RBC.Event t Point
eventToPosChange event = RBC.filterJust ((\e -> (e ^? _Keyboard) >>= eventToPosChange') <$> event)
  where eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Left}) = Just (V2 (-10) 0)
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Right}) = Just (V2 10 0)
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Up}) = Just (V2 0 (-10))
        eventToPosChange' (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Down}) = Just (V2 0 10)
        eventToPosChange' _ = Nothing

setupNetwork :: forall t p. Frameworks t => Platform p => p -> SurfaceMap (PlatformImage p) -> AddHandler TimeTicks -> AddHandler Event -> Handler () -> Moment t ()
setupNetwork platform surfaces tickAddHandler eventAddHandler quitFire = do
  etick <- fromAddHandler tickAddHandler
  eevent <- fromAddHandler eventAddHandler
  let
    cumulatedPositionBehavior :: RBC.Behavior t Point
    cumulatedPositionBehavior = RBC.accumB initialCarPosition ((+) <$> eventToPosChange eevent)
    currentPictureEvent = ((`pictureTranslated` (pictureSpriteCentered "car")) <$> cumulatedPositionBehavior) RBC.<@ etick
  --let carPosX = accumB 100 (1 <$ keyDownSyms eevent)
  reactimate $ (wrenchRender platform surfaces (error "no font specified") (Just colorsBlack)) <$> currentPictureEvent
  let quitEvent = RBC.filterE (has (_Keyboard . keySym . only KS.Escape)) eevent
  reactimate $ (\_ -> quitFire ()) <$> quitEvent
  --  reactimate $ (\v -> putStrLn $ "Ah, an event: " <> pack (show v) ) <$> cumulatedPosition

main :: IO ()
main = do
  withPlatform (WindowTitle "test") DynamicWindowSize $ \platform -> do
    (tickAddHandler,tickFire) <- newAddHandler
    (eventAddHandler,eventFire) <- newAddHandler
    (quitAddHandler,quitFire) <- newAddHandler
    md <- liftIO (readMediaFiles (loadImage platform) "media/images")
    quitRef <- newIORef True
    unregisterQuit <- register quitAddHandler (\_ -> writeIORef quitRef False)
    compiledNetwork <- compile (setupNetwork platform (md ^. mdSurfaces) tickAddHandler eventAddHandler quitFire)
    actuate compiledNetwork
    whileM_ (readIORef quitRef) $ do
      ticks <- getTicks
      events <- pollEvents platform
      mapM_ eventFire events
      tickFire ticks
      threadDelay (fromSeconds (1/30))
