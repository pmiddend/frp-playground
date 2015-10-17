{-# LANGUAGE TemplateHaskell #-}
module Main where

import Wrench.Engine(withPlatform,wrenchRender)
import Wrench.Color(colorsBlack)
import Wrench.MediaData(readMediaFiles,mdSurfaces)
import Wrench.Platform(WindowTitle(..),pollEvents,Platform,loadImage)
import Wrench.WindowSize(WindowSize(..))
import Wrench.MouseGrabMode(MouseGrabMode(..))
import ClassyPrelude
import Control.FRPNow.Core(runNowMaster,Behavior,callback,Now,sampleNow)
import Control.FRPNow.EvStream(EvStream,callbackStream)
import Control.Lens(makeLenses,(^.))
import Wrench.Time(TimeTicks,getTicks,threadDelay,fromSeconds)
import qualified Wrench.Event as WE
import Wrench.Picture(Picture)

type PictureType = Picture Integer Double

data MainLoopState p = MainLoopState {
    _statePlatform :: p
  , _stateTicks :: TimeTicks
  , _stateTickEvent :: EvStream ()
  , _stateTickEventCallback :: () -> IO ()
  , _stateEventEvent :: EvStream WE.Event
  , _stateEventEventCallback :: WE.Event -> IO ()
  , _stateQuitEventCallback :: () -> IO ()
  , _stateGameBehavior :: Behavior PictureType
  , _stateRenderFunction :: PictureType -> IO ()
  }

$(makeLenses ''MainLoopState)

game :: EvStream () -> EvStream WE.Event -> Behavior PictureType
game _ _ = return mempty

mainLoop :: Platform p => MainLoopState p -> Now ()
mainLoop state = do
  newTicks <- liftIO $ getTicks
  let oldTicks = state ^. stateTicks
  events <- liftIO $ pollEvents (state ^. statePlatform)
  mapM_ (\e -> liftIO $ (state ^. stateEventEventCallback) e) events
  when (any WE.isQuitEvent events) (liftIO $ state ^. stateQuitEventCallback $ ())
  liftIO $ state ^. stateTickEventCallback $ ()
  currentPicture <- sampleNow (state ^. stateGameBehavior)
  liftIO $ (state ^. stateRenderFunction) currentPicture
  liftIO $ threadDelay (fromSeconds (1/60))
  mainLoop state

main :: IO ()
main =
  withPlatform (WindowTitle "arkanoid, frp style") (ConstantWindowSize 640 480) MouseGrabYes $ \platform -> do
    initialTicks <- getTicks
    md <- readMediaFiles (loadImage platform) "media/images"
    runNowMaster $ do
      (tickEventStream,tickEventCallback) <- callbackStream
      (eventEventStream,eventEventCallback) <- callbackStream
      (quitEvent,quitEventCallback) <- callback
      let initialState = MainLoopState{
              _statePlatform = platform
            , _stateTicks = initialTicks
            , _stateTickEvent = tickEventStream
            , _stateTickEventCallback = tickEventCallback
            , _stateEventEvent = eventEventStream
            , _stateEventEventCallback = eventEventCallback
            , _stateQuitEventCallback = quitEventCallback
            , _stateGameBehavior = game tickEventStream eventEventStream
            , _stateRenderFunction = wrenchRender platform (md ^. mdSurfaces) (error "no font specified") (Just colorsBlack)
            }
      mainLoop initialState
      return quitEvent
