{-# LANGUAGE TemplateHaskell #-}
module OtherMain where

import Wrench.Engine(withPlatform,wrenchRender)
import Wrench.Color(colorsBlack)
import Wrench.MediaData(readMediaFiles,mdSurfaces)
import Wrench.Platform(WindowTitle(..),pollEvents,Platform,loadImage)
import Wrench.WindowSize(WindowSize(..))
import Wrench.MouseGrabMode(MouseGrabMode(..))
import Wrench.Time(TimeTicks,getTicks,threadDelay,fromSeconds,tickDelta,toSeconds,TimeDelta)
import qualified Wrench.Event as WE
import Wrench.Rectangle(rectDimensions)
import Wrench.ImageData(findSurfaceUnsafe)
import Wrench.Picture(Picture,pictureSprite,pictureTranslated)
import ClassyPrelude
import Control.FRPNow.Core(runNowMaster,Behavior,callback,Now,sampleNow,async,planNow)
import Control.FRPNow.Lib(sample,Sample,plan)
import Control.FRPNow.EvStream(EvStream,callbackStream,foldrEv,foldriEv,toChanges,foldEs)
import Control.Lens(makeLenses,(^.),view,(&),(.~))
import Linear.V2(V2(..))

data TickData = TickData { _currentTicks :: TimeTicks, _currentDelta :: TimeDelta }

$(makeLenses ''TickData)

type PictureType = Picture Int Double

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

-- EvStream -> Behavior
game :: EvStream TickData -> EvStream WE.Event -> PictureType -> Now (Behavior PictureType)
game time _ ballPicture = do
  let changeEvents = V2 1 1 <$ time :: EvStream (V2 Int)
  s <- sample $ foldEs (+) (V2 0 0) changeEvents
  return ((`pictureTranslated` ballPicture) <$> s)

newMainLoop :: Platform p => MainLoopState p -> Now ()
newMainLoop state = do
  currentTicks <- liftIO getTicks
  let d = currentTicks `tickDelta` (state ^. stateCurrentTicks)
  events <- liftIO $ pollEvents (state ^. statePlatform)
  mapM_ (liftIO . (state ^. stateEventEventCallback)) events
  when (any WE.isQuitEvent events) (liftIO $ state ^. stateQuitEventCallback $ ())
  liftIO $ state ^. stateTickEventCallback $ (TickData{_currentTicks = currentTicks,_currentDelta = d})
  currentPicture <- sample (state ^. stateGameBehavior)
  liftIO $ (state ^. stateRenderFunction) currentPicture
  waitFinished <- async (threadDelay (fromSeconds (1/60)))
  _ <- plan (newMainLoop (state & stateCurrentTicks .~ currentTicks) <$ waitFinished)
  return ()

fooMain :: IO ()
fooMain =
  withPlatform (WindowTitle "arkanoid, frp style") (ConstantWindowSize 640 480) MouseGrabYes $ \platform -> do
    md <- readMediaFiles (loadImage platform) "media/images"
    let ballPicture = pictureSprite "ball" (fromIntegral <$> view rectDimensions (snd (findSurfaceUnsafe (md ^. mdSurfaces) "ball")))
    runNowMaster $ do
      (tickEventStream,tickEventCallback) <- callbackStream
      (eventEventStream,eventEventCallback) <- callbackStream
      (quitEvent,quitEventCallback) <- callback
      --clock <- sample $ foldEs (+) 0 tickEventStream
      pictureBehavior <- game tickEventStream eventEventStream ballPicture
      initialTicks <- liftIO getTicks
      let initialState = MainLoopState{
              _statePlatform = platform
            , _stateTickEventCallback = tickEventCallback
            , _stateEventEvent = eventEventStream
            , _stateEventEventCallback = eventEventCallback
            , _stateQuitEventCallback = quitEventCallback
            , _stateGameBehavior = pictureBehavior
            , _stateRenderFunction = wrenchRender platform (md ^. mdSurfaces) (error "no font specified") (Just colorsBlack)
            , _stateCurrentTicks = initialTicks
            }
      -- event and tick code here
      waitFinished <- async (threadDelay (fromSeconds (1/60)))
      _ <- planNow (newMainLoop initialState <$ waitFinished)
      return quitEvent
