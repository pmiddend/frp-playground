{-# LANGUAGE TemplateHaskell #-}
module Main where

import Wrench.Engine(withPlatform,wrenchRender)
import Wrench.Color(colorsBlack)
import Wrench.MediaData(readMediaFiles,mdSurfaces)
import Wrench.Platform(WindowTitle(..),pollEvents,Platform,loadImage)
import Wrench.WindowSize(WindowSize(..))
import Wrench.MouseGrabMode(MouseGrabMode(..))
import Wrench.Time(TimeTicks,getTicks,threadDelay,fromSeconds)
import qualified Wrench.Event as WE
import Wrench.Rectangle(rectDimensions)
import Wrench.ImageData(findSurfaceUnsafe)
import Wrench.Picture(Picture,pictureSprite,pictureTranslated)
import ClassyPrelude
import Control.FRPNow.Core(runNowMaster,Behavior,callback,Now,sampleNow,async,planNow)
import Control.FRPNow.Lib(sample,Sample,plan)
import Control.FRPNow.EvStream(EvStream,callbackStream,foldrEv,foldriEv)
import Control.Lens(makeLenses,(^.),view)
import Linear.V2(V2(..))

type PictureType = Picture Integer Double

data MainLoopState p = MainLoopState {
    _statePlatform :: p
  , _stateTickEvent :: EvStream ()
  , _stateTickEventCallback :: () -> IO ()
  , _stateEventEvent :: EvStream WE.Event
  , _stateEventEventCallback :: WE.Event -> IO ()
  , _stateQuitEventCallback :: () -> IO ()
  , _stateGameBehavior :: Behavior PictureType
  , _stateRenderFunction :: PictureType -> IO ()
  }

$(makeLenses ''MainLoopState)

-- EvStream -> Behavior
game :: EvStream () -> a -> PictureType -> Behavior PictureType
game tickEvent _ ballPicture =
  (`pictureTranslated` ballPicture) <$> foldriEv (V2 0 0) (\v _ -> v + V2 1 1 ) (V2 0 0 <$ tickEvent)

newMainLoop :: Platform p => MainLoopState p -> Now ()
newMainLoop state = do
  events <- liftIO $ pollEvents (state ^. statePlatform)
  mapM_ (liftIO . (state ^. stateEventEventCallback)) events
  when (any WE.isQuitEvent events) (liftIO $ state ^. stateQuitEventCallback $ ())
  liftIO $ state ^. stateTickEventCallback $ ()
  currentPicture <- sample (state ^. stateGameBehavior)
  liftIO $ (state ^. stateRenderFunction) currentPicture
  waitFinished <- async (threadDelay (fromSeconds (1/60)))
  _ <- plan (newMainLoop state <$ waitFinished)
  return ()

main :: IO ()
main =
  withPlatform (WindowTitle "arkanoid, frp style") (ConstantWindowSize 640 480) MouseGrabYes $ \platform -> do
    md <- readMediaFiles (loadImage platform) "media/images"
    let ballPicture = pictureSprite "ball" (fromIntegral <$> view rectDimensions (snd (findSurfaceUnsafe (md ^. mdSurfaces) "ball")))
    runNowMaster $ do
      (tickEventStream,tickEventCallback) <- callbackStream
      (eventEventStream,eventEventCallback) <- callbackStream
      (quitEvent,quitEventCallback) <- callback
      let initialState = MainLoopState{
              _statePlatform = platform
            , _stateTickEvent = tickEventStream
            , _stateTickEventCallback = tickEventCallback
            , _stateEventEvent = eventEventStream
            , _stateEventEventCallback = eventEventCallback
            , _stateQuitEventCallback = quitEventCallback
            , _stateGameBehavior = game tickEventStream eventEventStream ballPicture
            , _stateRenderFunction = wrenchRender platform (md ^. mdSurfaces) (error "no font specified") (Just colorsBlack)
            }
      -- event and tick code here
      waitFinished <- async (threadDelay (fromSeconds (1/60)))
      _ <- planNow ((newMainLoop initialState) <$ waitFinished)
      --mainLoop initialState
      return quitEvent
