{-# LANGUAGE ScopedTypeVariables #-}
-- -*-haskell-*-
--
-- Author : Brandon Sloane
--
-- Created: 2 October 2017
--
-- Copyright (C) 2017 Brandon Sloane
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Stability   : provisional
-- Portability : Depends on X11 and ghc
--
-- Utility functions for threading
--
module Data.GI.Gtk.VideoPlayer
    (
-- | Use GStreamer playbin to play a video onto a GtkDrawingArea.
-- This module only exposed a very limited set of functionality: play, pause, seek, and volume.
-- To utilize the full functionality of playbin, one can get the playbin field of the VideoPlayer and use the functions in 'Data.Gst.Objects.Element' and 'Data.GI.Base.Properties' directly.
-- The properties available to playbin are listed at <https://gstreamer.freedesktop.org/data/doc/gstreamer/head/gst-plugins-base-plugins/html/gst-plugins-base-plugins-playbin.html>.
--
-- Be sure to call 'GI.Gst.Functions.init' before using this module.
      CallbackId
    , URI
    , localUri
    , remoteUri
    , VideoPlayer
    , initPlayer
    , addCallback
    , removeCallback
    , setPlayerUri
    , setPlayerState
    , playerPause
    , playerPlay
    , playerSeekNs
    ) where

import qualified GI.Gtk as Gtk 
import qualified GI.Gdk as Gdk 
import qualified GI.Gst as Gst 
import qualified GI.GLib as GLib
import qualified Data.GI.Base.Properties as Properties
import Data.Int
import Data.GI.Base.ManagedPtr (unsafeManagedPtrCastPtr, touchManagedPtr)
import qualified GI.GdkX11 as GdkX11
import qualified GI.GstVideo as GstVideo
import Data.Text (Text)
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as Text
import qualified Data.HashMap as Map 
import Data.HashMap (Map)
import Data.Maybe
import Control.Monad
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Control.Monad.IO.Class
import System.Directory
import Data.Hashable
import GHC.Generics (Generic)

data CallbackId = CallbackId Gst.MessageType Int deriving (Eq, Ord)
instance Hashable CallbackId where
    hashWithSalt salt (CallbackId _ n) = hashWithSalt salt n
data VideoPlayer = VideoPlayer{
                                callbacks :: MVar (Map CallbackId Callback)
                              , nextCallbackId :: MVar Int
                              , playbin :: Gst.Element
                              }

newtype URI = URI Text

type Callback = Gst.Message -> IO Bool

newtype MyGstElement = MyGstElement Gst.Element
instance GstVideo.IsVideoOverlay MyGstElement

localUri :: FilePath -> IO URI
localUri path = URI <$> Text.pack <$> ("file://"++) <$> makeAbsolute path

remoteUri :: String -> IO URI
remoteUri url = return $ URI $ Text.pack url

initPlayer :: (MonadIO m ) => Gtk.DrawingArea -> m VideoPlayer
initPlayer canvas = liftIO $ do
    playbin <- fromJust <$> Gst.elementFactoryMake (Text.pack "playbin") Nothing
    bus <- Gst.elementGetBus playbin
    callbacks :: MVar (Map CallbackId Callback) <- MVar.newMVar (Map.empty)
    nextCallbackId <- MVar.newMVar 0
    let ans = VideoPlayer{ callbacks=callbacks 
                         , nextCallbackId=nextCallbackId
                         , playbin=playbin   
                         }
    --register a callback to handle bus messages
    Gst.busAddWatch bus GLib.PRIORITY_DEFAULT $ \_ msg -> do
        msgTypes <- Gst.getMessageType msg
        callbackMap <- MVar.readMVar  callbacks
        forM_ (Map.assocs callbackMap) $ \(callbackId, callback) -> do
            let CallbackId msgType _ = callbackId
            when (msgType `elem` msgTypes) $ do
                keep <- callback msg
                unless keep $ removeCallback ans callbackId
        return True --prevent gstreamer from removing this watch

    --Bind the player to the provided drawing area
    gdkWindow <- fromJust <$> Gtk.widgetGetWindow canvas
    x11Window <- Gtk.unsafeCastTo GdkX11.X11Window gdkWindow
    xid <- fromIntegral <$> GdkX11.x11WindowGetXid x11Window

    GstVideo.videoOverlaySetWindowHandle (MyGstElement playbin) xid
    return ans

addCallback :: (MonadIO m) => VideoPlayer -> Gst.MessageType -> Callback -> m CallbackId
addCallback player messageType callback = liftIO $ do
    callbackMap <- MVar.takeMVar $ callbacks player
    id <- MVar.takeMVar $ nextCallbackId player
    MVar.putMVar (nextCallbackId player) $ id+1
    let callbackId = CallbackId messageType id
    let callbackMap' = Map.insert callbackId callback callbackMap
    MVar.putMVar (callbacks player) callbackMap'
    return callbackId

removeCallback :: (MonadIO m) => VideoPlayer -> CallbackId -> m ()
removeCallback player callbackId = liftIO $ MVar.modifyMVar_ (callbacks player) (return . Map.delete callbackId)

setPlayerUri :: (MonadIO m) => URI -> VideoPlayer -> m ()
setPlayerUri (URI uri) player = liftIO $ do
    Properties.setObjectPropertyString (playbin player) "uri" (Just uri)

setPlayerState :: (MonadIO m) => Gst.State -> VideoPlayer -> m Gst.StateChangeReturn
setPlayerState state player = liftIO $ Gst.elementSetState (playbin player) state

playerPause :: (MonadIO m) => VideoPlayer -> m Gst.StateChangeReturn
playerPause = setPlayerState Gst.StatePaused

playerPlay :: (MonadIO m) => VideoPlayer -> m Gst.StateChangeReturn
playerPlay = setPlayerState Gst.StatePlaying

playerSeekNs :: (MonadIO m) => VideoPlayer -> Int64 -> m Bool
playerSeekNs player target = liftIO $ do
    Gst.elementSeekSimple (playbin player) Gst.FormatTime [] target

playerGetPosistion :: (MonadIO m) => VideoPlayer -> m (Maybe Int64)
playerGetPosistion player = liftIO $ do
    (hasAns, ans) <- Gst.elementQueryPosition (playbin player) Gst.FormatTime
    if hasAns then
        return $ Just ans
    else
        return $ Nothing

playerGetVolume :: (MonadIO m) => VideoPlayer -> m Double
playerGetVolume player = liftIO $ do
    Properties.getObjectPropertyDouble (playbin player) "volume"

playerSetVolume :: (MonadIO m) => VideoPlayer -> Double -> m ()
playerSetVolume player volume = liftIO $ do
    Properties.setObjectPropertyDouble (playbin player) "volume" volume
