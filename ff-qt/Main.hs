{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Prelude hiding (id)

import           Control.Concurrent (forkIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (Day, toGregorian)
import           Data.Typeable (cast)
import           Data.Version (showVersion)
import           Foreign (Ptr)
import           Foreign.C (CInt)
import           Foreign.StablePtr (newStablePtr)
import qualified Language.C.Inline.Cpp as Cpp
import           RON.Storage.IO (CollectionDocId (CollectionDocId),
                                 DocId (DocId), runStorage, subscribeForever)
import qualified RON.Storage.IO as Storage

import           FF (getDataDir, load, loadActiveTasks)
import           FF.Config (loadConfig)
import           FF.Types (Entity (Entity), Note (Note), NoteId,
                           NoteStatus (TaskStatus), Status (Active), entityId,
                           entityVal, note_end, note_start, note_status,
                           note_text, note_track, track_externalId,
                           track_provider, track_source, track_url)

import           Cpp (MainWindow, ffCtx, includeDependent)
import           Paths_ff_qt (version)

Cpp.context $ Cpp.cppCtx <> Cpp.bsCtx <> ffCtx
includeDependent "FFI/Cxx.hxx"

main :: IO ()
main = do
    cfg     <- loadConfig
    dataDir <- getDataDir cfg
    storage <- Storage.newHandle dataDir

    let version' = stringZ $ showVersion version
    storagePtr <- newStablePtr storage

    -- set up UI
    mainWindow <- [Cpp.exp| MainWindow * {
        proxy_main($bs-ptr:version', $(StorageHandle storagePtr))
    }|]

    -- load current data to the view, asynchronously
    _ <- forkIO $ do
        activeTasks <- runStorage storage loadActiveTasks
        for_ activeTasks $ upsertTask mainWindow

    -- update the view with future changes
    _ <- forkIO $ subscribeForever storage $ upsertDocument storage mainWindow

    -- run UI
    [Cpp.exp| void { qApp_exec() }|]

upsertDocument :: Storage.Handle -> Ptr MainWindow -> CollectionDocId -> IO ()
upsertDocument storage mainWindow (CollectionDocId docid) = case docid of
    (cast -> Just (noteId :: NoteId)) -> do
        note <- runStorage storage $ load noteId
        upsertTask mainWindow note
    _ -> pure ()

upsertTask :: Ptr MainWindow -> Entity Note -> IO ()
upsertTask mainWindow Entity{entityId = DocId id, entityVal = note} = do
    let id' = stringZ id
        isActive = note_status == TaskStatus Active
        Note{note_text, note_start, note_end, note_track, note_status} = note
        text = stringZ note_text
        (startYear, startMonth, startDay) = toGregorianC note_start
        (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
        isTracking = isJust note_track
        provider   = textZ $ foldMap track_provider   note_track
        source     = textZ $ foldMap track_source     note_track
        externalId = textZ $ foldMap track_externalId note_track
        url        = textZ $ foldMap track_url        note_track
    [Cpp.block| void {
        MainWindow_upsertTask(
            $(MainWindow * mainWindow),
            (Note){
                .id = $bs-ptr:id',
                .isActive = $(bool isActive),
                .text = $bs-ptr:text,
                .start = {$(int startYear), $(int startMonth), $(int startDay)},
                .end   = {$(int   endYear), $(int   endMonth), $(int   endDay)},
                .isTracking = $(bool isTracking),
                .track = {
                    .provider   = $bs-ptr:provider,
                    .source     = $bs-ptr:source,
                    .externalId = $bs-ptr:externalId,
                    .url        = $bs-ptr:url,
                },
            }
        );
    }|]

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d) where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day

-- | Zero-terminated C string from 'String'
stringZ :: String -> ByteString
stringZ = textZ . Text.pack

-- | Zero-terminated C string from 'Text'
textZ :: Text -> ByteString
textZ = (`BS.snoc` 0) . Text.encodeUtf8
