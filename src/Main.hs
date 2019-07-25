{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Distributed.Process         (Process)
import           Control.Distributed.Process         hiding (bracket)
import           Control.Distributed.Process.Closure (SerializableDict (..),
                                                      staticDecode)
import           Control.Distributed.Static          (closure, staticApply,
                                                      staticCompose,
                                                      staticLabel, staticPtr)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Binary                         (Binary, encode)
import           Data.Data                           (Typeable)
import           GHC.StaticPtr                       (StaticPtr)
import           System.Posix.Files                  (readSymbolicLink)

fstStatic :: Static ((a,b) -> a)
fstStatic = staticLabel "$fst"

sndStatic :: Static ((a,b) -> b)
sndStatic = staticLabel "$snd"

type RemoteFunction a b = (a -> Process b, (SerializableDict a, SerializableDict b))

bindRemoteStatic
  :: (Binary a, Typeable a, Typeable b, Typeable c)
  => StaticPtr (a -> c, (SerializableDict a, SerializableDict b))
  -> a
  -> (Static (SerializableDict b), Process (Closure c))
bindRemoteStatic kRemotePtr a = (bDict, closureProcess)
  where
    closureProcess =  do
      return $ closure (f `staticCompose` staticDecode aDict) (encode a)
    s     = staticPtr kRemotePtr
    f     = fstStatic `staticApply` s
    aDict = fstStatic `staticApply` (sndStatic `staticApply` s)
    bDict = sndStatic `staticApply` (sndStatic `staticApply` s)

applyRemoteStatic
  :: (Binary a, Typeable a, Typeable b, Typeable c)
  => StaticPtr (a -> c, (SerializableDict a, SerializableDict b))
  -> a
  -> (Static (SerializableDict b), Process (Closure c))
applyRemoteStatic k a = k `bindRemoteStatic` a

remoteFnIO
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => (a -> IO b)
  -> RemoteFunction a b
remoteFnIO f = (liftIO . f, (SerializableDict, SerializableDict))

myExecutable :: IO FilePath
myExecutable = readSymbolicLink "/proc/self/exe"

data WorkerLaunchConfig = WorkerLaunchConfig
  { withLaunchedWorker :: forall b . Process b -> Process b }

workerLaunchConfigWithRunCmd
  :: MonadIO m
  => ((String, [String]) -> Process ())
  -> m WorkerLaunchConfig
workerLaunchConfigWithRunCmd runCmd = liftIO $ do
  hyperionExec <- myExecutable
  let
    withLaunchedWorker :: forall b . Process b -> Process b
    withLaunchedWorker go = do
      runCmd (hyperionExec, [])
      go
  return WorkerLaunchConfig{..}

withNodeLauncher
  :: Bool
  -> (WorkerLaunchConfig -> Process a)
  -> Process a
withNodeLauncher _ go = do
    sshLaunchConfig <- workerLaunchConfigWithRunCmd undefined
    withLaunchedWorker sshLaunchConfig $ foo
  where
    foo =
      let runCmdOnNode = (\(_,_) -> undefined) . applyRemoteStatic (static (remoteFnIO (const (return ()))))
      in workerLaunchConfigWithRunCmd runCmdOnNode >>= go

main :: IO ()
main = return ()

