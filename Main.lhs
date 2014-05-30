> {-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
> module Main where

> import Control.Concurrent
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import Control.Exception
> import System.IO.Error (IOError)
> import qualified System.IO.Error as E
> import Data.Time
> import System.IO
> import System.Locale
> import qualified Data.Concurrent.Queue as Q

> data Msg = Line T.Text
>          | Tick
>          | Done
>          | Error IOError

> gitLine :: IO (Either IOError T.Text)
> gitLine = try TIO.getLine

> ticksProc :: (Q.PutQueue q IO) => Int -> q Msg -> IO ()
> ticksProc interval chan = do
>   threadDelay (interval * 10)
>   Q.put chan $! Tick
>   ticksProc interval chan

> linesProc :: (Q.PutQueue q IO) => q Msg -> IO ()
> linesProc chan = do
>   l <- gitLine
>   case l of
>     Left err 
>       | E.isEOFError err -> do
>         Q.put chan Done
>       | otherwise -> do
>         Q.put chan $ Error err
>     Right line -> do
>       Q.put chan $ Line line
>       linesProc chan

> output :: (Q.TakeQueue q IO) => T.Text -> q Msg -> IO ()
> output state chan = do
>   m <- Q.take chan
>   case m of
>     Error e ->
>       throwIO e
>     Done ->
>       return ()
>     Tick ->
>       display state chan
>     Line line ->
>       display line chan

> display :: (Q.TakeQueue q IO) => T.Text -> q Msg -> IO ()
> display txt chan = do
>   time <- getZonedTime
>   TIO.putStrLn $ T.concat [txt, " | ^fg(#00ff00)", T.pack $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") time]
>   hFlush stdout
>   output txt chan

> main :: IO ()
> main = do
>   chan <- newEmptyMVar
>   _ <- forkIO $ linesProc chan
>   _ <- forkIO $ ticksProc 500 chan
>   output "" chan
