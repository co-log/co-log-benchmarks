{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main
       ( main
       ) where

import Control.Exception (onException)
import Control.Monad (replicateM_)
import GHC.Stack (HasCallStack, callStack, emptyCallStack)

import Colog (pattern D, LogAction, Message, Msg (..), cmap, cmapM, defaultFieldMap, fmtMessage,
              fmtRichMessageDefault, logByteStringStderr, logByteStringStdout, logPrint,
              logStringStdout, logTextStdout, richMessageAction, upgradeMessageAction, (<&))
import Test.Tasty.Bench ( bench, defaultMain, nfIO, Benchmark )

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding
import qualified Data.Text.IO as Text

-- | List of benchmarks.
benchmarks :: [Benchmark]
benchmarks =
    [ bench "Prelude.putStrLn" $ nfIO $
        runIO putStrLn "message"

    , bench "Text.putStrLn" $ nfIO $
        runIO Text.putStrLn "message"

    , bench "ByteString.putStrLn" $ nfIO $
        runIO ByteString.putStrLn "message"

    , bench "mempty" $ nfIO $
        runLA mempty ("message" :: String)

    , bench "logStringStdout" $ nfIO $
        let la = logStringStdout
        in runLA la "message"

    , bench "logPrint" $ nfIO $
        let la = logPrint
        in runLA la (5 :: Int)

    , bench "logTextStdout" $ nfIO $
        let la = logTextStdout
        in runLA la "message"

    , bench "logByteStringStdout" $ nfIO $
        let la = logByteStringStdout
        in runLA la "message"

    , bench "logByteStringStderr" $ nfIO $
        let la = logByteStringStderr
        in runLA la "message"

    , bench "ByteString > (stdout <> stderr)" $ nfIO $
        let la = logByteStringStdout <> logByteStringStderr
        in runLA la "message"

    , bench "Message > format > stdout" $ nfIO $
        let la = cmap fmtMessage logTextStdout
        in runLA la msg

    , bench "Message > format > ByteString > stdout" $ nfIO $
        let la = cmap
                (Data.Text.Encoding.encodeUtf8 . fmtMessage)
                logByteStringStdout
        in runLA la msg

    , bench "Message{callstack} > format > stdout" $ nfIO $
        let la = cmap fmtMessage logTextStdout
        in runLA la (Msg D callStack "message")

    , bench "Message{callstack:5} > format > stdout" $ nfIO $
        let la = cmap fmtMessage logTextStdout
        in nest 5 $ runLA la (Msg D callStack "message")

    , bench "Message{callstack:50} > format > stdout" $ nfIO $
        let la = cmap fmtMessage logTextStdout
        in nest 50 $ runLA la (Msg D callStack "message")

    , bench "Message{Time,ThreadId} > format > stdout" $ nfIO $
        let messageAction = cmapM fmtRichMessageDefault logTextStdout
            la = upgradeMessageAction defaultFieldMap messageAction
        in runLA la msg

    , bench "Message{Time,ThreadId} > format > ByteString > stdout" $ nfIO $
        runLA richMessageAction msg
    ]
  where
    samples10K :: Int
    samples10K = 10000

    msg :: Message
    msg = Msg D emptyCallStack "message"

    -- run @LogAction IO@ over single message 10K times
    runLA :: LogAction IO a -> a -> IO ()
    runLA la a = replicateM_ samples10K (la <& a)

    -- runs IO action 10 K times
    runIO :: (a -> IO ()) -> a -> IO ()
    runIO action = replicateM_ samples10K .  action

    nest :: HasCallStack => Int -> IO a -> IO a
    nest 0 f = f
    nest n f = nest (n - 1) f `onException` pure () -- force nesting

main :: IO ()
main = defaultMain benchmarks
