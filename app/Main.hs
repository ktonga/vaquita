{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Vaquita.Telegram.Bot (startServer)

main :: IO ()
main = startServer

