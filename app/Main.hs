{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.Hawt.Application
import UI.Hawt.Component
import UI.Hawt.Component.BorderLayout
import UI.Hawt.Component.Panel
import UI.Hawt.Component.Label
import UI.Hawt.Component.Button
import UI.Hawt.Type

main :: IO ()
main = do
    label <- createLabel "Text" (RGB8 0 0 0)
    tp <- createPanel (RGB8 0 0 255) label
    eb <- createEmpty
    bp <- createPanel (RGB8 0 255 0) eb
    el <- createEmpty
    lp <- createPanel (RGB8 255 0 0) el
    er <- createEmpty
    rp <- createPanel (RGB8 255 0 255) er
    button <- createButton (RGB8 170 170 170) "Click me!" (RGB8 0 0 0)
    cp <- createPanel (RGB8 255 255 255) button
    bl <- createBorderLayout tp bp lp rp cp
    app <- createApplication "My SDL Application" bl
    runApplication app
