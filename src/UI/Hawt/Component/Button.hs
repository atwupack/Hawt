module UI.Hawt.Component.Button
(
    createButton
)
where

    import Data.Text
    import UI.Hawt.Component
    import UI.Hawt.Render

    data Button = Button {  label :: Text,
                            textColor :: Color,
                            color :: Color,
                            pressed :: Bool,
                            highlighted :: Bool}

    instance IsComponent Button where
        draw button (w, h) = do
            setDrawColor $ color button
            fillRect $ R 0 0 w h 
            drawText (label button) (textColor button) (w `div` 2) (h `div` 2) Center Center
        getPrefSize button = do
            (w, h) <- getTextSize (label button)
            return (10 + fromIntegral w, 10 + fromIntegral h)
        handleEvent button event = return button


    createButton :: Color -> Text -> Color -> IO Component
    createButton c l lc = createComponent $ Button l lc c False False