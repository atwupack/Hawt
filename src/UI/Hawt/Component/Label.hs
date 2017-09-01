module UI.Hawt.Component.Label
(
    createLabel, Label
)
where

    import Data.Text
    import UI.Hawt.Component
    import UI.Hawt.Render

    data Label = Label {    text :: Text,
                            color :: Color }

    createLabel :: Text -> Color -> IO Component
    createLabel t c = createComponent $ Label t c

    instance IsComponent Label where
        draw label (w, h) = do
            drawText (text label) (color label) 0 0 Leading Leading
        getPrefSize label = do
            (w, h) <- getTextSize (text label)
            return (fromIntegral w, fromIntegral h)
        handleEvent label event = return label
