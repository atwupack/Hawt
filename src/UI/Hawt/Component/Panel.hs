module UI.Hawt.Component.Panel
(
    createPanel
)
where

    import UI.Hawt.Component
    import UI.Hawt.Render

    data Panel = Panel {    color :: Color,
                            content :: Component }

    instance IsComponent Panel where
        draw panel (w, h) = do
            setDrawColor $ color panel
            fillRect $ R 0 0 w h
            draw (content panel) (w,h)
        getPrefSize panel = getPrefSize (content panel)
        handleEvent panel event = return panel

    createPanel :: Color -> Component -> IO Component
    createPanel c comp = createComponent $ Panel { color = c, content = comp }
