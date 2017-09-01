module UI.Hawt.Application
(
    createApplication, runApplication
)
where
    import SDL hiding (present)
    import Data.Text (Text)
    import Control.Monad (unless)
    import UI.Hawt.Component
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.State hiding (state)
    import UI.Hawt.Render    

    data Application = Application {    renderEnv :: RenderEnv,
                                        root :: Component }

    createApplication :: Text -> Component -> IO Application
    createApplication title rootComponent = do
        initializeAll
        let windowConfig = defaultWindow { windowResizable = True }
        window <- createWindow title windowConfig
        renderEnv <- createRenderEnv window                
        return $ Application renderEnv rootComponent

    runApplication :: Application -> IO ()
    runApplication app = do
        let state = createRenderState
        loop app state

    loop :: Application -> RenderState -> IO ()
    loop app state = do
        events <- waitEvent
        let eventIsQPress event =
                case eventPayload event of
                    KeyboardEvent keyboardEvent ->
                        keyboardEventKeyMotion keyboardEvent == Pressed &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                    _ -> False
            qPressed = eventIsQPress events
            env = renderEnv app
        newState <- execStateT (runReaderT (drawRoot (root app)) env) state
        unless qPressed (loop app newState)

    drawRoot :: Component -> RenderCtx ()
    drawRoot root = do
        R x y w h <- getViewport
        draw root (w,h)
        present
