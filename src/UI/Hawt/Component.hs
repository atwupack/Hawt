module UI.Hawt.Component
(
    Component, createEmpty, createComponent, IsComponent(..)
)
where
    import UI.Hawt.Render
    import UI.Hawt.Event
    import Data.UUID
    import Data.UUID.V4
    import Control.Monad.IO.Class

    data Component = Component {    compId :: UUID,
                                    compDraw :: (Int, Int) -> RenderCtx (),
                                    compPrefSize :: RenderCtx ( Int, Int),
                                    compEvent :: UIEvent -> RenderCtx Component }

    createEmpty :: IO Component    
    createEmpty = do
        uuid <- nextRandom
        return $ Component { compId = uuid,  compDraw = \r -> return (), compPrefSize = return (50, 50),
            compEvent = \e -> do
                c <- liftIO $ createEmpty 
                return c}

    class IsComponent a where
        draw :: a -> (Int, Int) -> RenderCtx ()
        getPrefSize :: a -> RenderCtx ( Int, Int)
        handleEvent :: a -> UIEvent -> RenderCtx a

    instance IsComponent Component where
        draw = compDraw
        getPrefSize = compPrefSize
        handleEvent c e = do
            compEvent c e
            return c

    createComponent :: (IsComponent a) => a -> IO Component
    createComponent comp = do
        uuid <- nextRandom
        return $ Component {    compId = uuid,
                                compDraw = \r -> draw comp r
                                , compPrefSize = getPrefSize comp
                                , compEvent = \e -> do
                                    newComp <- handleEvent comp e                                    
                                    c <- liftIO $ createComponent newComp
                                    return $ c }
