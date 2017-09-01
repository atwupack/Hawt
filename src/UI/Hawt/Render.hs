module UI.Hawt.Render
(
    RenderCtx, RenderEnv(..), Rectangle(..), Color(..), RenderState, Alignment(..),
    setDrawColor, UI.Hawt.Render.fillRect, getViewport, present, createRenderEnv, drawText, getTextSize,
    createRenderState, setOffset, moveOffset, doWithMovedOffset
)
where

    import Data.Word
    import qualified SDL
    import qualified SDL.Font as SDLF
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.State
    import Control.Monad.Trans.Class
    import Foreign.C.Types
    import Linear (V4(..), V2(..))
    import Data.Maybe
    import System.FilePath
    import Data.Text
    import UI.Hawt.Type

    data RenderState = RenderState {    offset :: (Int, Int) }

    createRenderState :: RenderState
    createRenderState = RenderState (0,0)

    setOffset :: Int -> Int -> RenderCtx ()
    setOffset x y = do
        state <- lift $ get
        lift $ put $ state { offset = (x,y) }

    moveOffset :: Int -> Int -> RenderCtx ()
    moveOffset x y = do
        state <- lift $ get
        let (ox, oy) = offset state
        lift $ put $ state { offset = (ox + x, oy + y) }
        
    doWithMovedOffset :: Int -> Int -> RenderCtx a -> RenderCtx a
    doWithMovedOffset x y operation = do
        state <- lift $ get
        let (ox, oy) = offset state
        moveOffset x y
        result <- operation
        setOffset ox oy
        return result

    data RenderEnv = RenderEnv {    renderer :: SDL.Renderer,
                                    font :: SDLF.Font }

    createRenderEnv :: SDL.Window -> IO RenderEnv
    createRenderEnv win = do
        SDLF.initialize
        renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
        font <- SDLF.load ("data"</>"arial.ttf") 11
        return $ RenderEnv renderer font

    type RenderCtx a = ReaderT RenderEnv (StateT RenderState IO) a    

    toSdlColor :: Color -> V4 Word8
    toSdlColor (RGBA8 r g b a) = V4 r g b a
    toSdlColor (RGB8 r g b) = V4 r g b 255

    toSdlRect :: Rectangle -> SDL.Rectangle CInt
    toSdlRect (R x y width height) = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral width) (fromIntegral height))

    fromSdlRect :: SDL.Rectangle CInt -> Rectangle
    fromSdlRect (SDL.Rectangle (SDL.P (V2 x y)) (V2 width height)) = R (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)

    data Alignment =    Leading
                        | Center
                        | Trailing

    getTextSize :: Text -> RenderCtx (Int, Int)
    getTextSize text = do
        env <- ask
        SDLF.size (font env) text    

    drawText :: Text -> Color -> Int -> Int -> Alignment -> Alignment -> RenderCtx ()
    drawText text color x y halign valign = do
        env <- ask        
        (ox,oy) <- offset <$> (lift $ get)
        surf <- SDLF.blended (font env) (toSdlColor color) text
        t <- SDL.createTextureFromSurface (renderer env) surf
        tinfo <- SDL.queryTexture t
        let width = fromIntegral (SDL.textureWidth tinfo)
            xp =    case halign of
                        Leading -> x
                        Center -> x - width `div` 2
                        Trailing -> x - width
            height = fromIntegral (SDL.textureHeight tinfo)    
            yp =    case valign of
                        Leading -> y
                        Center -> y - height `div` 2
                        Trailing -> y - height
        SDL.copy (renderer env) t Nothing (Just (toSdlRect (R (ox + xp) (oy + yp) width height)))
        SDL.destroyTexture t
        SDL.freeSurface surf

    setDrawColor :: Color -> RenderCtx ()
    setDrawColor color = do
        env <- ask
        SDL.rendererDrawColor (renderer env) SDL.$= (toSdlColor color)

    fillRect :: Rectangle -> RenderCtx ()
    fillRect (R x y w h) = do
        env <- ask
        (ox,oy) <- offset <$> (lift $ get)        
        SDL.fillRect (renderer env) $ Just (toSdlRect (R (x + ox) (y + oy) w h))

    getViewport :: RenderCtx Rectangle
    getViewport = do
        env <- ask
        vp <- SDL.get $ SDL.rendererViewport (renderer env)
        return $ fromSdlRect (fromJust vp)

    present :: RenderCtx ()
    present = do
        env <- ask
        SDL.present (renderer env)
