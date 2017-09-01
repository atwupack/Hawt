module UI.Hawt.Type
(
    Color(..), Rectangle(..)
)
where

    import Data.Word

    data Color =    RGBA8 Word8 Word8 Word8 Word8
                    | RGB8 Word8 Word8 Word8

    data Rectangle = R Int Int Int Int