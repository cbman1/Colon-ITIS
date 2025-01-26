-- GraphicsState.hs
module GraphicsState (
    GraphicsState(..)
) where

-- Структура, хранящая состояние графики
data GraphicsState = GraphicsState {
    pixels :: [[Int]], -- 2D список пикселей
    width  :: Int,
    height :: Int,
    lastKey :: Maybe Int
}
