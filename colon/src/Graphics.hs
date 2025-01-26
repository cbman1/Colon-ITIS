-- Graphics.hs
module Graphics (
    initializeGraphics,
    updateGraphics,
    shutdownGraphics
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State
import Memory
import GraphicsState
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map

-- Размер окна
windowWidth :: Int
windowWidth = 600

windowHeight :: Int
windowHeight = 600

-- Размер пикселя
pixelSize :: Float
pixelSize = 10.0

-- Инициализация графического состояния
initializeGraphics :: MemoryState -> IO (TVar GraphicsState)
initializeGraphics (_, _, _, gs) = do
    tvar <- newTVarIO gs
    -- Запуск окна Gloss
    forkIO $ playIO (InWindow "Colon Interpreter Graphics" (windowWidth, windowHeight) (100, 100))
                       black
                       30
                       gs
                       renderGraphics
                       handleEvent
                       (const $ return ()) -- Обновление состояния не требуется
    return tvar

-- Обновление графического состояния при изменении памяти
updateGraphics :: TVar GraphicsState -> Address -> Int -> IO ()
updateGraphics graphicsTVar addr val = do
    -- Предполагаем, что GRAPHICS начинается с адреса 2
    let w = 10 -- WIDTH = 10
        x = (addr - 2) `mod` w
        y = (addr - 2) `div` w
    atomically $ do
        gs <- readTVar graphicsTVar
        let h = height gs
        Control.Monad.when (y < h && x < w) $ do
                let currentPixels = pixels gs
                    row = currentPixels !! y
                    newRow = take x row ++ [val] ++ drop (x + 1) row
                    newPixels = take y currentPixels ++ [newRow] ++ drop (y + 1) currentPixels
                writeTVar graphicsTVar gs { pixels = newPixels }

-- Завершение работы графической системы
shutdownGraphics :: TVar GraphicsState -> IO ()
shutdownGraphics _ = return () -- Gloss сам завершает работу

-- Рендеринг графического состояния
renderGraphics :: GraphicsState -> IO Picture
renderGraphics gs = return $ Pictures $ concatMap renderRow (zip [0..] (pixels gs))
  where
    renderRow :: (Int, [Int]) -> [Picture]
    renderRow (y, row) = zipWith (curry (renderPixel y)) [0..] row

    renderPixel :: Int -> (Int, Int) -> Picture
    renderPixel y (x, val) =
        translate (fromIntegral x * pixelSize - fromIntegral (width gs) * pixelSize / 2)
                  (fromIntegral (height gs) * pixelSize / 2 - fromIntegral y * pixelSize)
                  (color (if val == 1 then white else black) $ rectangleSolid (pixelSize - 1) (pixelSize - 1))

-- Обработка событий (например, нажатие клавиш)
handleEvent :: Event -> GraphicsState -> IO GraphicsState
handleEvent event gs = case event of
    EventKey (Char c) Down _ _ -> return gs { lastKey = Just (fromEnum c) }
    _ -> return gs
