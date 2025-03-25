-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Heatmap
-- Copyright   :  FIXME
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- FIXME

{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Heatmap (Heatmap) where

import           Control.Lens
import           Graphics.Rendering.Chart.Geometry
import           Graphics.Rendering.Chart.Drawing
import           Graphics.Rendering.Chart.Plot.Types
import           Data.Default.Class

-- | FIXME
data Heatmap x y z = Heatmap { _heatmap_title :: String
                             , _heatmap_color_map :: z -> AlphaColour Double
                             , _heatmap_grid :: [(x, y)]
                             , _heatmap_mapf :: (x, y) -> z
                             }

-- defaultColor :: Plotvalue z => z -> AlphaColour Double
-- defaultColor z =
--   opaque . uncurryRGB (rgbUsingSpace sRGBSpace) . hsl 0 0 . toValue

-- defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => HeatMap z x y
-- defaultHeatMap = HeatMap { _heatmap_title = "Heatmap: Default"
--                           , _heatmap_color_map :: z -> AlphaColour Double
--                           , _heatmap_grid :: [(x, y)]
--                           , _heatmap_mapf :: (x, y) -> z
--                           }
