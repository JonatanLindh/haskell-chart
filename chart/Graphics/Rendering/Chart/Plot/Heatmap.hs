-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Heatmap
-- Copyright   :  FIXME
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- FIXME

{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.HeatMap
    ( HeatMap(..)
    , heatmap_title
    , heatmap_gradient
    , heatmap_grid
    , heatmap_mapf
    , defaultHeatMap) where

import           Control.Lens
import           Graphics.Rendering.Chart.Drawing
import           Graphics.Rendering.Chart.Plot.Types
import           Data.Default.Class
import Graphics.Rendering.Chart.Axis.Types (PlotValue)
import Data.Colour (AlphaColour, opaque)

-- | FIXME
data HeatMap x y z = HeatMap { _heatmap_title :: String
                             , _heatmap_gradient :: z -> AlphaColour Double
                             , _heatmap_grid :: [(x, y)]
                             , _heatmap_mapf :: (x, y) -> z
                             }

mapGrid :: (PlotValue x, PlotValue y, PlotValue z)
        => [(x, y)]
        -> ((x, y) -> z)
        -> [((x, y), z)]
mapGrid grid f = map (\xy -> (xy, f xy)) grid



defaultColors = [(-1, opaque (rgbtoC)), (0, opaque white), (1, opaque red)]

defaultGradient :: PlotValue z => z -> AlphaColour Double
defaultGradient z = undefined

defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => HeatMap x y z
defaultHeatMap = HeatMap { _heatmap_title = "Heatmap: Default"
                          , _heatmap_gradient = defaultGradient
                          , _heatmap_grid = []
                          , _heatmap_mapf = const 1
                          }

$(makeLenses ''HeatMap)
