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
import           Graphics.Rendering.Chart.Axis.Types (PlotValue(fromValue))
import           Data.Colour (AlphaColour, opaque, blend)
import           Data.Colour.Names (blue, white, red)

-- | FIXME
 -- ?Should? Be? z -> AlphaColour Double
data HeatMap x y z =
  HeatMap { _heatmap_title :: String
          , _heatmap_gradient :: Double -> AlphaColour Double
          , _heatmap_grid :: [(x, y)]
          , _heatmap_mapf :: (x, y) -> z
          }

mapGrid :: (PlotValue x, PlotValue y, PlotValue z)
        => [(x, y)]
        -> ((x, y) -> z)
        -> [((x, y), z)]
mapGrid grid f = map (\xy -> (xy, f xy)) grid

defaultColors :: [(Double, AlphaColour Double)]
defaultColors = [(-1, opaque blue), (0, opaque white), (1, opaque red)]

colorStepsToGradient
  :: [(Double, AlphaColour Double)] -> (Double -> AlphaColour Double)
colorStepsToGradient = aux
  where
    aux ((z1, c1):(z2, c2):rest) z
      | z <= z1 = c1
      | z <= z2 = blend ((z - z1) / (z2 - z1)) c2 c1
      | otherwise = aux ((z2, c2):rest) z
    aux [(_, c1)] _ = c1
    aux [] _ = opaque white

-- defaultGradient :: PlotValue z => z -> AlphaColour Double
defaultGradient :: Double -> AlphaColour Double
defaultGradient = colorStepsToGradient defaultColors

defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => HeatMap x y z
defaultHeatMap = HeatMap { _heatmap_title = "Heatmap: Default"
                         , _heatmap_gradient = defaultGradient
                         , _heatmap_grid = []
                         , _heatmap_mapf = const (fromValue 1)
                         }

$(makeLenses ''HeatMap)



