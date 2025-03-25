-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Graphics.Rendering.Chart.Plot.Heatmap
Copyright   :  FIXME
License     :  BSD-style (see chart/COPYRIGHT)

FIXME
-}
module Graphics.Rendering.Chart.Plot.HeatMap (
  PlotHeatMap (..),
  plot_heatmap_title,
  plot_heatmap_gradient,
  plot_heatmap_grid,
  plot_heatmap_mapf,
  defaultHeatMap,
  plotHeatMap,
) where

import Control.Lens
import Data.Colour (
  AlphaColour,
  blend,
  opaque,
 )
import Data.Colour.Names (blue, red, white)
import Data.Default.Class
import Graphics.Rendering.Chart.Axis.Types (PlotValue (fromValue, toValue))
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry hiding (xy)
import Graphics.Rendering.Chart.Plot.Types

{- | FIXME
?Should? Be? z -> AlphaColour Double
-}
data PlotHeatMap x y z = HeatMap
  { _plot_heatmap_title :: String
  , _plot_heatmap_gradient :: Double -> AlphaColour Double
  , _plot_heatmap_grid :: [(x, y)]
  , _plot_heatmap_mapf :: (x, y) -> z
  }

mapGrid ::
  (PlotValue x, PlotValue y, PlotValue z) =>
  [(x, y)] ->
  ((x, y) -> z) ->
  [((x, y), z)]
mapGrid grid f = map (\xy -> (xy, f xy)) grid

plotHeatMap ::
  (PlotValue x, PlotValue y, PlotValue z) =>
  PlotHeatMap x y z ->
  Plot x y
plotHeatMap phm =
  Plot
    { _plot_render = renderPlotHeatMap phm
    , _plot_legend = [(_plot_heatmap_title phm, renderPlotLegendHeatMap phm)]
    , _plot_all_points = unzip $ _plot_heatmap_grid phm
    }

renderPlotHeatMap ::
  (PlotValue x, PlotValue y, PlotValue z) =>
  PlotHeatMap x y z ->
  PointMapFn x y ->
  BackendProgram ()
renderPlotHeatMap p pmap =
  mapM_ draw (_plot_heatmap_grid p)
 where
  draw xy = drawPoint ps (mapXY pmap xy)
   where
    z = f xy
    c = gradient (toValue z)
    ps = filledPolygon 2 4 False c

  f = _plot_heatmap_mapf p
  gradient = _plot_heatmap_gradient p

renderPlotLegendHeatMap :: PlotHeatMap x y z -> Rect -> BackendProgram ()
renderPlotLegendHeatMap p (Rect p1 p2) = do
  drawPoint ps (Point (p_x p1) y)
  drawPoint ps (Point ((p_x p1 + p_x p2) / 2) y)
  drawPoint ps (Point (p_x p2) y)
 where
  ps = def
  y = (p_y p1 + p_y p2) / 2

defaultColors :: [(Double, AlphaColour Double)]
defaultColors = [(-1, opaque blue), (0, opaque white), (1, opaque red)]

colorStepsToGradient ::
  [(Double, AlphaColour Double)] -> (Double -> AlphaColour Double)
colorStepsToGradient = aux
 where
  aux ((z1, c1) : (z2, c2) : rest) z
    | z <= z1 = c1
    | z <= z2 = blend ((z - z1) / (z2 - z1)) c2 c1
    | otherwise = aux ((z2, c2) : rest) z
  aux [(_, c1)] _ = c1
  aux [] _ = opaque white

-- defaultGradient :: PlotValue z => z -> AlphaColour Double
defaultGradient :: Double -> AlphaColour Double
defaultGradient = colorStepsToGradient defaultColors

defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => PlotHeatMap x y z
defaultHeatMap =
  HeatMap
    { _plot_heatmap_title = "Heatmap: Default"
    , _plot_heatmap_gradient = defaultGradient
    , _plot_heatmap_grid = []
    , _plot_heatmap_mapf = const (fromValue 1)
    }

instance (PlotValue x, PlotValue y, PlotValue z) => Default (PlotHeatMap x y z) where
  def = defaultHeatMap

$(makeLenses ''PlotHeatMap)
