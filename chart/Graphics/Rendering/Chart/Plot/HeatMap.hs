{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Graphics.Rendering.Chart.Plot.Heatmap
Copyright   :  FIXME
License     :  BSD-style (see chart/COPYRIGHT)

This module provides functionality for creating heat maps, which are graphical
representations of data where values are represented as colors. Heat maps are
useful for visualizing matrix-like data or continuous functions over a
2D domain.
-}
module Graphics.Rendering.Chart.Plot.HeatMap (
  PlotHeatMap (..),
  plot_heatmap_legend_text,
  plot_heatmap_gradient,
  plot_heatmap_grid,
  plot_heatmap_mapf,
  defaultHeatMap,
  plotHeatMap,
  colorStepsToGradient,
) where

import Control.Lens
import Data.Colour (
  AlphaColour,
  black,
  blend,
  opaque,
 )
import Data.Colour.Names (blue, red, white)
import Data.Default.Class
import Graphics.Rendering.Chart.Axis.Types (PlotValue (toValue))
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry hiding (xy)
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Renderable (Rectangle (_rect_fillStyle, _rect_minsize), drawRectangle)

{- | A specification for a heat map plot. A heat map visualizes a function
mapping from (x,y) coordinates to values that are represented as colors.
-}
data PlotHeatMap x y = HeatMap
  { _plot_heatmap_legend_text :: String
  -- ^ The title of the heat map plot, used in legends.
  , _plot_heatmap_gradient :: Double -> AlphaColour Double
  -- ^ A function that maps values to colors. The default colors expect
  -- z-values that are normalized to [-1, 1]. This can be customized however.
  , _plot_heatmap_grid :: [(x, y)]
  -- ^ The grid points at which to sample the mapping function.
  , _plot_heatmap_mapf :: (x, y) -> Double
  -- ^ The function that maps coordinates to values, which will then be mapped to colors.
  }

{- | Convert a 'PlotHeatMap' to a 'Plot', which can be rendered on a chart.
This function handles the necessary conversions between the heat map-specific
representation and the general plotting framework.
-}
plotHeatMap ::
  (PlotValue x, PlotValue y) =>
  PlotHeatMap x y ->
  Plot x y
plotHeatMap phm =
  Plot
    { _plot_render = renderPlotHeatMap zs phm
    , _plot_legend =
        [ (_plot_heatmap_legend_text phm, const (return ()))
        , (show minZ, const (return ()))
        , (" ", renderPlotLegendHeatMap (minZ, maxZ) phm)
        , (" ", const (return ()))
        , (show maxZ, const (return ()))
        ]
    , _plot_all_points = unzip $ _plot_heatmap_grid phm
    }
 where
  zs = map (_plot_heatmap_mapf phm) $ _plot_heatmap_grid phm
  minZ = foldr min (head zs) zs
  maxZ = foldr max (head zs) zs

{- | Render a heat map plot. This function is typically not called directly,
but is used by the chart rendering system.
-}
renderPlotHeatMap ::
  (PlotValue x, PlotValue y) =>
  [Double] ->
  PlotHeatMap x y ->
  PointMapFn x y ->
  BackendProgram ()
renderPlotHeatMap zs phm pmap =
  mapM_ draw (zip zs (_plot_heatmap_grid phm))
 where
  draw (z, xy) = drawRectangle (mapXY pmap xy) rect
   where
    rect =
      def
        { _rect_minsize = (unitX + 1, unitY - 1) -- +-1 to fix white lines :/
        , _rect_fillStyle = Just (FillStyleSolid c)
        }
    c = gradient z
  gradient = _plot_heatmap_gradient phm

  (pts_x, pts_y) = unzip $ _plot_heatmap_grid phm
  (min_x, max_x) = minmax pts_x
  (min_y, max_y) = minmax pts_y

  xrange = toValue max_x - toValue min_x
  yrange = toValue max_y - toValue min_y

  pmin = pmap (LMin, LMin)
  pmax = pmap (LMax, LMax)

  (Vector actual_width actual_height) = psub pmax pmin
  unitX = actual_width / xrange
  unitY = actual_height / yrange

{- | Calculate the minimum and maximum values in a list.
Returns a tuple of (minimum, maximum). The list must be non-empty.
-}
minmax :: (Ord a) => [a] -> (a, a)
minmax x = (foldl min (head x) x, foldl max (head x) x)

renderPlotLegendHeatMap :: (Double, Double) -> PlotHeatMap x y -> Rect -> BackendProgram ()
renderPlotLegendHeatMap (minValue, maxValue) p (Rect p1 p2) = do
  -- drawPoint def p1
  mapM_ drawR [0 .. nsamples]
 where
  Vector areaW areaH = psub p2 p1

  nsamples = 50
  w = 4.5 * areaW / nsamples
  gradie = _plot_heatmap_gradient p

  lerp t v1 v2 = t * v2 + (1 - t) * v1
  drawR n = drawRectangle (pvadd p1 (Vector (n * w) 0)) rect
   where
    c = gradie (lerp (n / nsamples) minValue maxValue)
    rect =
      def
        { _rect_minsize = (w, areaH)
        , _rect_fillStyle = Just (FillStyleSolid c)
        }

{- | Default color scheme for heat maps, using blue for low values, white for
middle values, and red for high values. The values are mapped from -1 to 1.
-}
defaultColors :: [(Double, AlphaColour Double)]
defaultColors = [(-1, opaque blue), (0, opaque white), (1, opaque red)]

{- | Convert a list of (value, color) pairs to a continuous gradient function.
The function linearly interpolates between adjacent colors for values between
the specified points. Values outside the range use the nearest color.
-}
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

{- | The default gradient function for heat maps, using 'defaultColors'.
Maps values from -1 (blue) through 0 (white) to 1 (red).
-}
defaultGradient :: Double -> AlphaColour Double
defaultGradient = colorStepsToGradient defaultColors

{- | A default heat map with the title "Heatmap: Default", using the default
gradient function. The grid is empty, and the mapping function returns the
constant value 1. This is meant to be customized using lens setters.
-}
defaultHeatMap :: (PlotValue x, PlotValue y) => PlotHeatMap x y
defaultHeatMap =
  HeatMap
    { _plot_heatmap_legend_text = "Heatmap: Default"
    , _plot_heatmap_gradient = defaultGradient
    , _plot_heatmap_grid = []
    , _plot_heatmap_mapf = const 1
    }

-- | Default instance for 'PlotHeatMap', using 'defaultHeatMap'.
instance (PlotValue x, PlotValue y) => Default (PlotHeatMap x y) where
  def = defaultHeatMap

$(makeLenses ''PlotHeatMap)
