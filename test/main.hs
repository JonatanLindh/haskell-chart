-- \|
-- Module      :  Main
-- Description :  Example usage of heatmap functionality in haskell-chart
-- Copyright   :  (c) The Chart contributors
-- License     :  BSD-style
--
-- This module demonstrates two examples of heat maps:
-- 1. A procedural earth map using Perlin noise
-- 2. A mathematical visualization of a sine wave function

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.HeatMap (plot_heatmap_legend_text)
import Numeric.Noise.Perlin

{- | Creates a square grid of points with specified size and step.

@square a s@ creates a grid of (x,y) points where x and y both range
from -a to a with a step size of s.
-}
square :: Double -> Double -> [(Double, Double)]
square a s = [(x, y) | x <- range, y <- range] where range = [-a, -a + s .. a]

{- | Main entry point that generates example heat map visualizations.
Outputs two PNG files: "map.png" with a procedural terrain map and
"sin.png" with a visualization of a sine wave function.
-}
main :: IO ()
main = do
    putStrLn "Note: `proceduralEarthMap` takes a while since it is 500x500"
    toFile def "map.png" proceduralEarthMap
    toFile def "sin.png" heatmapGraph
    toFile def "suninthesky.png" sunInTheSkyMap

---------------------------------------
-------------- EXAMPLE 1 --------------
---------------------------------------

{- | Creates a procedural earth-like terrain map visualization using Perlin noise.
Uses a 500x500 grid for high resolution, which can be computationally intensive.
-}
proceduralEarthMap = do
    layout_title .= "Procedural Earth-map"
    plot $ fmap plotHeatMap $ liftEC $ do
        plot_heatmap_legend_text .= "Height"
        plot_heatmap_mapf .= noisef
        plot_heatmap_grid .= square 500 1
        plot_heatmap_gradient .= colorStepsToGradient earthColors

{- | Generates a terrain height value at a given coordinate using Perlin noise.

The function uses a fixed seed and noise parameters to create terrain-like
height distributions.
-}
noisef :: (Double, Double) -> Double
noisef (x, y) = z
  where
    seed = 1
    octaves = 20
    scale = 0.01
    persistance = 0.1
    perlinNoise = perlin seed octaves scale persistance
    z = noiseValue perlinNoise (x, y, 1)

-- | Color steps that look like terrain with sea, land, and mountains
earthColors :: [(Double, AlphaColour Double)]
earthColors =
    [ (-1, opaque navy)
    , (-0.5, opaque mediumblue)
    , (0.05, opaque mediumblue)
    , (0.1, opaque moccasin)
    , (0.2, opaque forestgreen)
    , (0.5, opaque forestgreen)
    , (0.6, opaque dimgray)
    , (0.95, opaque dimgray)
    , (1, opaque white)
    ]

---------------------------------------
-------------- EXAMPLE 2 --------------
---------------------------------------

{- | Creates a visualization of a sine wave function on a 2D plane.
Uses a 70x70 grid which provides good detail while remaining computationally
efficient.
-}
heatmapGraph = do
    layout_title .= "Cool sin waves function thing"
    plot $ fmap plotHeatMap $ liftEC $ do
        plot_heatmap_legend_text .= "f(x, y)"
        plot_heatmap_mapf .= coolSinf
        plot_heatmap_grid .= square 70 1

{- | The function creates concentric circular wave patterns by computing
the sine of the squared distance from the origin (scaled by 0.1).
-}
coolSinf :: (Double, Double) -> Double
coolSinf (x, y) = sin (((0.1 * x) ^ 2) + ((0.1 * y) ^ 2))

---------------------------------------
-------------- EXAMPLE 3 --------------
--------------------------------------

{- | 'sunInTheSkyMap' creates a heat map visualization that looks like a
sun in the sky :)
-}
sunInTheSkyMap = do
    layout_title .= "Sun In The Sky"
    plot $ fmap plotHeatMap $ liftEC $ do
        plot_heatmap_legend_text .= "HEAT"
        plot_heatmap_legend_minmax .= Just (0, 100)
        plot_heatmap_mapf .= sunInTheSkyF
        plot_heatmap_grid .= square 10 0.02
        plot_heatmap_gradient .= colorStepsToGradient sunInTheSkyColors

{- | 'sunInTheSkyF' computes the heat value for a given coordinate.
At the origin, the value is set to 100; for other coordinates, the function
scales the value based on the inverse distance from the origin, with an
upper bound of 100.
-}
sunInTheSkyF :: (Double, Double) -> Double
sunInTheSkyF (0, 0) = 100
sunInTheSkyF (x, y) = min 100 (80 / sqrt (x ^ 2 + y ^ 2))

{- | 'sunInTheSkyColors' defines the gradient used in the 'sunInTheSkyMap'.
It specifies key color-stop pairs that map numeric heat values to colors,
transitioning from black through deepskyblue, orange, red, yellow, and finally
white.
-}
sunInTheSkyColors :: [(Double, AlphaColour Double)]
sunInTheSkyColors =
    [ (0, opaque black)
    , (40, opaque deepskyblue)
    , (70, opaque orange)
    , (90, opaque red)
    , (99.95, opaque yellow)
    , (100, opaque white)
    ]