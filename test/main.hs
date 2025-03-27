import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Numeric.Noise.Perlin

signal :: [Double] -> [(Double, Double)]
signal xs = [(x, x) | x <- xs]

square a s = [(x, y) | x <- range, y <- range] where range = [-a, -a + s .. a] :: [Double]
crow a s = [(x, y) | x <- range, y <- range, abs x < 5] where range = [-a, -a + s .. a] :: [Double]
grid = square 30 3
bronk = crow 10 1

g x = (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))

f :: (Double, Double) -> (Double, Double)
f (x, y) = (x, y)

vectorField title f grid = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf .= f
    plot_vectors_grid .= grid
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c
    plot_vectors_title .= title
    plot_vectors_values .= zip bronk (map (\(a, b) -> (-b, a)) bronk)

vectorGraph = do
    setColors [customGradient (-0.5), customGradient (0.5)]
    layout_title .= "Positive and Negative Charges"
    plot $ vectorField "Electric Field" f grid
    plot $ vectorField "B-field" (uncurry (flip (curry f))) grid

-- cfile = toFile def "example1_big.png" $ vectorGraph
main = renderableToWindow (toRenderable heatmapGraph) 1000 1000

heatmapGraph = do
    setColors [customGradient (-0.5), customGradient (0.5)]
    layout_title .= "Heat Map Test Layout Title"
    plot heatmaptest

heatmapf :: (Double, Double) -> Double
heatmapf (x, y) = z
  where
    seed = 1
    octaves = 5
    scale = 0.05
    persistance = 0.5
    perlinNoise = perlin seed octaves scale persistance
    z = noiseValue perlinNoise (x, y, 1)

heatmaptest = fmap plotHeatMap $ liftEC $ do
    plot_heatmap_title .= "TESTING PLEASE"
    plot_heatmap_mapf .= heatmapf
    plot_heatmap_grid .= square 200 1

customGradient = colorStepsToGradient [(-1, withOpacity blue 0.3), (0, withOpacity green 0.3), (1, withOpacity red 0.3)]
defaultColors :: [(Double, AlphaColour Double)]
defaultColors = [(-1, opaque blue), (0, opaque white), (1, opaque red)]

-- defaultGradient :: PlotValue z => z -> AlphaColour Double
defaultGradient :: Double -> AlphaColour Double
defaultGradient = colorStepsToGradient defaultColors
