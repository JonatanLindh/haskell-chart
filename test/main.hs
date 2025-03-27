import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Numeric.Noise.Perlin

square :: Double -> Double -> [(Double, Double)]
square a s = [(x, y) | x <- range, y <- range] where range = [-a, -a + s .. a]

main :: IO ()
main = toFile def "map.png" proceduralEarthMap

heatmapGraph = do
    layout_title .= "Cool sin waves function thing"
    plot $ fmap plotHeatMap $ liftEC $ do
        plot_heatmap_title .= "f(x, y)"
        plot_heatmap_mapf .= coolSinf
        plot_heatmap_grid .= square 70 1

coolSinf :: (Double, Double) -> Double
coolSinf (x, y) = sin (((0.1 * x) ^ 2) + ((0.1 * y) ^ 2))

proceduralEarthMap = do
    layout_title .= "Procedural Earth-map"
    plot $ fmap plotHeatMap $ liftEC $ do
        plot_heatmap_title .= "Height"
        plot_heatmap_mapf .= noisef
        plot_heatmap_grid .= square 500 1
        plot_heatmap_gradient .= colorStepsToGradient earthColors

noisef :: (Double, Double) -> Double
noisef (x, y) = z
  where
    seed = 1
    octaves = 20
    scale = 0.01
    persistance = 0.1
    perlinNoise = perlin seed octaves scale persistance
    z = noiseValue perlinNoise (x, y, 1)

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
