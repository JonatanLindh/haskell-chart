import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk

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
main = renderableToWindow (toRenderable heatmapGraph) 500 500

heatmapGraph = do
    setColors [customGradient (-0.5), customGradient (0.5)]
    layout_title .= "Heat Map Test Layout Title"
    plot heatmaptest

heatmapf :: (Double, Double) -> Double
heatmapf (x,y) = sin (x^2 + y^2) 
heatmaptest = fmap plotHeatMap $ liftEC $ do 
    plot_heatmap_title .= "TESTING PLEASE"
    plot_heatmap_mapf .= heatmapf
    plot_heatmap_grid .= square 50 1

    

customGradient = colorStepsToGradient [(-1, withOpacity blue 0.3), (0, withOpacity green 0.3), (1, withOpacity red 0.3)]
defaultColors :: [(Double, AlphaColour Double)]
defaultColors = [(-1, opaque blue), (0, opaque white), (1, opaque red)]

colorStepsToGradient :: [(Double, AlphaColour Double)] -> (Double -> AlphaColour Double)
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

