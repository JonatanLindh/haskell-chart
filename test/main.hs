import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

graph = do
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))
 
cfile = toFile def "example1_big.png" $ graph
main = renderableToWindow (toRenderable graph) 500 500
  
