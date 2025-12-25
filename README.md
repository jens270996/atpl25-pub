# atpl25-pub




## Executables
The repository contains some executables for playing with the functionalities of the HZX library.

### Convert
The `Convert.hs` executable converts the example quantum circuit specfied by the `circuit` variable. Then prints the resulting ZX-diagram.
Can be run by `cabal run Convert`

### Visualize
The `Visualize.hs` executable converts a quantum circuit specfied by the `circuit` variable to ZX, and visualizes the result by producing a .svg file. It can be run by `Visualize -- -o [output file] -w [width of image]`. For example `Visualize -- -o test.svg -w 400`.