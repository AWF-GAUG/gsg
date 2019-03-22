# gsg
The package provides functions for generating a global sampling grid that can be used for land-cover assessments. The characterisitcs of the grid are a constant sampling intensity, a constant distance between sampling locations along latitudes, and constant distance between circles of latitudes. The package contains a shiny app for displaying and downloading generated grids.

Install the package to your R-environment using install_github("https://github.com/AWF-GAUG/gsg") from the devtools package.
After loading the package - library(gsg) -, the included shiny app can be started using the launch_app() function.
