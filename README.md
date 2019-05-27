# gsg
The package provides functions for generating a global sampling grid that can be used for land-cover assessments. The characterisitcs of the grid are a constant sampling intensity, a constant distance between sampling locations along latitudes, and constant distance between circles of latitudes. The package contains a shiny app for displaying and downloading generated grids.

Install the package to your R-environment using install_github("https://github.com/AWF-GAUG/gsg") from the devtools package.
After loading the package - library(gsg) -, the included Shiny app can be started using the launch_app() function.

This package and the related Shiny app were developed in context of a research grant of the German Research Foundation (DFG) (FE1341_2-1).
