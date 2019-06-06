#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
shiny_app_ui <- fluidPage(
  shinyjs::useShinyjs(),

  navbarPage(
    "Global Sampling Grid",
    id = "nav",

    tabPanel(
      "Interactive map",
      value = "gomap",

      div(
        class = "outer",
        tags$head(# Include our custom CSS
          includeCSS("inst/shiny_app/styles.css")
        ),

        leafletOutput("map", height = "100%", width = "100%")
      )
    ),

    tabPanel("Generate/ download GSG",
             div(id = "settings",
                 fluidRow(
                   column(4,
                          wellPanel(
                            # Inputs
                            h4("GSG Area"),

                            # Select country code
                            selectInput(
                              "country_code",
                              "Select (multiple) countries",
                              c("World", raster::ccodes()[, 1]),
                              c("world", raster::ccodes()[, 2]),
                              multiple = TRUE
                            ),

                            # Input administrative level
                            selectInput(
                              "adm_level",
                              "Administrative level",
                              c(
                                "Level 0" = "0",
                                "Level 1" = "1",
                                "Level 2" = "2"
                              )
                            ),

                            radioButtons(
                              inputId = 'inputformat',
                              label = 'Upload specific aoi as shapefile (epsg:4326) or KML',
                              choices = c('Shapefile' = 'shp', 'KML' = 'kml'),
                              inline = TRUE
                            ),

                            # File input for aoi
                            fileInput(
                              "aoi",
                              "For .shp upload select the .shp, .prj, .shx and .dbf file simultaneously!",
                              accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', ".prj", ".kml"),
                              multiple = TRUE
                            )
                          )),

                   column(4,
                          wellPanel(
                            h4("GSG settings"),
                            # grid distance in km
                            numericInput("dist", "Grid distance: [km]", 250),

                            # Cluster generation
                            numericInput("clusterpoints", "Points per cluster", 4),

                            # Cluster configuration
                            selectInput(
                              "configuration",
                              "Cluster configuration",
                              c(
                                "Line" = "line",
                                "L-shape" = "lshape",
                                "Square" = "square"
                              )
                            ),

                            # point distance
                            sliderInput(
                              "pointdist",
                              "Point distance: [m]",
                              min = 0,
                              max = 500,
                              value = 100,
                              step = 10
                            ),

                            # Button "generate"
                            actionButton("reset_input", "Reset inputs"),
                            actionButton("go", "Generate")
                          )),

                   column(4,
                          wellPanel(
                            h4("Download GSG"),
                            textOutput("text1"),

                            radioButtons(
                              inputId = 'output',
                              label = 'Export options',
                              choices = c('Export sample points only' = 'shp', 'Export all subplots' = 'kml'),
                              inline = FALSE
                            ),

                            selectInput(
                              "format",
                              "Select output format:",
                              c("ESRI Shapefile" = "shp",
                                "KML" = "kml")
                            ),

                            # Button "download"
                            downloadButton("download", "Download")

                          ))
                 ))# ,
             #
             # plotOutput("se_plot", width = 450)
    ),



    tabPanel("Point list",
             DT::dataTableOutput('mytable'))
  )
)
