options(shiny.error = browser)

#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#'@examples
#' # Start shiny app using launch_app() function
#' launch_app()
shiny_app_server <- shiny::shinyServer(function(input, output, session) {
  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles() %>%

      leaflet::addScaleBar(position = "bottomleft") %>%

      leaflet::addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit = "hectares") %>%
      leaflet::addMiniMap() %>%
      leaflet::setView(lng = 0, lat = 20, zoom = 3) %>%
      leaflet::addWMSTiles("http://globalforestwatch-624153201.us-west-1.elb.amazonaws.com:80/arcgis/services/TreeCover2000/ImageServer/WMSServer",
                           layers = "0",
                           options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
                           attribution = 'Hansen/UMD/Google/USGS/NASA, accessed through Global Forest Watch') #%>%
    #addWMSLegend("http://globalforestwatch-624153201.us-west-1.elb.amazonaws.com:80/arcgis/services/TreeCover2000/ImageServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=0",
    #position = 'bottomright')
  })

  shiny::observeEvent(input$reset_input, {
    shinyjs::reset("settings")
  })

  shiny::observeEvent(# Take a dependency on input$goButton
    input$go, {

      progress <- shiny::Progress$new(session, min = 1, max = 4)
      on.exit(progress$close())

      progress$set(value = 1,
                   message = 'Calculation in progress',
                   detail = 'Downloading boundary...')

      # load selected aoi as boundary
      in_bnd <- input$aoi
      if (is.null(in_bnd)) {
        getaoi <- NULL
      } else {
        switch(input$inputformat,
               'kml' = {
                 dir <- dirname(in_bnd[1, 4])
                 file.rename(in_bnd[1, 4], paste0(dir, "/", in_bnd[1, 1]))
                 getaoi <- list.files(dir, pattern = "*.kml", full.names = TRUE)
               },

               'shp' = {
                 dir <- dirname(in_bnd[1, 4])
                 for (i in 1:nrow(in_bnd)) {
                   file.rename(in_bnd[i, 4], paste0(dir, "/", in_bnd[i, 1]))
                 }
                 getaoi <- list.files(dir, pattern = "*.shp", full.names = TRUE)
               })
      }

      bnd <- shiny::isolate(gsg::load_boundary(
        x = getaoi,
        country_code = input$country_code,
        adm_level = 0
      ))

      # Increment the progress bar, and update the detail text.
      progress$set(value = 2,
                   message = 'Calculation in progress',
                   detail = 'Initializing GSG generation...')

      # generate GSG based on inputs from ui.R
      # isolate () to avoid dependency on input$dist (but reactive on button)
      gsg <- shiny::isolate(gsg::gen_gsg(input$dist, bnd))

      # define cluster configuration based on inputs from ui.R



      # generate GSG subplots based on inputs from ui.R
      gsg_clus <- shiny::isolate(gsg::plot_design(gsg,layout, dist=input$pointdist))

      # Show resulting sample size
      output$text1 = shiny::renderText({
        paste0("Number of generated points: ", length(gsg))
      })

      if (length(gsg) == 1)
      {
        shiny::showModal(
          shiny::modalDialog(
            title = "Warning",
            "No sample locations in the area of interest! Adjust grid distance!",
            easyClose = FALSE
          )
        )

      } else {
        center = rgeos::gCentroid(gsg)

        # Increment the progress bar, and update the detail text.
        progress$set(value = 3,
                     message = 'Calculation in progress',
                     detail = 'Gerenating outputs...')

        # # Plot se_plot
        # output$se_plot <- shiny::renderPlot({
        #   se_binomial <- function(p) {
        #     se <- 100*sqrt((p*(1 - p))/(length(gsg) - 1))/p
        #     return(se)
        #   }
        #
        #   curve(se_binomial,
        #         from = 0.001,
        #         to = 1,
        #         main = "Expected standard errors for grid points (no clusters)",
        #         xlab = "Expected area proportion of target class",
        #         #log = "x",
        #         ylab = "Relative standard error (%)"
        #   )
        # })

        ## Interactive Map ###########################################

        # Create the map
        leaflet::leafletProxy("map", data = gsg) %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(
            data = bnd,
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0.3,
            fillOpacity = 0.5) %>%
          leaflet::addCircles(
            data = gsg,
            weight = 3,
            radius = 40,
            color = "#FFFFFF",
            stroke = TRUE,
            fillOpacity = 0.9
          ) %>%
          leaflet::setView(center$x, center$y, zoom = 5)

        shiny::updateNavbarPage(session, "nav", selected = "gomap")

        # Generate Data Table
        output$mytable <- DT::renderDataTable(DT::datatable(as.data.frame(gsg), options = list(pageLength = 25)))

        # Increment the progress bar, and update the detail text.
        progress$set(value = 4,
                     message = 'Calculation in progress',
                     detail = 'Plotting the GSG...')

        ### Export GSG ###
        shiny::observeEvent(input$format,
                            if (input$format == "shp") {
                              output$download <- shiny::downloadHandler(
                                filename = 'GSGExport.zip',
                                content = function(file) {
                                  if (length(Sys.glob("GSGExport.*")) > 0) {
                                    file.remove(Sys.glob("GSGExport.*"))
                                  }
                                  rgdal::writeOGR(gsg,
                                                  dsn = "GSGExport.shp",
                                                  layer = "GSGExport",
                                                  driver = "ESRI Shapefile")
                                  zip(zipfile = 'GSGExport.zip', files = Sys.glob("GSGExport.*"))
                                  file.copy("GSGExport.zip", file)
                                  if (length(Sys.glob("GSGExport.*")) > 0) {
                                    file.remove(Sys.glob("GSGExport.*"))
                                  }
                                }
                              )
                            }
                            else if (input$format == "kml") {
                              output$download <- shiny::downloadHandler(
                                filename = function() {
                                  paste0("GSG", input$dist, ".kml")
                                },
                                content = function(file) {
                                  rgdal::writeOGR(gsg,
                                                  file,
                                                  layer = paste0("GSG", input$dist, "_KML"),
                                                  driver = "KML")
                                }
                              )
                            })
      }
    })
})