library(shiny)
library(plotly)
library(lidR)
library(leaflet)
library(raster)
library(viridis)
library(sf)

# Increase the maximum file upload size to 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Function to generate sample LiDAR data when no file is uploaded
generate_sample_data <- function(n_points = 5000) {
  set.seed(123)
  data.frame(
    X = rnorm(n_points, mean = 0, sd = 10),
    Y = rnorm(n_points, mean = 0, sd = 10),
    Z = abs(rnorm(n_points, mean = 5, sd = 2)),
    Intensity = runif(n_points, 0, 1)
  )
}

ui <- fluidPage(
  titlePanel("LiDAR 3D Point Cloud Viewer and DEM Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("las_file", "Upload LAS File (Max 500 MB)",
                accept = c(".las", ".laz")),
      sliderInput("point_size", "Point Size", min = 1, max = 10, value = 3),
      sliderInput("opacity", "Opacity", min = 0.1, max = 1, value = 0.7, step = 0.1),
      checkboxInput("color_by_height", "Color by Height", value = TRUE),
      numericInput("max_points", "Max Points to Display", value = 100000, min = 1000, max = 1000000),
      numericInput("dem_resolution", "DEM Resolution (m)", value = 10, min = 1, max = 50),
      downloadButton("download_dem", "Download DEM (GeoTIFF)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("3D Point Cloud", plotlyOutput("point_cloud", height = "600px")),
        tabPanel("DEM Visualization", plotOutput("dem_plot", height = "600px"))
      ),
      textOutput("processing_status"),
      verbatimTextOutput("error_message"),
      verbatimTextOutput("crs_info")
    )
  )
)

server <- function(input, output, session) {
  lidar_data <- reactive({
    req(input$las_file)
    
    tryCatch({
      withProgress(message = 'Processing LiDAR data...', value = 0, {
        # Read LAS file
        incProgress(0.3, detail = "Reading LAS file")
        las <- readLAS(input$las_file$datapath)
        
        # Extract points
        incProgress(0.3, detail = "Extracting point data")
        data <- data.frame(
          X = las@data$X,
          Y = las@data$Y,
          Z = las@data$Z,
          Intensity = las@data$Intensity
        )
        
        # Subsample if necessary
        if (nrow(data) > input$max_points) {
          data <- data[sample(nrow(data), input$max_points), ]
        }
        
        # Get CRS information
        crs <- st_crs(las)
        
        incProgress(0.4, detail = "Finalizing")
        return(list(data = data, las = las, crs = crs))
      })
    }, error = function(e) {
      # If an error occurs, return NULL and store the error message
      output$error_message <- renderText(paste("Error:", e$message))
      return(NULL)
    })
  })
  
  dem <- reactive({
    req(lidar_data())
    las <- lidar_data()$las
    
    # Create DEM
    rasterize_terrain(las, res = input$dem_resolution, algorithm = tin())
  })
  
  output$processing_status <- renderText({
    if (is.null(input$las_file)) {
      "Upload a LAS file to begin processing."
    } else if (is.null(lidar_data())) {
      "Processing LiDAR data..."
    } else {
      paste("LiDAR data processed and ready for visualization. Displaying", nrow(lidar_data()$data), "points.")
    }
  })
  
  output$crs_info <- renderPrint({
    req(lidar_data())
    cat("Coordinate Reference System (CRS) of the LiDAR data:\n")
    print(lidar_data()$crs)
  })
  
  output$point_cloud <- renderPlotly({
    # Use sample data if no file is uploaded
    data <- if (is.null(input$las_file)) generate_sample_data() else lidar_data()$data
    
    if (is.null(data)) {
      return(NULL)  # Don't render anything if there's an error
    }
    
    color_var <- if(input$color_by_height) "Z" else "Intensity"
    
    plot_ly(data = data, 
            x = ~X, y = ~Y, z = ~Z, 
            type = "scatter3d", 
            mode = "markers",
            marker = list(
              size = input$point_size,
              opacity = input$opacity,
              color = data[[color_var]],
              colorscale = "Viridis",
              showscale = TRUE
            )) %>%
      layout(scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Z")
      ))
  })
  
  output$dem_plot <- renderPlot({
    req(dem())
    plot(dem(), col = viridis(100), main = "Digital Elevation Model")
  })
  
  output$download_dem <- downloadHandler(
    filename = function() {
      paste("dem_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif", sep = "")
    },
    content = function(file) {
      writeRaster(dem(), file, format = "GTiff", overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
