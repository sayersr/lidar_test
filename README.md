# LiDAR 3D Point Cloud Viewer and DEM Generator

This Shiny app allows users to upload LAS/LAZ files, visualize 3D point clouds, generate DEMs, and download the results.

## Requirements

This app requires R and the following R packages:
- shiny
- plotly
- lidR
- leaflet
- raster
- viridis
- sf

## Running the app

1. Clone this repository
2. Open R or RStudio
3. Set your working directory to the app folder
4. Run the following commands:

   ```R
   library(shiny)
   runApp()