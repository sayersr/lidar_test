# List of required packages
packages <- c("shiny", "plotly", "lidR", "leaflet", "raster", "viridis", "sf")

# Function to check and install packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
sapply(packages, install_if_missing)

# Print a message when done
cat("All required packages have been checked and installed if necessary.\n")
