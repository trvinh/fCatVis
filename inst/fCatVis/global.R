source("R/functions.R")

# List of dependent packages --------------------------------------------------
packages <- c(
    "data.table", "dplyr", "ggplot2", "grid", "gridExtra",
    "shiny", "shinyBS", "shinyFiles", "shinyjs", "shinyalert", "shinythemes",
    "stringr", "plotly", "PhyloProfile"
)

# Load packages
lapply(packages, library, character.only = TRUE)