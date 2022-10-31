source("R/functions.R")

# List of dependent packages --------------------------------------------------
packages <- c(
    "data.table", "ggplot2", "gridExtra",
    "shiny", "shinyBS", "shinyFiles", "shinyjs", "shinyalert", "shinythemes",
    "stringr", "plotly"
)

# Load packages
lapply(packages, library, character.only = TRUE)