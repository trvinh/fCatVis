shinyUI(
    fluidPage(
        
        tags$style(type = "text/css", "body {padding-top: 80px;}"),
        useShinyjs(),
        
        # Application title
        titlePanel("", windowTitle = "fCatVis"),
        navbarPage(
            em(strong("fCatVis v0.0.1")),
            id = "tabs",
            collapsible = TRUE,
            inverse = TRUE,
            fluid = TRUE,
            position = "fixed-top",
            tabPanel(
                "Playground",
                sidebarPanel(
                    width = 3,
                    selectInput(
                        "inputType", "Input type",
                        c(
                            "fCAT file" = "File",
                            "fCAT folder" = "Folder"
                        ),
                        selected = "Folder",
                        width = 150
                    ),
                    conditionalPanel(
                        condition = "input.inputType == 'File'",
                        shinyFilesButton(
                            "fcatFile", "Input file" ,
                            title = "Please provide fCAT output file:",
                            multiple = FALSE,
                            buttonType = "default", class = NULL
                        ),
                        uiOutput("fcatFile.ui")
                    ),
                    conditionalPanel(
                        condition = "input.inputType == 'Folder'",
                        shinyDirButton(
                            "fcatDir", "Input directory" ,
                            title = "Please select a folder",
                            buttonType = "default", class = NULL
                        ),
                        uiOutput("fcatDir.ui"),
                        br()
                    ),
                    br(),
                    uiOutput("filter.ui"),
                    uiOutput("specID.ui"),
                    bsButton("doPlot", "Plot", style = "info")
                ),
                mainPanel(
                    # fluidRow(
                    #     column(
                    #         2,
                    #         createPlotSize("archiHeight", "Plot height(px)",400)
                    #     ),
                    #     column(
                    #         2,
                    #         createPlotSize("archiWidth", "Plot width(px)", 800)
                    #     ),
                    #     column(
                    #         2,
                    #         createTextSize(
                    #             "titleArchiSize", "Title size(px)", 14, 100
                    #         )
                    #     ),
                    #     column(
                    #         2,
                    #         createTextSize(
                    #             "labelArchiSize", "DomainID size(px)", 12, 150
                    #         )
                    #     )
                    # ),
                    # uiOutput("domainPlot.ui"),
                    createSummaryPlotUI("summaryPlot")
                    # downloadButton("archiDownload", "Download plot", class = "butDL"),
                    # hr(),
                    # tableOutput("domainTable")
                )
            )
        )
    )
)