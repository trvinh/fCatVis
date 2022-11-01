#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2, # size limit for input 9999mb
    scipen = 999 # disabling scientific notation
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    homePath = c(wd='~/') # for shinyFileChoose
    
    # input file ===============================================================
    getFcatFile <- reactive({
        shinyFileChoose(
            input, "fcatFile", roots = homePath, session = session,
            filetypes = c('', 'txt')
        )
        fileSelected <- parseFilePaths(homePath, input$fcatFile)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$fcatFile.ui <- renderUI({
        req(getFcatFile())
        if (length(getFcatFile()) > 0) {
            outString <- getFcatFile()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # input folder =============================================================
    getFcatDir <- reactive({
        shinyDirChoose(
            input, "fcatDir", roots = homePath, session = session
        )
        fcatPath <- parseDirPath(homePath, input$fcatDir)
        return(replaceHomeCharacter(as.character(fcatPath)))
    })
    output$fcatDir.ui <- renderUI({
        req(getFcatDir())
        if (length(getFcatDir()) > 0) {
            outString <- getFcatDir()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # render filter option ==================================================
    output$filter.ui <- renderUI({
        if (input$inputType == "File") req(getFcatFile())
        if (input$inputType == "Folder") req(getFcatDir())
        tagList(
            radioButtons(
                "filter",
                "Show only different genomes",
                choices = c("Yes", "No"),
                selected = "No",
                inline = TRUE
            ),
            conditionalPanel(
                condition = "input.filter == 'Yes'",
                numericInput(
                    "diffCutoff",
                    "Difference threshold (%)",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 10
                )
            ),
            br()
        )
    })
    
    # render species IDs ==================================================
    output$specID.ui <- renderUI({
        req(input$filter)
        if (input$inputType == "File") req(getFcatFile())
        if (input$inputType == "Folder") req(getFcatDir())
        
        specIDs <- NULL
        if (input$filter == "No") {
            specIDs <- getSpecIDs(input$inputType, getFcatFile(), getFcatDir())
        } else {
            files <- list.files(getFcatDir(), pattern = ".report_summary.txt")
            fullFiles <- paste0(getFcatDir(), "/", files)
            specIDs <- filterSpec(fullFiles, input$diffCutoff)
        }
        
        selectInput(
            "specID",
            paste0("Species ID (", length(specIDs), " in total)"),
            choices = specIDs
        )
    })
    
    # summary plot =============================================================
    reportDf <- reactive({
        req(input$doPlot)
        req(input$specID)
        outDf <- NULL
        if (input$inputType == "File") {
            outDf <- getReportDf(getFcatFile(), input$specID)
        } else if (input$inputType == "Folder") {
            allFiles <- list.files(getFcatDir(), pattern = ".txt")
            fcatFile <- paste0(
                getFcatDir(), "/", 
                allFiles[grep(paste0("^", input$specID), allFiles)]
            )
            outDf <- getReportDf(fcatFile, input$specID)
        }
        outDf$found <- outDf$similar + outDf$dissimilar + outDf$duplicated
        return(outDf)
    })
    
    callModule(
        createSummaryPlot, "summaryPlot",
        data = reportDf
    )
})