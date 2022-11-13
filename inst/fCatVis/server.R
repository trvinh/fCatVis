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
                    "diffCutoffPercent",
                    "Difference threshold (%)",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 10
                ),
                numericInput(
                    "diffCutoff",
                    "Min difference count",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 10
                )
            ),
            br()
        )
    })
    
    # render species IDs =======================================================
    filteredSpecDf <- reactive({
        req(input$filter)
        req(getFcatDir())
        req(input$diffCutoffPercent)
        files <- list.files(getFcatDir(), pattern = ".report_summary.txt")
        fullFiles <- paste0(getFcatDir(), "/", files)
        return(filterSpec(fullFiles, input$diffCutoffPercent, input$diffCutoff))
    })
    
    output$specID.ui <- renderUI({
        req(input$filter)
        if (input$inputType == "File") req(getFcatFile())
        if (input$inputType == "Folder") req(getFcatDir())
        
        specIDs <- NULL
        if (input$filter == "No") {
            specIDs <- getSpecIDs(input$inputType, getFcatFile(), getFcatDir())
        } else {
            filteredSpec <- filteredSpecDf()$spec
            specIDs <- sort(filteredSpec[!duplicated(filteredSpec)])
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
            allFiles <- list.files(getFcatDir(), pattern=".report_summary.txt")
            fcatFile <- paste0(
                getFcatDir(), "/", 
                allFiles[grep(paste0("^", input$specID), allFiles)]
            )
            outDf <- getReportDf(fcatFile, input$specID)
        }
        outDf$found <- outDf$similar + outDf$dissimilar + outDf$duplicated
        return(outDf)
    })
    
    diffDf <- reactive({
        req(input$doPlot)
        req(input$specID)
        filteredSpecDf <- filteredSpecDf()
        return(filteredSpecDf[filteredSpecDf$spec == input$specID,])
    })
    
    callModule(
        createSummaryPlot, "summaryPlot",
        data = reportDf,
        diffDf = diffDf
    )
    
    # PhyloProfile plot ########################################################
    pathToPhyloprofile <- paste0(
        path.package("PhyloProfile"), "/PhyloProfile"
    )
    nameFullFile <- paste0(
        pathToPhyloprofile, "/data/preProcessedTaxonomy.txt"
    )
    nameFullDf <- data.table::fread(nameFullFile, select = c(1:3))
    
    profileData <- eventReactive(input$doPP, {
        req(input$doPlot)
        req(input$specID)
        
        assessmentDf <- diffDf()
        ppDf <- list()
        geneIDs <- NULL
        if (nrow(assessmentDf) > 0) {
            ### get genes based on assessments
            allReportFiles <- list.files(
                getFcatDir(), pattern=".report_full.txt"
            )
            reportFile <- paste0(
                getFcatDir(), "/", 
                allReportFiles[grep(paste0("^", input$specID), allReportFiles)]
            )
            geneIDs_mode1 <- getGroupsByAssessment(
                reportFile, levels(as.factor(assessmentDf$type)), "mode_1"
            )
            geneIDs_mode2 <- getGroupsByAssessment(
                reportFile, levels(as.factor(assessmentDf$type)), "mode_2"
            )
            geneIDs_mode3 <- getGroupsByAssessment(
                reportFile, levels(as.factor(assessmentDf$type)), "mode_3"
            )
            geneIDs <- c(geneIDs_mode1, geneIDs_mode2, geneIDs_mode3)
            
            if (length(geneIDs) > 0) {
                ### get profile data
                allPPFiles <- list.files(getFcatDir(), pattern=".phyloprofile")
                profileFile_mode1 <- paste0(
                    getFcatDir(), "/", 
                    allPPFiles[
                        grep(paste0("^", input$specID, "_mode1"), allPPFiles)
                    ]
                )
                ppDf_mode1 <- getProfileDf(profileFile_mode1, geneIDs_mode1)
                profileFile_mode2 <- paste0(
                    getFcatDir(), "/", 
                    allPPFiles[
                        grep(paste0("^", input$specID, "_mode2"), allPPFiles)
                    ]
                )
                ppDf_mode2 <- getProfileDf(profileFile_mode2, geneIDs_mode2)
                profileFile_mode3 <- paste0(
                    getFcatDir(), "/", 
                    allPPFiles[
                        grep(paste0("^", input$specID, "_mode3"), allPPFiles)
                    ]
                )
                ppDf_mode3 <- getProfileDf(profileFile_mode3, geneIDs_mode3)
                ppDf <- list(ppDf_mode1, ppDf_mode2, ppDf_mode3)
            }
        }
        
        if (length(ppDf) == 0) {
            return(NULL)
        } else {
            ### get domain data
            allDomainFiles <- list.files(
                getFcatDir(), pattern=".domains"
            )
            domainFile <- paste0(
                getFcatDir(), "/", 
                allDomainFiles[grep(paste0("^", input$specID), allDomainFiles)]
            )
            domainDf <- getDomainDf(domainFile, geneIDs)
            return(list(ppDf, domainDf))
        }
    })
    
    output$test.ui <- renderUI({
        ll <- profileData()
        return(ll[2])
    })
    
    callModule(phyloprofileLite, "phyloprofileLite", profileData)
})