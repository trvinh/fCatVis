replaceHomeCharacter <- function (fullPath = NULL) {
    homeName <- system("echo $HOME", intern = TRUE)
    stringr::str_replace(fullPath, "~", homeName)
}

getFileName <- function (filePath = NULL) {
    if (is.null(filePath)) stop ("No complete path given!")
    tmp <- strsplit(filePath, "/")[[1]]
    return(tmp[length(tmp)])
}

substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft <- function(x, n) {
    substr(x, 1, n)
}

createPlotSize <- function(id, title, value) {
    numericInput(id,
                 title,
                 min = 100,
                 max = 3200,
                 step = 50,
                 value = value,
                 width = 100)
}

createTextSize <- function(id, title, value, width) {
    numericInput(id,
                 title,
                 min = 3,
                 max = 99,
                 step = 1,
                 value = value,
                 width = width)
}

createSliderCutoff <- function(id, title, start, stop, varID){
    if (is.null(varID)) return()
    if (varID == "") {
        sliderInput(id, title,
                    min = 1,
                    max = 1,
                    step = 0.025,
                    value = 1,
                    width = 200)
    } else {
        sliderInput(id, title,
                    min = 0,
                    max = 1,
                    step = 0.025,
                    value = c(start, stop),
                    width = 200)
    }
}

updateSliderCutoff <- function(session, id, title, newVar, varID){
    if (is.null(varID) || varID == "") return()
    updateSliderInput(session, id, title,
                      value = newVar,
                      min = 0,
                      max = 1,
                      step = 0.025)
}

reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
}


##### Processing functions #####

getSpecIDs <- function (inputType = NULL, fcatFile = NULL, fcatDir = NULL) {
    if (is.null(inputType)) stop("Input type undefined!")
    
    if (inputType == "File") {
        df <- read.csv(
            fcatFile, header = TRUE, sep = "\t", stringsAsFactors = FALSE
        )
        return(
            levels(
                as.factor(tstrsplit(df$genomeID, "@", fixed=TRUE)[[1]])
            )
        )
    } else if (inputType == "Folder") {
        files <- list.files(fcatDir, pattern = ".report_summary.txt")
        return(levels(as.factor(tstrsplit(files, ".", fixed=TRUE)[[1]])))
    }
}

# filter full reports to get only groups that have different assessments
# between query species
getDiffReport <- function (fullReportFile = NULL) {
    if (is.null(fullReportFile)) stop("No full report file given!")
    fullReport <- read.csv(fullReportFile, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    # remove missing groups
    missingIDs <- unique(fullReport[fullReport$mode_1 == "missing",]$groupID)
    fullReport <- fullReport[!(fullReport$groupID %in% missingIDs),]
    # split duplicated assessments to get only "duplicated" word
    fullReport$spec <- str_split_fixed(fullReport$orthoID, "\\|", 4)[,2]
    fullReport$mode1 <- str_split_fixed(fullReport$mode_1, " ", 2)[,1]
    fullReport$mode2 <- str_split_fixed(fullReport$mode_2, " ", 2)[,1]
    fullReport$mode3 <- str_split_fixed(fullReport$mode_3, " ", 2)[,1]
    fullReport$mode4 <- str_split_fixed(fullReport$mode_4, " ", 2)[,1]
    # join assessments from 4 modes
    fullReport$assessment <- paste(
        fullReport$mode1, fullReport$mode2, fullReport$mode3, fullReport$mode4,
        sep = "_"
    )
    # count number of joined assessments per group
    # if all species have the same joined assessments, the number of the same 
    # assessments per group will be equal to the number of query species
    countGroupDf <- fullReport %>% count(groupID, assessment)
    # get groups that have different assessments in query species
    diffGroups <- countGroupDf[countGroupDf$n < nlevels(as.factor(fullReport$spec)),]$groupID
    if (length(diffGroups) > 0) {
        # create dataframe for groups that have diff. assessments
        diffDf <- fullReport[
            fullReport$groupID %in% diffGroups, 
            c("groupID", "spec", "mode1", "mode2", "mode3", "mode4")
        ]
        return(diffDf[!duplicated(diffDf), ])
    } else return(NULL)
}

# compare the assessments and return the one that are different between species
compareAssessment <- function (
        modeDf = NULL, cutoff = 10, minDiffCount = 10, mode = NULL
) {
    if (is.null(modeDf)) stop("Input data is null!")
    if (is.null(mode)) stop("Mode name not specified!")
    
    modeDf$specName <- str_split_fixed(modeDf$spec, "@", 3)[,1]
    aggregate(
        modeDf$n, 
        by = list(spec = modeDf$spec), FUN = sum
    )
    maxDf <- aggregate(
        modeDf$n, 
        by = list(spec = modeDf$specName, type = modeDf[,2]), FUN = max
    )
    minDf <- aggregate(
        modeDf$n, 
        by = list(spec = modeDf$specName, type = modeDf[,2]), FUN = min
    )
    colnames(maxDf) <- c("spec", "type", "max")
    colnames(minDf) <- c("spec", "type", "min")
    commDf <- merge(maxDf, minDf, by = c("spec", "type"))
    commDf$diff <- 100 - round((commDf$min/commDf$max)*100, 0)
    commDf$diffCount <- abs(commDf$max  - commDf$min)
    commDf$mode <- mode
    return(
        commDf[
            commDf$diff >= cutoff & commDf$diffCount >= minDiffCount, 
            c("spec", "diff", "type", "mode")
        ]
    )
}

getDiffMissing <- function (
        summaryDf = NULL, cutoff = 10, minDiffCount = 10
) {
    if (is.null(summaryDf)) stop("Input df is null!")
    maxDf <- aggregate(
        summaryDf$missing,
        by = list(spec = summaryDf$spec, mode = summaryDf$mode), FUN = max
    )
    minDf <- aggregate(
        summaryDf$missing,
        by = list(spec = summaryDf$spec, mode = summaryDf$mode), FUN = min
    )
    colnames(maxDf) <- c("spec", "mode", "max")
    colnames(minDf) <- c("spec", "mode", "min")
    commDf <- merge(maxDf, minDf, by = c("spec", "mode"))
    commDf$diff <- 100 - round((commDf$min/commDf$max)*100, 0)
    commDf$diffCount <- abs(commDf$max  - commDf$min)
    filteredDf <- commDf[
        commDf$diff >= cutoff & commDf$diffCount >= minDiffCount,
        c("spec", "diff")
    ]
    finalDf <- aggregate(filteredDf$diff, by = list(filteredDf$spec), max)
    colnames(finalDf) <- c("spec", "diff")
    finalDf$type <- "missing"
    if (nrow(finalDf) > 0) {
        return(finalDf)
    } else (return(NULL))
}

# get species that have different assessments between different gene sets
# filterSpec <- function (inputFiles = NULL, cutoff = 10, minDiffCount = 10) {
#     if (is.null(inputFiles)) stop("No input files given!")
#     fullDf <- do.call(
#         rbind, lapply(
#             inputFiles, function(i){
#                 read.csv(i, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#             }
#         )
#     )
#     # fullDf$spec <- str_split_fixed(fullDf$genomeID, "@", 2)[,1]
#     missingDf <- getDiffAssessment(fullDf, "missing", cutoff, minDiffCount)
#     # similarDf <- getDiffAssessment(fullDf, "similar", cutoff, minDiffCount)
#     # dissimilarDf <- getDiffAssessment(
#     #     fullDf, "dissimilar", cutoff, minDiffCount
#     # )
#     # duplicatedDf <- getDiffAssessment(
#     #     fullDf, "duplicated", cutoff, minDiffCount
#     # )
#     # filteredDf <- rbind(
#     #     similarDf, dissimilarDf, duplicatedDf, missingDf
#     # )
#     
#     return(filteredDf)
# }


getReportDf <- function (reportSummary = NULL, specID = NULL) {
    if (is.null(reportSummary)) stop("No infile given!")
    if (is.null(specID)) stop("Species not specified!")
    df <- read.csv(
        reportSummary, header = TRUE, sep = "\t", stringsAsFactors = FALSE
    )
    return(df[grep(paste0("^", specID), df$genomeID),])
}


# getGroupsByAssessment <- function (
#         reportFull = NULL, assessments = NULL, mode = NULL
# ) {
#     if (is.null(reportFull)) stop("No infile given!")
#     if (is.null(assessments)) stop("Assessments not specified!")
#     if (is.null(mode)) stop("Assessment mode not specified!")
#     df <- read.csv(
#         reportFull, header = TRUE, sep = "\t", stringsAsFactors = FALSE
#     )
#     if ("duplicated" %in% assessments) {
#         assessments <- c(
#             assessments, "duplicated (similar)", "duplicated (dissimilar)",
#             "duplicated (complete)", "duplicated (fragmented)"
#         )
#     }
#     assessments <- c("missing", "duplicated")
#     selectedDf <- df[, c("groupID", mode)] %>% 
#         filter_all(any_vars(. %in% assessments))
#     return(levels(as.factor(selectedDf$groupID)))
# }


getProfileDf <- function (profileFile = NULL, geneIDs = NULL) {
    if (is.null(profileFile)) stop("No infile given!")
    if (is.null(geneIDs)) stop("No gene ID list specified!")
    
    df <- read.csv(
        profileFile, header = TRUE, sep = "\t", stringsAsFactors = FALSE
    )
    return(df[df$geneID %in% geneIDs,])
}


getDomainDf <- function (domainFile = NULL, geneIDs = NULL) {
    if (is.null(domainFile)) stop("No infile given!")
    if (is.null(geneIDs)) stop("No gene ID list specified!")
    
    domainDf <- PhyloProfile::parseDomainInput(NULL, domainFile, "file")
    domainDf[c("geneID", "refID")] <- str_split_fixed(domainDf$seedID, '#', 2)
    selectedDf <- domainDf[
        domainDf$geneID %in% geneIDs, 
        !(names(domainDf) %in% c("geneID", "refID"))
    ]
    return(selectedDf)
    
}