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


getDiffAssessment <- function (fullDf = NULL, type = "similar", cutoff = 10) {
    if (is.null(fullDf)) stop("Input df is null!")
    if (type == "similar") {
        maxDf <- aggregate(
            fullDf$similar, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = max
        )
        minDf <- aggregate(
            fullDf$similar, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = min
        )
    } else if (type == "dissimilar") {
        maxDf <- aggregate(
            fullDf$dissimilar, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = max
        )
        minDf <- aggregate(
            fullDf$dissimilar, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = min
        )
    } else if (type == "missing") {
        maxDf <- aggregate(
            fullDf$missing, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = max
        )
        minDf <- aggregate(
            fullDf$missing, 
            by = list(spec = fullDf$spec, mode = fullDf$mode), FUN = min
        )
    }
    colnames(maxDf) <- c("spec", "mode", "max")
    colnames(minDf) <- c("spec", "mode", "min")
    similarDf <- merge(maxDf, minDf, by = c("spec", "mode"))
    similarDf$diff <- 100 - round((similarDf$min/similarDf$max)*100, 0)
    filteredSimilar <- similarDf[similarDf$diff >= cutoff,]$spec
    filteredSimilar <- filteredSimilar[!duplicated(filteredSimilar)]
    if (length(filteredSimilar) > 0) {
        return(filteredSimilar[!is.na(filteredSimilar)])
    } else (return(NULL))
}

filterSpec <- function (inputFiles = NULL, cutoff = 10) {
    if (is.null(inputFiles)) stop("No input files given!")
    fullDf <- do.call(
        rbind, lapply(
            inputFiles, function(i){
                read.csv(i, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
            }
        )
    )
    fullDf$spec <- str_split_fixed(fullDf$genomeID, "@", 2)[,1]
    missingIds <- getDiffAssessment(fullDf, "missing", cutoff)
    similarIds <- getDiffAssessment(fullDf, "similar", cutoff)
    dissimilarIds <- getDiffAssessment(fullDf, "dissimilar", cutoff)
    filteredIds <- c(missingIds, similarIds, dissimilarIds)
    return(sort(filteredIds[!duplicated(filteredIds)]))
}


getReportDf <- function (inputFile = NULL, specID = NULL) {
    if (is.null(inputFile)) stop("No infile given!")
    if (is.null(specID)) stop("Species not specified!")
    
    df <- read.csv(
        inputFile, header = TRUE, sep = "\t", stringsAsFactors = FALSE
    )
    return(df[grep(specID, df$genomeID),])
}






