#' Summary plot
#' @param data Summary output of fCAT
#' @param plotHeight Summary output of fCAT
#' @return Barplot as ggplot object
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}

createSummaryPlotUI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                2,
                numericInput(
                    ns("summaryHeight"),
                    "Height(px)",
                    min = 100,
                    max = 3200,
                    step = 50,
                    value = 300,
                    width = 100
                )
            ),
            column(
                2,
                numericInput(
                    ns("summaryWidth"),
                    "Width(px)",
                    min = 100,
                    max = 3200,
                    step = 50,
                    value = 800,
                    width = 100
                )
            ),
            column(
                2,
                numericInput(
                    ns("summaryText"),
                    "Text(px)",
                    min = 3,
                    max = 99,
                    step = 1,
                    value = 11,
                    width = 100
                )
            ),
            column(
                2,
                numericInput(
                    ns("summaryValue"),
                    "Values(px)",
                    min = 3,
                    max = 99,
                    step = 1,
                    value = 3,
                    width = 100
                )
            )
        ),
        uiOutput(ns("summaryPlot.ui")),
        downloadButton(ns("downloadSummary"), "Download plot",
                       class = "butDL"),
        hr(),
        
        radioButtons(
            ns("tableType"), 
            "Type", 
            choices = c("Count", "Percentage"),
            selected = "Count", inline = TRUE
        ),
        tableOutput(ns("summary.table")),
        
        tags$head(
            tags$style(HTML(
                ".butDL{background-color:#476ba3;} .butDL{color: white;}"))
        )
    )
}

createSummaryPlot <- function(
    input, output, session, data
){

    # render detailed plot -----------------------------------------------------
    output$missingPlot <- renderPlotly({
        createMissingPlot(data(), input$summaryText, input$summaryValue)
    })
    
    output$foundPlot <- renderPlotly({
        createFoundPlot(data(), input$summaryText, input$summaryValue)
    })
    
    output$summary.table <- renderTable({
        df <- data()
        df$mode[df$mode == "mode_1"] <- "Strict"
        df$mode[df$mode == "mode_2"] <- "Reference"
        df$mode[df$mode == "mode_3"] <- "Relaxed"
        df$mode[df$mode == "mode_4"] <- "Length"
        df <- df[
            ,c(
                "genomeID", "mode", "found", "similar", "dissimilar", 
                "duplicated", "missing", "ignored", "total"
            )
        ]
        if (input$tableType == "Count") {
            return(df)
        } else {
            df$similar <- df$similar/df$found
            df$dissimilar <- df$dissimilar/df$found
            df$duplicated <- df$duplicated/df$found
            df$found <- df$found/df$total
            df$missing <- df$missing/df$total
            df$ignored <- df$ignored/df$total
            df$total <- 1
            return(df)
        }
        
    })

    output$summaryPlot.ui <- renderUI({
        ns <- session$ns
        tagList(
            plotlyOutput(
                ns("missingPlot"),
                width = input$summaryWidth,
                height = input$summaryHeight
            ),
            plotlyOutput(
                ns("foundPlot"),
                width = input$summaryWidth,
                height = input$summaryHeight
            )
        )
    })

    # output$downloadSummary <- downloadHandler(
    #     filename = function() {
    #         c("detailedPlot.pdf")
    #     },
    #     content = function(file) {
    #         g <- summaryPlot(data())
    #         ggsave(
    #             file,
    #             plot = g,
    #             width = 800 * 0.056458333,
    #             height = plotHeight * 0.056458333,
    #             units = "cm",
    #             dpi = 300,
    #             device = "pdf",
    #             limitsize = FALSE
    #         )
    #     }
    # )
}


#' create missing plot
#' @param df data for plotting 
#' @param textSize size of legend and axis
#' @param valueSize size of geom_text
#' @return found and missing percentage as a ggplotly bar plot
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}

createMissingPlot <- function (df = NULL, textSize = 11, valueSize = 3){
    if (is.null(df)) stop("No data for plotting!")
    missingDf <- df[,c("genomeID", "missing", "found")]
    missingDf <- missingDf[!duplicated(missingDf$genomeID),]
    missingDf <- melt(
        setDT(missingDf), id.vars = c("genomeID"), variable.name = "type"
    )
    sum <- aggregate(
        missingDf$value, by=list(Category=missingDf$genomeID), FUN=sum
    )[1,]$x

    missing_plot <- ggplot(missingDf, aes(fill=type, y=value, x=genomeID)) + 
        geom_col(position = "fill", width = 0.5) +
        geom_text(
            aes(label = value),
            position = position_fill(vjust = 0.5), size = valueSize
        ) +
        theme_minimal() +
        # scale_fill_brewer(palette="Set2") +
        labs(title = "Found and missing genes", y = "Percentage") +
        theme(
            legend.title = element_blank(), legend.position = "bottom",
            axis.title = element_blank(),
            axis.text = element_text(size = textSize, hjust = 1), 
            axis.text.x = element_text(angle = 0), 
            plot.title = element_text(size = textSize + 3)
        ) +
        coord_flip()
    return(
        ggplotly(missing_plot) 
        %>% layout(
            legend = list(orientation = "h", x = 0, y = -0.1)
        ) %>% reverse_legend_labels()
    )
}


#' create assessment plot
#' @param df data for plotting 
#' @param textSize size of legend and axis
#' @param valueSize size of geom_text
#' @return assessment as a ggplotly bar plot
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}

createFoundPlot <- function (df = NULL, textSize = 11, valueSize = 3){
    if (is.null(df)) stop("No data for plotting!")
    
    foundDf <- df[
        ,c("genomeID", "mode", "similar", "dissimilar", "duplicated")
    ]
    foundDf <- melt(
        setDT(foundDf), id.vars = c("genomeID", "mode"), variable.name = "type"
    )
    foundDf$type <- factor(
        foundDf$type, levels = c("duplicated", "dissimilar", "similar")
    )
    foundDf$modeName[foundDf$mode == "mode_1"] <- "Strict"
    foundDf$modeName[foundDf$mode == "mode_2"] <- "Reference"
    foundDf$modeName[foundDf$mode == "mode_3"] <- "Relaxed"
    foundDf$modeName[foundDf$mode == "mode_4"] <- "Length"
    foundDf$modeName <- factor(
        foundDf$modeName, levels = c("Strict", "Reference", "Relaxed", "Length")
    )

    found_plot <- ggplot(foundDf, aes(fill=type, y=value, x=genomeID)) + 
        geom_col(position = "fill", width = 0.5) +
        facet_grid(.~modeName) +
        theme_minimal() +
        scale_fill_brewer(palette="Set2") +
        labs(title = "Assessments of found genes") +
        theme(
            legend.title = element_blank(), legend.position = "bottom",
            axis.title = element_blank(),
            axis.text = element_text(size = textSize, hjust = 1), 
            axis.text.x = element_text(angle = 90), 
            plot.title = element_text(size = textSize + 3)
        ) + 
        guides(fill = guide_legend(reverse = TRUE)) +
        coord_flip()

    return(
        ggplotly(found_plot) 
        %>% layout(
            legend = list(orientation = "h", x = 0, y = -0.3)
        ) %>% reverse_legend_labels()
    )
}


