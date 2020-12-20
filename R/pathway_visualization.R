#### Part 1 ####
# # Load data to play with
#load("~/Desktop/testdata/R data/huntigton-ines-batch-local.Rdata")

#### Shiny ####
#TODO write documentation
#' Title
#'
#' @param result
#'
#' @return
#' @export
#'
#' @examples
#' @import shiny
#' @import visNetwork
visualize_result <- function(result) {
  server <- function(input, output) {
    # Determine configuration and pathway
    output$configuration <- renderText({
      paste("Your configuration:", input$configuration)
    })
    output$pathway <- renderText({
      paste("Your pathway:", input$pathway)
    })

    # Display network for specific configuration and pathway
    output$network <- renderVisNetwork({
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      edges <- data.frame(from = pathway@edges$source, to = pathway@edges$target)
      nodes <- data.frame(id = unlist(pathway@nodes), label = unlist(pathway@nodes), title = paste('<a target="_blank" href = "https://www.ncbi.nlm.nih.gov/gene/?term=', unlist(pathway@nodes), '">Gene id: ',unlist(pathway@nodes) ,' (Visit NCBI)</a>', sep = ""))
      visNetwork(submain = paste("Configuration: ", input$configuration), main = input$pathway, nodes, edges, footer = "Tipp: Zoom in to be able to see the individual gene id's") %>%
        visIgraphLayout() %>%
        visExport(type = "jpeg") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    })


    # Display pathway statistics
    output$nodes <- renderText({
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      paste("Number of nodes: ", pathway@num_nodes)
    })
    output$edges <- renderText({
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      paste("Number of edges: ", pathway@num_edges)
    })
    output$avg_exp <- renderText({
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      paste("Average expression: ", pathway@avg_exp)
    })
    output$info_content <- renderText({
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      paste("Average expression: ", pathway@info_content)
    })

    # Functionality of export edges button
    observeEvent(input$export_edges, {
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      path <- paste(easycsv::choose_dir(), tolower(input$configuration), tolower(input$pathway), "-edges", sep = "")
      export_edges(file = path, pathway_object = pathway, format = "sif")
      output$file <- renderText({
        paste("Edges saved in:", path)
      })
    })


    # Functionality of export nodes button
    observeEvent(input$export_nodes, {
      path <- easycsv::choose_dir()
      pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
      export_nodes(file = paste(path, tolower(input$configuration), "-", tolower(input$pathway), "-nodes", sep = ""), pathway_object = pathway)
      output$file <- renderText({
        paste("Nodes saved in:", path)
      })
    })
  }

  #### UI ####
  ui <- shinyUI(navbarPage(
    title = "KPM - Result",
    tabPanel(
      title = "Pathways",
      fluidPage(

        # App title ----
        titlePanel("Select configuration and pathway"),

        # Sidebar layout with a input and output definitions ----
        sidebarLayout(
          sidebarPanel(
            width = 3,

            # Input: Selector for choosing dataset ----
            selectInput(
              inputId = "configuration",
              label = "Choose a configuration:",
              choices = get_configurations(result_object = result)
            ),

            selectInput(
              inputId = "pathway",
              label = "Choose a pathway:",
              choices = sprintf("Pathway-%s", seq(1:result@parameters$computed_pathways))
            ),
            h4("Export:"),
            actionButton("export_edges", "Export edges (SIF)"),
            actionButton("export_nodes", "Export nodes"),
            tableOutput("file")
          ),

          # Main panel for displaying outputs ----
          mainPanel(
            width = 9,

            visNetworkOutput("network", height = "500px"),
            fluidRow(
              h4("Pathway statistics:"),
              column(
                width = 3,
                verbatimTextOutput("nodes"),
                verbatimTextOutput("edges")
              ),
              column(
                width = 3,
                verbatimTextOutput("avg_exp"),
                verbatimTextOutput("info_content")
              )
            )
          )
        )
      )
    ),
    tabPanel("Union networks"),
    tabPanel("Parameters")
  ))

  shinyApp(ui = ui, server = server)
}
