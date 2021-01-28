#' Shiny App for KeyPathwayMineR results
#'
#' App for visualization and extraction of
#' KPM results.
#'
#' @param result Result object obtained from a KeyPathwayMiner execution.
#'
#' @export
#'
#' @import shiny
#' @import visNetwork
visualize_result <- function(result) {
  if (length(result@configurations) != 0) {
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
        nodes <- data.frame(id = unlist(pathway@nodes), label = unlist(pathway@nodes), title = paste('<a target="_blank" href = "https://www.ncbi.nlm.nih.gov/gene/?term=', unlist(pathway@nodes), '">Gene id: ', unlist(pathway@nodes), " (Visit NCBI)</a>", sep = ""))

        if (nrow(edges) == 0 & nrow(nodes) > 0) {
          # In cases no edges exist but notes were extracted
          visNetwork(submain = paste("Configuration: ", input$configuration), main = input$pathway, nodes, edges, footer = "Tipp: Zoom in to see the individual gene id's") %>%
            visExport(type = "jpeg") %>%
            visOptions(nodesIdSelection = TRUE)
        } else if (length(edges) > 0) {
          visNetwork(submain = paste("Configuration: ", input$configuration), main = input$pathway, nodes, edges, footer = "Tipp: Zoom in to see the individual gene id's") %>%
            visIgraphLayout() %>%
            visExport(type = "jpeg") %>%
            visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
        }
      })

      # Display union networks for specific configuration
      output$union_network <- renderVisNetwork({
        union_network <- result@configurations[[input$configuration_union]]@union_network
        nodes <- data.frame(id = unlist(union_network@nodes$node), label = unlist(union_network@nodes$node), group = unlist(union_network@nodes$group), title = paste('<a target="_blank" href = "https://www.ncbi.nlm.nih.gov/gene/?term=', unlist(union_network@nodes$node), '">Gene id: ', unlist(union_network@nodes$node), " (Visit NCBI)</a>", sep = ""))
        edges <- data.frame(from = union_network@edges$source, to = union_network@edges$target)

        visNetwork(main = paste("Configuration: ", input$configuration_union), nodes, edges, footer = "Tipp: Zoom in to see the individual gene id's") %>%
          visOptions(selectedBy = list(variable = "group", multiple = TRUE, sort = FALSE)) %>%
          visGroups(useDefaultGroups = TRUE) %>%
          visExport(type = "jpeg") %>%
          visIgraphLayout()
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
        paste("Avg. DE cases per gene: ", pathway@avg_exp)
      })

      output$union_nodes <- renderText({
        union_network <- result@configurations[[input$configuration_union]]@union_network
        paste("Number of nodes: ", union_network@num_nodes)
      })
      output$union_edges <- renderText({
        union_network <- result@configurations[[input$configuration_union]]@union_network
        paste("Number of edges: ", union_network@num_edges)
      })
      output$union_avg_exp <- renderText({
        union_network <- result@configurations[[input$configuration_union]]@union_network
        paste("Avg. DE cases per gene: ", union_network@avg_exp)
      })

      # Functionality of export edges button
      observeEvent(input$export_edges, {
        path <- easycsv::choose_dir()
        pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
        export_edges(file = paste(path, tolower(input$configuration), tolower(input$pathway), "-edges.sif", sep = ""), pathway_object = pathway, format = "sif")
        output$file <- renderText({
          paste("Edges saved in:", path)
        })
      })


      # Functionality of export nodes button
      observeEvent(input$export_nodes, {
        path <- easycsv::choose_dir()
        pathway <- result@configurations[[input$configuration]]@pathways[[input$pathway]]
        export_nodes(file = paste(path, tolower(input$configuration), "-", tolower(input$pathway), "-nodes.txt", sep = ""), pathway_object = pathway)
        output$file <- renderText({
          paste("Nodes saved in:", path)
        })
      })



      # Functionality of export edges button
      observeEvent(input$export_edges_union, {
        path <- easycsv::choose_dir()
        union_network <- result@configurations[[input$configuration_union]]@union_network
        export_edges(file = paste(path, "union-", tolower(input$configuration_union), "-edges.sif", sep = ""), pathway_object = union_network, format = "sif")
        output$file_union <- renderText({
          paste("Edges saved in:", path)
        })
      })


      # Functionality of export nodes button
      observeEvent(input$export_nodes_union, {
        path <- easycsv::choose_dir()
        union_network <- result@configurations[[input$configuration_union]]@union_network
        union_network@nodes[["group"]] <- NULL
        export_nodes(file = paste(path, "union-", tolower(input$configuration_union), "-nodes.txt", sep = ""), pathway_object = union_network)
        output$file_union <- renderText({
          paste("Nodes saved in:", path)
        })
      })
    }

    #### UI ####
    ui <- shinyUI(navbarPage(
      title = "KeyPathwayMineR - Result",
      tabPanel(
        title = "Pathways",
        fluidPage(
          titlePanel("Select pathway:"),
          sidebarLayout(
            sidebarPanel(
              width = 3,
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
              hr(),
              h4("Export:"),
              actionButton("export_edges", "Export edges (SIF)"),
              actionButton("export_nodes", "Export nodes"),
              tableOutput("file")
            ),

            # Main panel for displaying outputs ----
            mainPanel(
              width = 9,

              visNetworkOutput("network", height = "700px"),
              fluidRow(
                h4("Pathway statistics:"),
                column(
                  width = 3,
                  verbatimTextOutput("nodes"),
                ),
                column(
                  width = 3,
                  verbatimTextOutput("edges")
                ),
                column(
                  width = 3,
                  verbatimTextOutput("avg_exp")
                )
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Union networks",

        titlePanel("Select union network:"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # Input: Selector for choosing dataset ----
            selectInput(
              inputId = "configuration_union",
              label = "Choose a configuration:",
              choices = get_configurations(result_object = result)
            ),
            hr(),
            h4("Export:"),
            actionButton("export_edges_union", "Export edges (SIF)"),
            actionButton("export_nodes_union", "Export nodes"),
            tableOutput("file_union")
          ),
          # Main panel for displaying outputs ----
          mainPanel(
            width = 9,
            visNetworkOutput("union_network", height = "700px"),
            fluidRow(
              h4("Pathway statistics:"),
              column(
                width = 3,
                verbatimTextOutput("union_nodes")
              ), column(
                width = 3,
                verbatimTextOutput("union_edges")
              ), column(
                width = 3,
                verbatimTextOutput("union_avg_exp")
              )
            )
          )
        )
      )
    ))

    shinyApp(ui = ui, server = server)
  } else {
    stop("Result object contains 0 extracted pathways. Visualization not possible.")
  }
}

#' Two pathway comparison plots.
#'
#' The first compares the union network
#' of each configuration.
#' The second compares the pathway with the
#' highest number of nodes of each configuration.
#'
#' @param result Result object from KeyPathwayMineR execution.
#'
#' @return Return two ggplots objects with the comparison plots and
#'         the data for creating the plots.
#' @export
#'
#' @importFrom tibble tibble
#' @import ggplot2
pathway_comparison_plots <- function(result) {
  # Union network comparison plot ####
  configurations <- get_configurations(result)
  config <- c()
  numNodes <- c()
  avgDiffExp <- c()
  for (configuration in configurations) {
    union_network <- get_pathway(configuration = get_configuration(result, configuration),union = TRUE)
    config <- c(config, configuration)
    numNodes <- c(numNodes, union_network@num_nodes)
    avgDiffExp <- c(avgDiffExp, union_network@avg_exp)
  }

  union_network_comparison_data <- tibble::tibble(config = config, numNodes = numNodes, avgDiffExp = avgDiffExp)

  union_network_comparison_plot <- ggplot(union_network_comparison_data, aes(x = numNodes, y = avgDiffExp)) +
    geom_point(aes(col = config), size = 3) +
    labs(
      title = "Union network comparison",
      subtitle = "Avg. de. cases per gene vs. number of genes",
      y = "Average de cases cases per gene",
      x = "Genes in the pathway",
      col = "Configurations"
    ) +
    theme(legend.position = "right", plot.title = element_text(size = 20), text = element_text(size = 15))
  # Best pathway comparison plot ####
  config <- c()
  numNodes <- c()
  avgDiffExp <- c()
  for (configuration in configurations) {
    pathways <- get_pathways(result_object = result, configuration_name = configuration)
    pathway <- pathways$`Pathway-1`
    config <- c(config, configuration)
    numNodes <- c(numNodes, pathway@num_nodes)
    avgDiffExp <- c(avgDiffExp, pathway@avg_exp)
  }
  top_pathway_comparison_data <- tibble::tibble(config = config, numNodes = numNodes, avgDiffExp = avgDiffExp)
  top_pathway_comparison_plot <- ggplot(top_pathway_comparison_data, aes(x = numNodes, y = avgDiffExp)) +
    geom_point(aes(col = config), size = 3) +
    labs(
      title = "Best pathway of each configuration comparison",
      subtitle = "Avg. de. cases per gene vs. number of genes",
      y = "Average de cases cases per gene",
      x = "Genes in the pathway",
      col = "Configurations"
    ) +
    theme(legend.position = "right", plot.title = element_text(size = 20), text = element_text(size = 15))

  return(list(union_network_comparison = list(plot = union_network_comparison_plot, data = union_network_comparison_data),
              top_pathway_comparison = list(plot = top_pathway_comparison_plot, data = top_pathway_comparison_data)))
}
