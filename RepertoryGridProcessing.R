Sys.setlocale("LC_ALL", "Russian_Russia")

###################################
#install and load R packages      #
###################################
#install.packages("OpenRepGrid")
#install.packages("shiny")
#install.packages("devtools")
#install_github("AnalytixWare/ShinySky")
#install.packages("DT")
#install_github("daattali/shinyjs")
#install.packages("data.table")

library(devtools)
library(shiny)
library(shinythemes)
library(shinysky)
library(shinyjs)
library(DT)
library(data.table)
library(OpenRepGrid)

###################################
# Define UI                       #
###################################
ui <- fluidPage(
  theme = shinytheme("darkly"),
  navbarPage(
    #theme = "yeti",
    "Repertory Grid Processing",
    id = "inTabset",
    
    ############### tab 1 ui ###############
    tabPanel(
      "Elements",
      value = "elements_tab",
      sidebarPanel(
        width = 5,
        tags$h3("Input Elements:"),
        textInput("txt1", "Element 1", ""),
        #to the server
        actionButton("add", "Add Element"),
        actionButton("save",
                     "Submit Elements",
                     class = "btn btn-primary"),
      ),
      column(
        6,
        h2("Elements elicitation"),
        br(),
        p(
          "To begin with, you have to define Elements for the experiment. You can add as many elements as you want. Press:"
        ),
        strong("Add Element"),
        p("to add yet another element for the experiment."),
        p("OR"),
        strong("Submit Elements"),
        p("to finish adding elements and switch to the Constructs tab.")
      )
    ),
    
    ############### tab 2 ui ###############
    tabPanel(
      "Constructs",
      value = "constructs_tab",
      sidebarPanel(
        #width = 12,
        tags$h3("Elicited Elements:"),
        tableOutput("elements_table"),
      ),
      
      column(
        4,
        useShinyjs(),
        tags$h3("Input Constructs:"),
        textInput("txte", "Explicit Construct", ""),
        #to the server
        textInput("txti", "Implicit Construct", ""),
        #to the server
        actionButton("add_c",
                     "Add Constructs"),
        actionButton("save_c",
                     "Save Constructs",
                     class = "btn btn-primary"),
      ),
      
      mainPanel(
        width = 4,
        tags$h3("Elicited Constructs:"),
        tableOutput("constructs_table"),
        verbatimTextOutput("txt_c_out"),
      ),
      column(
        7,
        h2("Constructs elicitation"),
        br(),
        p(
          "On this page you have to enter elcitated bipolar constructs. Press:"
        ),
        strong("Add Constructs"),
        p(
          "after you entered both explicit and implicit constructs in the poles to save them."
        ),
        p("AND"),
        strong("Save Constructs"),
        p("to finish adding constructs and switch to the Raiting tab.")
      )
    ),
    
    ############### tab 3 ui ###############
    tabPanel(
      "Rating",
      value = "raiting_tab",
      sidebarPanel(
        tags$h3("Evaluation:"),
        align = "left",
        width = 4,
        dataTableOutput("rating_scale"),
        actionButton("fill_grid", "Fill the grid"),
        actionButton("finish_grid", "Finish filling",
                     class = "btn btn-primary"),
      ),
      mainPanel(#width = 7, 
        DTOutput('x1')),
      
      
      
      
      tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                   "
                      
                      
      )),
    
      
      
      
      column(
        7,
        align = "left",
        h2("Elements Raiting"),
        br(),
        p(
          "Now it is necessary to evaluate the elements in accordance with their relation to the constructs, where 1 means that element is closer to the left construct(explicit) and 5 - to the right one(implicit). You are only allowed to use numbers from 1 to 5. Press:"
        ),
        strong("Fill the grid"),
        p("to display the grid for evaluating."),
        p("AND"),
        strong("Finish filling"),
        p("to save result and switch to Resluts tab.")
      )
    ),
    
    ############### tab 4 ui ###############
    tabPanel(
      "Results",
      value = "results_tab",
      tabsetPanel(
        id = "tabSelected",
        ####### RepGrid ######
        tabPanel(
          "Repertory Grid",
          sidebarPanel(
            #fileInput("grid_file", "Choose XLSX File"),
            #actionButton("upload_grid", "Upload grid"),
            actionButton("create_grid",
                         "Create grid",
                         class = "btn btn-primary"),
            textOutput("created_true"),
            br(),
            actionButton("show_table",
                         "Show grid's information")
          ),
          mainPanel(verbatimTextOutput("table_out")),
          column(
            7,
            h2("Getting the results"),
            p(
              "In the next step you will be able to create a Repertory Grid object and use different metrics to analyze it. Press:"
            ),
            strong("Create grid"),
            p("to create a Repertory Grid object."),
            p("AND"),
            strong("Show grid's information"),
            p("So you can visualize the grid and see some brief information."),
            p("AND/OR"),
            p(
              "Now you can press metrics' names to analyze the Repertory Grid. The metrics are located on the top of the tab."
            )
          )
        ),
        ####### PCA ######
        tabPanel(
          "PCA",
          sidebarPanel(
            h4("Principal Components Analysis"),
            numericInput("n_factors",
                         "Number of components",
                         value = 3),
            selectInput(
              "pca_rotation",
              "Rotation",
              choices = list(
                "None" = 'none',
                "Varimax" = 'varimax',
                "Promax" = 'promax',
                "Cluster" = 'cluster'
              ),
              selected = 'none'
            ),
            selectInput(
              "pca_method",
              "Method",
              choices = list(
                "Pearson" = 'pearson',
                "Kendall" = 'kendall',
                "Spearman" = 'spearman'
              ),
              selected = 'pearson'
            ),
            actionButton("pca_button", "Analyze",
                         class = "btn btn-primary")
          ),
          mainPanel(verbatimTextOutput("pca_out"))
        ),
        ####### Cluster Analysis ######
        tabPanel(
          "Cluster Analysis",
          sidebarPanel(
            h4("Cluster analysis of elements and constructs"),
            selectInput(
              "ca_along",
              "Dimension to cluster",
              choices = list(
                "Constructs" = 1,
                "Elements" = 2,
                "Both" = 0
              ),
              selected = 0
            ),
            selectInput(
              "ca_dmethod",
              "Distance measure",
              choices = list(
                "Euclidean" = 'euclidean',
                "Maximum" = 'maximum',
                "Manhattan" = 'manhattan',
                "Binary" = 'binary',
                "Minkowski" = 'minkowski'
              ),
              selected = 'euclidean'
            ),
            selectInput(
              "ca_cmethod",
              "Agglomeration method",
              choices = list(
                "Ward" = "ward",
                "Single" = "single",
                "Complete" = "complete",
                "Average" = "average",
                "Median" = "median"
              ),
              selected = 'ward'
            ),
            checkboxInput("ca_align",
                          label = "Align the constructs before clustering?",
                          value = TRUE),
            actionButton("ca_button", "Analyze",
                         class = "btn btn-primary")
            
          ),
          mainPanel(imageOutput(
            "ca_out",
            width = "100%",
            height = "400px"
          ))
          
        ),
        ####### Intensity index ######
        tabPanel(
          "Intensity index",
          sidebarPanel(
            "Bannister's measure",
            actionButton("ii_button", "Analyze",
                         class = "btn btn-primary")
          ),
          mainPanel(verbatimTextOutput("ii_out"))
          
        ),
        ####### Somer's D ######
        tabPanel(
          "Somer's D",
          sidebarPanel(
            "Somer's D",
            actionButton("sd_button", "Analyze",
                         class = "btn btn-primary")
          ),
          mainPanel(verbatimTextOutput("sd_out"))
        ),
        ####### Construct Correlations ######
        tabPanel(
          "Construct Correlations",
          sidebarPanel(
            "Construct Correlations",
            selectInput(
              "cor_method",
              "Method",
              choices = list(
                "Pearson" = 'pearson',
                "Kendall" = 'kendall',
                "Spearman" = 'spearman'
              ),
              selected = 'pearson'
            ),
            actionButton("cc_button", "Analyze",
                         class = "btn btn-primary")
          ),
          mainPanel(verbatimTextOutput("cc_out"))
        ),
        ####### Distances ######
        tabPanel(
          "Distances",
          sidebarPanel(
            "Distances",
            selectInput(
              "dist_along",
              "Calculate for",
              choices = list("Constructs" = 1,
                             "Elements" = 2),
              selected = 1
            ),
            selectInput(
              "dist_dmethod",
              "Distance measure",
              choices = list(
                "Euclidean" = 'euclidean',
                "Maximum" = 'maximum',
                "Manhattan" = 'manhattan',
                "Binary" = 'binary',
                "Minkowski" = 'minkowski'
              ),
              selected = 'euclidean'
            ),
            actionButton("dist_button", "Analyze",
                         class = "btn btn-primary")
          ),
          mainPanel(verbatimTextOutput("dist_out"))
        ),
        ####### Biplot ######
        tabPanel(
          "Biplot",
          sidebarPanel(
            "Biplot",
            actionButton("bipl_button", "Create",
                         class = "btn btn-primary")
          ),
          mainPanel(plotOutput("bipl_out"))
        )
      )
    )
  )
)

###################################
# Some variables                  #
###################################
vals <- reactiveValues(n = 2)
vals$elArray <- vector()

vals_c <- reactiveValues(m = 2)
vals_c$conEArray <- vector()
vals_c$conIArray <- vector()

grid_df <- data.frame()


###################################
# Define server function          #
###################################
server <- function(input, output, session) {
  
  ############### tab 1 server ###############
  observeEvent(input$add, {
    insertUI(
      selector = paste0('#txt', vals$n - 1),
      where = "afterEnd",
      ui = textInput(paste0('txt', vals$n), paste('Element', vals$n, sep = ' '), ""),
    )
    isolate(vals$n <- vals$n + 1)
  })
  
  observeEvent(input$save, {
    for (i in seq(1:(vals$n - 1))) {
      vals$elArray[paste0('element', i)] = input[[paste0('txt', i)]]# to 2nd tab
    }
    updateTabsetPanel(session, "inTabset",
                      selected = "constructs_tab")
  })
  
  ############### tab 2 server ###############
  output$elements_table <- renderTable(vals$elArray)
  
  observeEvent(input$add_c, {
    reset()
    vals_c$conEArray <- append(vals_c$conEArray, input$txte)
    vals_c$conIArray <- append(vals_c$conIArray, input$txti)
    
    output$constructs_table <-
      renderTable(paste(vals_c$conEArray,
                        vals_c$conIArray,
                        sep = ' : '))
  })
  
  observeEvent(input$save_c, {
    updateTabsetPanel(session, "inTabset",
                      selected = "raiting_tab")
  })
  
  ############### tab 3 server ###############
  observeEvent(input$fill_grid, {
    grid_size = (length(vals$elArray) + 2) * length(vals_c$conIArray)
    
    tab <- matrix(
      rep(0, grid_size),
      ncol = (length(vals$elArray) + 2),
      nrow = length(vals_c$conIArray)
    )
    colnames(tab) <-
      c("Explicit Construct", vals$elArray, "Implicit Construct")
    
    x = as.data.frame(
      tab,
      col.names = c("Explicit Construct", vals$elArray, "Implicit Construct"),
      stringsAsFactors = default.stringsAsFactors(),
      row.names = FALSE
    )
    
    x["Explicit Construct"] <- vals_c$conEArray
    x["Implicit Construct"] <- vals_c$conIArray
    
    output$x1 = renderDT(
      x,
      selection = 'none',
      editable = TRUE,
      options = list(columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      ))
    )

    proxy = dataTableProxy('x1')
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)  # important
      
      grid_df <- x
      raitings_df <- x[, 2:(length(vals$elArray) + 1)]
      raitings <<- as.vector(t(raitings_df))
    })
  })
  observeEvent(input$finish_grid, {
    updateTabsetPanel(session, "inTabset",
                      selected = "results_tab")
  })
  ############### tab 4 server ###############
  #observeEvent(input$upload_grid, {
  #file <- input$grid_file
  
  observeEvent(input$create_grid, {
    #if (is.null(file)){
    args <-  list(
      name = vals$elArray,
      l.name = vals_c$conEArray,
      r.name = vals_c$conIArray,
      scores = raitings
    )
    grid <- makeRepgrid(args)
    grid <- setScale(grid, 1, 5, 1)
    output$created_true <-
      renderText({
        "The Repertory Grid object is created"
      })
    
    #}
    #else {
    #grid <- importExcel(file, sheetIndex = 1)
    #output$created_true <- renderText({"A grid is imported"})
    #}
    
    
    ####### processing #######
    observeEvent(input$show_table, {
      output$table_out <- renderPrint({
        grid
      })
    })
    
    #PCA
    observeEvent(input$pca_button, {
      output$pca_out <- renderPrint(
        constructPca(
          grid,
          nfactors = input$n_factors,
          rotate = input$pca_rotation,
          method = input$pca_method
        )
      )
    })
    #Components Analysis
    observeEvent(input$ca_button, {
      output$ca_out <- renderPlot(
        cluster(
          grid,
          along = input$ca_along,
          dmethod = input$ca_dmethod,
          cmethod = input$ca_cmethod,
          align = input$ca_align
        )
      )
    })
    #Intensity Index
    observeEvent(input$ii_button, {
      output$ii_out <- renderPrint(indexIntensity(grid, rc = FALSE))
    })
    #Somer's D
    observeEvent(input$sd_button, {
      output$sd_out <- renderPrint(constructD(grid))
    })
    #Construct Correlations
    observeEvent(input$cc_button, {
      output$cc_out <- renderPrint(constructCor(grid,
                                                method = input$cor_method,
                                                index = TRUE))
    })
    #Distances
    observeEvent(input$dist_button, {
      output$dist_out <- renderPrint(distance(
        grid,
        dmethod = input$dist_dmethod,
        along = input$dist_along
      ))
    })
    #Biplot
    observeEvent(input$bipl_button, {
      output$bipl_out <-
        renderPlot(biplotSimple(grid)) # or biplot2d
    }) 
  })
  #})
  
}
###################################
# Create Shiny object             #
###################################

shinyApp(ui = ui, server = server)
