Sys.setlocale("LC_ALL", "Russian_Russia")

###################################
#install and load R packages      #
###################################
#install.packages("https://cran.r-project.org/bin/windows/Rtools/ or https://www.r-project.org/nosvn/winutf8/ucrt3/")
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
#library(shinysky)
library(shinyjs)
library(DT)
library(data.table)
library(OpenRepGrid)

###################################
# Define UI                       #
###################################
ui <- fluidPage(
  
  #setting the theme
  theme = shinytheme("darkly"),
  
  #navigation bar
  navbarPage(
    #theme = "yeti",
    "Repertory Grid Processing",
    id = "inTabset",
    
    
    
    ############### tab 1 ui ###############
    tabPanel(
      "Elements", value = "elements_tab",
      
      fluidRow(
        
        #Left column for adding and saving elements
        column(
          width = 5,
          
          #Window to input elements
          sidebarPanel(
            width = 12,
            tags$h3("Input Elements:"), 
            
            #Text input box
            textInput("txt1","Element 1",""),
            
            #Button to add elements
            actionButton("add", "Add Element"),
            
            #Button to finish adding elements
            actionButton("save",
                         "Submit Elements",
                         class = "btn btn-primary"),
          )
        ),
        
        #Right column for selecting a RepGrid and explanation
        column(
          width = 7,
          
          #Window to load previously built RepGrid
          sidebarPanel(
            width = 12,
            tags$h3("Use previously created RepGrid"),
            
            #Selection of the grid
            selectInput(inputId = "repgrid_select",
                        label = "Choose the RepGrid:",
                        
                        #options to choose from
                        choices = tools::file_path_sans_ext(basename(
                          list.files(path = "repgrids",
                                     pattern = "*.rds", full.names = TRUE))
                        )
            ),
            
            #to display buttons one above another
            div(style="display: flex; flex-direction: column;",
                
                #Button to confirm the grid choice
                actionButton("Grid_chosen",
                             "Confirm choice",
                             class = "btn btn-primary"),
                
                br(),
                
                #Button to clear all created RepGrids
                actionButton("clear_history",
                             "Clear history",
                             class = "btn btn-danger",
                ),
                
                #Warn user that it's impossible to undo the clearance of history
                textOutput("warning")
            )
          ),
          
          #Explanation of what to do on this page
          column(
            width = 12,
            
            h2("Elements elicitation"),
            br(),
            p(
              "To begin with, you have to define Elements for the experiment. You can add as many elements as you want. Press:"
            ),
            strong("Add Element"),
            p("to add yet another element for the experiment."),
            p("OR"),
            strong("Submit Elements"),
            p("to finish adding elements and switch to the Constructs tab."),
            br(),
            p("OR"),
            p("Choose the grid from the ones you created earlier by pressing 'Choose the grid'")
          )
        )
      )
    ),
    
    
    
    ############### tab 2 ui ###############
    
    #this panel is used to make pairs of constructs
    tabPanel(
      "Constructs", #visible name
      value = "constructs_tab", #id
      
      #a window displaying the elements
      sidebarPanel(
        tags$h3("Elicited Elements:"),
        tableOutput("elements_table"),
      ),
      
      #a place to input pairs of constructs
      column(
        width = 4,
        useShinyjs(),
        tags$h3("Input Constructs:"),
        
        #box to input the explicit construct
        textInput("txte", "Explicit Construct", ""),
        
        #box to input the implicit construct
        textInput("txti", "Implicit Construct", ""),
        
        #button to add a pair of constructs
        actionButton("add_c",
                     "Add Constructs"),
        
        #button to finish making constructs
        actionButton("save_c",
                     "Save Constructs",
                     class = "btn btn-primary"),
      ),
      
      #panel to display entered constructs
      mainPanel(
        width = 4,
        tags$h3("Elicited Constructs:"),
        
        #displaying already made constructs
        tableOutput("constructs_table"),
        
        verbatimTextOutput("txt_c_out"),
      ),
      
      #a text explanation of what to do on this page
      column(
        width = 7,
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
    
    #this panel is for filling the grid in
    tabPanel(
      "Rating", #visible name
      value = "raiting_tab", #id
      
      #panel for evaluation
      sidebarPanel(
        tags$h3("Evaluation:"),
        align = "left",
        width = 4,
        
        #the RepGrid to fill
        dataTableOutput("rating_scale"),
        
        #button to start filling the grid
        actionButton("fill_grid", "Fill the grid"),
        p(),
        
        #text input box to name a RepGrid
        textInput("filename", label="Enter the name of the grid"),
        
        #button to finish filling
        actionButton("finish_grid", "Finish",
                     class = "btn btn-primary"),
      ),
      
      mainPanel(
  
        #displaying the DataTable
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
      
      #a text explanation of what to do on this page
      column(
        width = 7,
        align = "left",
        
        h2("Elements Raiting"),
        br(),
        p(
          "Now it is necessary to evaluate the elements in accordance with their relation to the constructs, where 1 means that element is closer to the left construct(explicit) and 5 - to the right one(implicit). You are only allowed to use numbers from 1 to 5. Press:"
        ),
        strong("Fill the grid"),
        p("to display the grid for evaluating."),
        p("Enter a filename"),
        p("AND"),
        strong("Finish"),
        p("to save result and switch to Resluts tab.")
      )
    ),
    
    ############### tab 4 ui ###############
    
    #panel to view different metrics of created or chosen  RepGrid
    tabPanel(
      "Results", #visible name
      value = "results_tab", #id
      
      #subpanel for choosing the metric to view
      tabsetPanel(
        id = "tabSelected",
        
        #a panel to create the RepGrid and view the general info
        tabPanel(
          "Repertory Grid", #visible name
          
          #place to create, name, and show the grid
          sidebarPanel(

            #a button to create grid
            actionButton("create_grid",
                         "Create grid",
                         class = "btn btn-primary"),
            
            #an indicator showing if the grid was created
            textOutput("created_true"),
            br(),

            #button to view the general info about the grid
            actionButton("show_table",
                         "Show grid's information")
          ),
          
          #an exlanation of what to do in this panel
          mainPanel(verbatimTextOutput("table_out")),
          column(
            width = 7,
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
        
        
        
        
        #PCA
        tabPanel(
          "PCA",
          
          #place to choose PCA parameters
          sidebarPanel(
            
            h4("Principal Components Analysis"),
            
            #input the number of components
            numericInput("n_factors",
                         "Number of components",
                         value = 1),
            
            #select the rotation option
            selectInput(
              "pca_rotation",
              "Rotation",
              
              #Rotations to choose from
              choices = list(
                "None" = 'none',
                "Varimax" = 'varimax',
                "Promax" = 'promax',
                "Cluster" = 'cluster'
              ),
              
              #default option
              selected = 'none'
            ),
            
            #select the method for PCA
            selectInput(
              "pca_method",
              "Method",
              
              #methods to choose from
              choices = list(
                "Pearson" = 'pearson',
                "Kendall" = 'kendall',
                "Spearman" = 'spearman'
              ),
              
              #default option
              selected = 'pearson'
            ),
            
            #button to analyze with chosen PCA parameters
            actionButton("pca_button", "Analyze",
                         class = "btn btn-primary")
          ),
          
          mainPanel(verbatimTextOutput("pca_out"))
        ),
        
        
        
      
        ####### Cluster Analysis ######
        tabPanel(
          "Cluster Analysis",
          
          #place to choose parameters for claster analysis
          sidebarPanel(
            h4("Cluster analysis of elements and constructs"),
            
            #select dimension for cluster analysis
            selectInput(
              "ca_along",
              "Dimension to cluster",
              
              #options to choose from
              choices = list(
                "Constructs" = 1,
                "Elements" = 2,
                "Both" = 0
              ),
              
              #default value
              selected = 0
            ),
            
            #select cluster analysis method
            selectInput(
              "ca_dmethod",
              "Distance measure",
              
              #options to choose from
              choices = list(
                "Euclidean" = 'euclidean',
                "Maximum" = 'maximum',
                "Manhattan" = 'manhattan',
                "Binary" = 'binary',
                "Minkowski" = 'minkowski'
              ),
              
              #default value
              selected = 'euclidean'
            ),
            
            #select method of cluster analysis
            selectInput(
              "ca_cmethod",
              "Agglomeration method",
              
              #options to choose from
              choices = list(
                "Ward" = "ward",
                "Single" = "single",
                "Complete" = "complete",
                "Average" = "average",
                "Median" = "median"
              ),
              
              #default value
              selected = 'ward'
            ),
            
            #indicator of aligning the constructs before clustering
            checkboxInput("ca_align",
                          label = "Align the constructs before clustering?",
                          value = TRUE),
            
            #button to start cluster analysis
            actionButton("ca_button", "Analyze",
                         class = "btn btn-primary")
            
          ),
          
          #displaying the results of CA
          mainPanel(imageOutput(
            "ca_out",
            width = "100%",
            height = "400px"
          ))
          
        ),
        
        
        
        ####### Intensity index ######
        tabPanel(
          "Intensity index",
          
          #place to do bunnister's measure
          sidebarPanel(
            "Bannister's measure",
            
            #button to get bannister's measure
            actionButton("ii_button", "Analyze",
                         class = "btn btn-primary")
          ),
          
          #displaying intensity index
          mainPanel(verbatimTextOutput("ii_out"))
        ),
        
        
        
        ####### Somer's D ######
        tabPanel(
          "Somer's D",
          
          sidebarPanel(
            "Somer's D",
            
            #button to get somer's D metric
            actionButton("sd_button", "Analyze",
                         class = "btn btn-primary")
          ),
          
          #displaying the somer's D
          mainPanel(verbatimTextOutput("sd_out"))
        ),
        
        
        
        ####### Construct Correlations ######
        tabPanel(
          "Construct Correlations",
          
          #place to choose parameters for construct correlations
          sidebarPanel(
            "Construct Correlations",
            
            #select the method
            selectInput(
              "cor_method",
              "Method",
              
              #options ro choose from
              choices = list(
                "Pearson" = 'pearson',
                "Kendall" = 'kendall',
                "Spearman" = 'spearman'
              ),
              
              #default value
              selected = 'pearson'
            ),
            
            #button to get construct correlation
            actionButton("cc_button", "Analyze",
                         class = "btn btn-primary")
          ),
          
          #displaying CC
          mainPanel(verbatimTextOutput("cc_out"))
        ),
        
        
        
        ####### Distances ######
        tabPanel(
          "Distances",
          
          #place to choose options for distance calculation
          sidebarPanel(
            "Distances",
            
            #select the object of distance analysis
            selectInput(
              "dist_along",
              "Calculate for",
              
              #options to choose from
              choices = list("Constructs" = 1,
                             "Elements" = 2),
              
              #default value
              selected = 1
            ),
            
            #select a method for measuring distance
            selectInput(
              "dist_dmethod",
              "Distance measure",
              
              #options to choose from
              choices = list(
                "Euclidean" = 'euclidean',
                "Maximum" = 'maximum',
                "Manhattan" = 'manhattan',
                "Binary" = 'binary',
                "Minkowski" = 'minkowski'
              ),
              
              #default value
              selected = 'euclidean'
            ),
            
            #button to get distance analysis
            actionButton("dist_button", "Analyze",
                         class = "btn btn-primary")
          ),
          
          #displaying the results of distance analysis
          mainPanel(verbatimTextOutput("dist_out"))
        ),
        
        
        
        ####### Biplot ######
        tabPanel(
          "Biplot",
          
          sidebarPanel(
            "Biplot",
            
            #button to draw a biplot
            actionButton("bipl_button", "Create",
                         class = "btn btn-primary")
          ),
          
          #displaying the biplot
          mainPanel(plotOutput("bipl_out"))
        )
      )
    )
  )
)

###################################
# Some variables                  #
###################################

#number of elements  + 1
vals <- reactiveValues(n = 2)

#vector containing elements of the grid
vals$elArray <- vector()

#number of construct pairs + 1
vals_c <- reactiveValues(m = 2)

#vector of explicit constructs
vals_c$conEArray <- vector()

#vector of implicit constructs
vals_c$conIArray <- vector()

#RepGrid template for filling the grid
grid_df <- data.frame()

#RepGrid object for loading created grid
grid

#an indicator of an upload of created grid
Grid_chosen_pressed = FALSE

#Directory to store created repertory grids
if (file.exists("repgrids") && file.info("repgrids")$isdir) {
  
} else {
  dir.create("repgrids")
}



###################################
# Define server function          #
###################################
server <- function(input, output, session) {
  
  
  
  ############### tab 1 server ###############
  output$warning <- renderText({
    "WARNING, IMPOSSIBLE TO UNDO"
  })
  
  #Get the grid from file if the button is pressed
  observeEvent(input$Grid_chosen, {
    
    #changing the indicator to TRUE since the button has been pressed
    Grid_chosen_pressed <<- TRUE
    
    # switch to Results tab to analyze created grid
    updateTabsetPanel(session, "inTabset", selected = "results_tab")
    })
  
  #Clear history
  observeEvent(input$clear_history,{
    
    output$warning <- renderText({
      "History has been cleared"
    })
    
    #remove all files in the directory
    file.remove(list.files(path = paste0(getwd(),"/","repgrids"), full.names = TRUE))
  })
  
  #adding element to the grid
  observeEvent(input$add, {
    
    #Insert a new UI element into the app
    insertUI(
      
      #Find the UI element to insert the new element after
      selector = paste0('#txt', vals$n - 1), 
      
      #Specify where to insert the new element (after the previous one)
      where = "afterEnd",
      
      #Designing the UI to insert
      ui = textInput(paste0('txt', vals$n), #id
                     paste('Element', vals$n, sep = ' '), #visible name
                     ""), #default value
    )
    
    # Increment the value of vals$n (the number of elements added)
    isolate(vals$n <- vals$n + 1)
  })
  
  #saving added elements
  observeEvent(input$save, {
    
    #loop through all the elements
    for (i in seq(1:(vals$n - 1))) {
      
      # store the value of the i-th element in the elArray
      vals$elArray[paste0('element', i)] = input[[paste0('txt', i)]]
    }
    
    #switch to the constructs_tab
    updateTabsetPanel(session, "inTabset",
                      selected = "constructs_tab")
  })
  

  
  ############### tab 2 server ###############
  
  #displaying the added elements
  output$elements_table <- renderTable(vals$elArray)
  
  #adding constructs
  observeEvent(input$add_c, {
    
    #resetting the text input boxes
    reset()
    
    #Add explicit construct from the text input box
    vals_c$conEArray <- append(vals_c$conEArray, input$txte)
    
    #Add implicit construct from the text input box
    vals_c$conIArray <- append(vals_c$conIArray, input$txti)
    
    #displaying added construct pairs
    output$constructs_table <-  
      renderTable(paste(vals_c$conEArray,
                        vals_c$conIArray,
                        sep = ' : '))   
  })
  
  #saving added constructs
  observeEvent(input$save_c, {
    
    #switching to the grid evaluating tab
    updateTabsetPanel(session, "inTabset",
                      selected = "raiting_tab")
  })
  
  
  
  ############### tab 3 server ###############
  
  #filling the grid
  observeEvent(input$fill_grid, {
    
    # calculate the size of the grid
    grid_size = (length(vals$elArray) + 2)* length(vals_c$conIArray)  # +2 for columns that will contain construct names
    
    # create a matrix to store the data, with appropriate column and row names
    tab <- matrix(
      rep(0, grid_size), #vector of zeros of length grid_size
      ncol = (length(vals$elArray) + 2), #number of columns
      nrow = length(vals_c$conIArray) #number of rows
    )
    
    #setting the column names
    colnames(tab) <- c("Explicit Construct", vals$elArray, "Implicit Construct")
    
    #converting the matrix to a data frame
    x = as.data.frame(
      tab,
      col.names = c("Explicit Construct", vals$elArray, "Implicit Construct"),
      stringsAsFactors = default.stringsAsFactors(),
      row.names = FALSE
    )
    
    #set the values for the first and last columns of the data frame
    x["Explicit Construct"] <- vals_c$conEArray
    x["Implicit Construct"] <- vals_c$conIArray
    
    #render the data table with editable cells
    output$x1 = renderDT(
      x,
      selection = 'none',
      editable = TRUE,
      options = list(columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      ))
    )
    
    #create a proxy for the data table
    proxy = dataTableProxy('x1')
    
    #editing the cell
    observeEvent(input$x1_cell_edit, {
      
      #getting the cell edit info
      info = input$x1_cell_edit              
      str(info)
      
      #getting row index, column index
      i = info$row                           
      j = info$col
      
      #getting new value
      v = info$value
      
      #update the data frame
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      
      #replace the data in the data table without resetting the page
      replaceData(proxy, x, resetPaging = FALSE)
      
      #update the grid data frame
      grid_df <- x
      
      #getting the ratings data frame
      raitings_df <- x[, 2:(length(vals$elArray) + 1)]
      
      #update the ratings vector
      raitings <<- as.vector(t(raitings_df))
    })
  })
  
  #finish filling
  observeEvent(input$finish_grid, {
    
    #switching to the results tab
    updateTabsetPanel(session, "inTabset",
                      selected = "results_tab")
  })
  
  
  
  ############### tab 4 server ###############
  
  #creating the RepGrid object
  observeEvent(input$create_grid, {
    
    #if user decided to make new grid
    if (Grid_chosen_pressed == FALSE){
    
    #creating a list of arguments for RepGrid
    args <-  list(
      
      name = vals$elArray,       #column names
      l.name = vals_c$conEArray, #explicit contructs on the left
      r.name = vals_c$conIArray, #implicit constructs on the right
      scores = raitings          #values
    )
    
    #creating new RepGrid
    grid <- makeRepgrid(args)
    
    #setting the scale for new RepGrid
    grid <- setScale(grid, 1, 5, 1)
    
    #saving new grid as an .RDS file
    saveRDS(grid, file = paste0(getwd(),"/","repgrids", "/", input$filename, ".rds"))
    
    #if the user decided to load created RepGrid
    }else{
      
      #load the grid from the chosen file
      grid <- readRDS(file = paste0(getwd(), "/","repgrids","/",input$repgrid_select, ".rds"))
    }
    
    #inform the user that the RepGrid was created
    output$created_true <- renderText({
        "The Repertory Grid object is created"
      })
    

    
    ####### processing #######
    
    #showing RepGrid info
    observeEvent(input$show_table, {
      output$table_out <- renderPrint({
        grid
      })
    })
    
    #displaying the PCA results
    observeEvent(input$pca_button, {
      output$pca_out <- renderPrint(
        constructPca(
          grid, #the grid to use                       
          
          #chosen parameters
          nfactors = input$n_factors,  
          rotate = input$pca_rotation,
          method = input$pca_method
        )
      )
    })
    
    
    #displaying components Analysis
    observeEvent(input$ca_button, {
      output$ca_out <- renderPlot(
        cluster(
          grid, #the grid to use
          
          #chosen parameters
          along = input$ca_along,
          dmethod = input$ca_dmethod,
          cmethod = input$ca_cmethod,
          align = input$ca_align
        )
      )
    })
    
    
    #displaying intensity Index
    observeEvent(input$ii_button, {
      output$ii_out <- renderPrint(indexIntensity(grid, rc = FALSE))
    })
    
    
    #displaying somer's D
    observeEvent(input$sd_button, {
      output$sd_out <- renderPrint(constructD(grid))
    })
    
    
    #Showing Construct Correlations
    observeEvent(input$cc_button, {
      output$cc_out <- renderPrint(constructCor(grid,
                                                method = input$cor_method, #chosen method
                                                index = TRUE))
    })
    
    
    #showing distances
    observeEvent(input$dist_button, {
      output$dist_out <- renderPrint(distance(
        grid, #grid to use
        
        #chosen parameters
        dmethod = input$dist_dmethod,
        along = input$dist_along
      ))
    })
    
    
    #showing biplot
    observeEvent(input$bipl_button, {
      output$bipl_out <-
        renderPlot(biplotSimple(grid)) 
    }) 
  })
}



###################################
# Create Shiny object             #
###################################

shinyApp(ui = ui, server = server)
