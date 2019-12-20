## Megan Grout
## 20191219
## PCA app for CycIF workflow

# Load libraries
library(shiny)
library(tidyverse)
library(gplots)
library(ggfortify)
library(plotly)

# Adjust maximum file size allowable, but large files not recommended
options(shiny.maxRequestSize = 3000*1024^2)


# get colors of samples - file must be in same directory as R file
if (file.exists("sample_color_data.csv")){
  sample_colors <- read.csv("sample_color_data.csv")
  sample_cols <- as.character(sample_colors$hex)
  names(sample_cols) <- sample_colors$Sample_ID
}


# Get length of dataframe
length_function <- function(d) {
  return(dim(d)[1])
}

# Perform PCA
# takes in data.frame d, Boolean for scaling scaleBool, and not_intensities
# Returns prcomp object
perform_PCA <- function(d, scaleBool, not_intensities) {
  pr.out <- prcomp(as.matrix(d[, !names(d) %in% not_intensities]), scale = scaleBool)
  return(pr.out)
}

# Get proportion of variance explained by each PC
# Takes in prcomp object
get_pve <- function(pca_data) {
  pve <- pca_data$sdev^2/sum(pca_data$sdev^2) 
  pve <-  pve %>% data.frame()
  names(pve) <- c("PVE")
  pve <- pve %>% mutate("PC" = as.numeric(rownames(.)))
  return(pve)
  }


# UI information 
ui <- fluidPage(
  
  # Application title
  titlePanel("CycIF Workflow: Principal Components Analysis Dashboard"),
  br(),
  h3('Load data'),
  fluidRow(
    column(3, fileInput("dataFile", "Choose data csv file to upload", accept = ".csv")),
    column(3, fileInput("metadataFile", "Choose metadata csv file to upload", accept = ".csv")),
    column(3, fileInput("intensitiesFile", "Choose 'not_intensities.csv' file to upload", accept = ".csv"))#,
    # Futher development could have multiple coloring information files input here
    # I was unable to get this to work within the constraints of the project
    #column(3, fileInput("coloringFiles","Choose coloring information files to uplaod", accept = ".csv",
                        #multiple = TRUE))
  ),
  h3("PCA Parameters"),
  fluidRow(
    column(3,
           radioButtons(inputId = "scaleButton", label = "Choose Data Scaling", 
                        choices = c("Scaled"="scaled", "Unscaled"="unscaled"), selected = NULL,
                        inline = FALSE, width = NULL)),
    column(9,
           checkboxGroupInput(inputId = "checkPCASamples", label ="Choose samples to perform PCA on", 
                              choiceNames = NULL, # should be set once we have data
                              choiceValues = NULL, # should be set once we have data
                              selected = NULL, inline=TRUE))
  ),
  br(),
  # Perform PCA
  fluidRow(
    column(2,actionButton("pcaButton", "Perform PCA")),
    column(2, downloadButton("downloadPCA","Download PCA rotated data"))
  ),
  br(),
  br(),
  h3("PCA Plots:"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      ## PCA options ##
      h4("Plotting Parameters"),

      ## Final PCs plot options ##
      # Y-axis PC number
      sliderInput("y_axis_pc", "Select Y-Axis PC", min = 1,
                  max= NULL, value = 2, step = 1), # max will be set to toal #PCs after PCA
      # X-axis PC number
      sliderInput("x_axis_pc", "Select X-Axis PC", min = 1,
                  max=NULL, value = 1, step = 1), # max will be set to toal #PCs after PCA
      # Color
      selectInput("pcs_color_opts","Select feature to color by",
                  choices = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Plots (generated after running PCA)"),
      plotlyOutput("pcs_plot"),
      plotlyOutput("pve_plot"),
      plotlyOutput("cum_pve_plot")
    )
  )
)

## Server information
server <- function(input, output, session) {
  
  # Get raw data
  data <- reactive({
    inFile <- input$dataFile
    if (is.null(inFile)) {
      if (exists("repro_PCA_test.csv")) {
      d <- read.csv("repro_PCA_test.csv")
      } else {
        return(NULL)
      }
    } else {
      d <- read.csv(inFile$datapath)
    }
    return(d)
  })
  
  # Get metadata
  metadata <- reactive({
    inFile <- input$metadataFile
    if (is.null(inFile)) {
      d <- read.csv("metadata.csv")
    } else {
      d <- read.csv(inFile$datapath)
    }
    # Recode various columns as factors
    if ("TMA_column" %in% names(d)){
      d$TMA_column <- as.factor(d$TMA_column)
    }
    if ("Replicate" %in% names(d)) {
      d$Replicate <- as.factor(d$Replicate)
    }
    return(d)
  })
  
  # Get not_intensities
  not_intensities <- reactive({
    inFile <- input$intensitiesFile
    if (is.null(inFile)) {
      d <- read.csv("not_intensities.csv", header = 0)
    } else {
      d <- read.csv(inFile$datapath, header = 0)
    }
    d <- d$V1 %>% as.character()
    return(d)
  })
  
  # Get color options for PC plot
  pcs_color_options <- eventReactive(metadata(),{
    d <- names(metadata())
    d <- c(d, "Sample_ID")
    d
  })
  
  # Determine samples in data
  samples <- eventReactive(data(),{
    #req(input$dataFile)
    data() %>% select(Sample_ID) %>% unique() %>% 
      deframe() %>% as.character() %>% str_sort(numeric=TRUE)
  })
  
  # Use samples() to update sample checkbox
  observeEvent(samples(),{
    updateCheckboxGroupInput(session, 
                             inputId = "checkPCASamples", 
                             choiceNames = samples(), # should be set once we have data
                             choiceValues = samples(), # should be set once we have data
                             selected = NULL, inline = TRUE)
  })
  
  # Scaled v unscaled
  output$scale <- renderPrint({ input$scaleButton })
  
  # Go PCA Button
  output$goPCA <- renderPrint({input$pcaButton})
  
  # Check which samples to process
  output$PCASamples <- renderPrint({ input$checkPCASamples })
  
  # Download PC data
  output$downloadPCA <- downloadHandler(
    filename = function() {
      paste("PCA_output_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pca_data()$x, file, row.names = TRUE)
    }
  )
  
  # update PC color options select widget
  # when metadataFile changes
  observeEvent(input$pcaButton, { #input$metadataFile,
    #pcs_color_options <- names(metadata())
    #pcs_color_options <- c(pcs_color_options, "Sample_ID")
    updateSelectInput(session, "pcs_color_opts",
                      choices = pcs_color_options(),
                    selected = 'Sample_ID')
  })
  
  # Update PC plot display parameter options
  observeEvent(input$pcaButton, {
    updateSliderInput(session, "x_axis_pc", min = 1,
                      max=dim(pca_data()$x)[2], value = 1, step = 1)
    updateSliderInput(session, "y_axis_pc", min = 1,
                      max=dim(pca_data()$x)[2], value = 2, step = 1)
    updateSelectInput(session, "pcs_color_opts",
                      choices = pcs_color_options(), selected = "Sample_ID")
  })
  
  
  # Perform PCA
  pca_data <- eventReactive(input$pcaButton, {
    #req(data())
    if (input$scaleButton == "scaled") {
      scaleBoolean <- TRUE
    } else { 
      scaleBoolean <- FALSE
    }
    d <- data() %>% filter(Sample_ID %in% input$checkPCASamples)
    perform_PCA(d, scaleBoolean, not_intensities())
  })
  
  # Create metadata df for PCs plot coloring
  for_plot <- eventReactive(input$pcaButton, {
    # not efficient to be calculating this twice
    d <- data() %>% filter(Sample_ID %in% input$checkPCASamples)
    join_cols <- c("Sample_ID","ROI_index","ROI_slide")
    join_cols <- join_cols[join_cols %in% names(metadata())]
    left_join(d, metadata(), by = join_cols) %>% 
      mutate("Sample_ID" = as.factor(Sample_ID)) %>% 
      filter(Sample_ID %in% input$checkPCASamples)
    
  })
  
  # Calculate Percent Variance Explained by PC
  pve <- function(){
    get_pve(pca_data())
  }

  
  # PCs plot
  output$pcs_plot <- renderPlotly({
    p <- autoplot(pca_data(), data = for_plot(), x = input$x_axis_pc, y = input$y_axis_pc, 
             colour = input$pcs_color_opts, alpha = 0.75,
             #colour = NULL,
             main = paste('PC',input$y_axis_pc,' by PC', input$x_axis_pc,
                          ' colored by ',input$pcs_color_opts, sep = "")) +  
      theme(
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.line.x =  element_line(color = "black", size = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))
    # If we successfully input sample color information, use that to color points
    # when 'Sample_ID' is selected.
    if ((input$pcs_color_opts == 'Sample_ID') && (exists('sample_cols'))) {
      p <- p + scale_color_manual(values = sample_cols)
    }
    p
  })

  
  # PVE plot
  output$pve_plot <- renderPlotly({
    ggplot(pve(), aes(x = PC, y = PVE)) + geom_line() + geom_point() +
      labs(title = "Variance explained by each PC", 
           x = "Principal component",y = "Variance explained (%)") +
      scale_x_continuous(breaks=seq(0,pve()$PC[dim(pve())[1]],5)) +
      theme(
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.line.x =  element_line(color = "black", size = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))
    
  })
  
  # Cumulative PVE plot 
  output$cum_pve_plot <- renderPlotly({
    ggplot(pve(), aes(x = PC, y = cumsum(pve()$PVE))) + geom_line() + geom_point() +
      labs(title = "Cumulative variance explained by each PC", 
           x = "Principal component",y = "Cumulative variance explained (%)") +
      scale_x_continuous(breaks=seq(0,pve()$PC[dim(pve())[1]],5)) +
      
      theme(
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.line.x =  element_line(color = "black", size = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))   
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

