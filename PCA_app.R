## Megan Grout
## 20191211
## PCA app for CycIF workflow

library(shiny)
library(tidyverse)
library(fivethirtyeight)
library(gplots)
library(ggfortify)
library(plotly)
options(shiny.maxRequestSize = 3000*1024^2)

##load the PCA data
data <- read.csv("repro_PCA_test.csv")

## not intensities
not_intensities <- read.csv("not_intensities.csv", header = 0)
not_intensities <- not_intensities$V1 %>% as.character()

# get colors of samples
if (file.exists("sample_color_data_repro.csv")){
  sample_colors <- read.csv("sample_color_data_repro.csv")
  sample_cols <- as.character(sample_colors$hex)
  names(sample_cols) <- sample_colors$Sample_ID
}
# get colors of 

## perform PCA 
# scaled vs unscaled
#pr.out <- prcomp(as.matrix(data[, !names(data) %in% not_intensities]), scale = F)

length_function <- function(d) {
  
  return(dim(d)[1])
}

perform_PCA <- function(d, scaleBool, not_intensities) {
  pr.out <- prcomp(as.matrix(d[, !names(d) %in% not_intensities]), scale = scaleBool)
  return(pr.out)
}

get_pve <- function(pca_data) {
  pve <- pca_data$sdev^2/sum(pca_data$sdev^2) 
  pve <-  pve %>% data.frame()
  names(pve) <- c("PVE")
  pve <- pve %>% mutate("PC" = as.numeric(rownames(.)))
  return(pve)
  }



# get metadata
#metadata2 <- read.csv("more_metadata_for_PCA_test.csv") %>% mutate("treatment" = tolower(treatment))
metadata <- read.csv("ROI_Map.csv")
if ("TMA_column" %in% names(metadata)) {
  metadata$TMA_column <- as.factor(metadata$TMA_column)
}
if ("Replicate" %in% names(metadata)) {
  metadata$Replicate <- as.factor(metadata$Replicate)
}

# join PCA output and metadata
#for_plot <- left_join(data, metadata, by = c("Sample_ID","ROI_index","ROI_slide")) %>% mutate("Sample_ID" = as.factor(Sample_ID))

# select PCs plot color options
#pcs_color_options <- names(for_plot[, !names(for_plot) %in% names(data)])
pcs_color_options <- names(metadata)
pcs_color_options <- c(pcs_color_options, "Sample_ID")

#data(biopics)
#biopics <- biopics %>% filter(!is.na(box_office))
##specify what categories we want to color with
#select_color_options <- c("type_of_subject", "subject_race", "subject_sex","year_release")
#min_year <- min(biopics$year_release)
#max_year <- max(biopics$year_release)

### Change this!!! ###
m <- data %>% select(Sample_ID) %>% unique() %>% 
  deframe() %>% as.character() %>% str_sort(numeric=TRUE)


# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("CycIF Workflow: Principal Components Analysis Dashboard"),
  br(),
  h3('Load data'),
  fluidRow(
    column(3, fileInput("dataFile", "Choose data csv file to upload", accept = ".csv")),
    column(3, fileInput("metadataFile", "Choose metadata csv file to upload", accept = ".csv")),
    column(3, fileInput("intensitiesFile", "Choose 'not_intensities.csv' file to upload", accept = ".csv"))
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
      ## Add User Interface element here
      
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
                  choices = NULL),#, selected = 'xx'),
      
      selectInput("staticInput","Old label", 
                  choices = c('c1','c2','c3'), selected = 'c1')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Plots (generated after running PCA)"),
      plotlyOutput("pcs_plot"),
      textOutput("scaleSampleSummary"),
      textOutput("reactiveAttempt"),
      plotlyOutput("pve_plot"),
      plotlyOutput("cum_pve_plot")
      #tableOutput("table")
    )
  )
)

##Server is where all of the computations happen
server <- function(input, output, session) {
  
  data <- reactive({
    inFile <- input$dataFile
    if (is.null(inFile)) {
      d <- read.csv("repro_PCA_test.csv")
    } else {
      d <- read.csv(inFile$datapath)
    }
    return(d)
  })
  
  metadata <- reactive({
    inFile <- input$metadataFile
    if (is.null(inFile)) {
      d <- read.csv("ROI_Map.csv")
    } else {
      d <- read.csv(inFile$datapath)
    }
    if ("TMA_column" %in% names(d)){
      d$TMA_column <- as.factor(d$TMA_column)
    }
    if ("Replicate" %in% names(d)) {
      d$Replicate <- as.factor(d$Replicate)
    }
    return(d)
  })
  
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
  
  pcs_color_options <- eventReactive(metadata(),{
    d <- names(metadata())
    d <- c(d, "Sample_ID")
    d
  })
  
  samples <- eventReactive(data(),{
    data() %>% select(Sample_ID) %>% unique() %>% 
      deframe() %>% as.character() %>% str_sort(numeric=TRUE)
  })
  
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

  output$downloadPCA <- downloadHandler(
    filename = function() {
      paste("PCA_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pca_data()$x, file, row.names = TRUE)
    }
  )
  
  output$scaleSampleSummary <- renderText({
    # Simply accessing input$goButton here makes this reactive
    # object take a dependency on it. That means when
    # input$goButton changes, this code will re-execute.
    input$pcaButton
    
    # input$text is accessed here, so this reactive object will
    # take a dependency on it. However, input$ is inside of
    # isolate(), so this reactive object will NOT take a
    # dependency on it; changes to input$n will therefore not
    # trigger re-execution.
    paste0('input$scaleButton is: "', isolate(input$scaleButton),
      '", and input$checkPCASamples is: "', isolate(input$checkPCASamples),'"')
  })
  
  # update PC color options select widget
  # when metadataFile changes
  observeEvent(input$pcaButton, { #input$metadataFile,
    #pcs_color_options <- names(metadata())
    #pcs_color_options <- c(pcs_color_options, "Sample_ID")
    updateSelectInput(session, "pcs_color_opts",
                      choices = pcs_color_options(),
                    selected = 'Sample_ID')
  })
  
  # Update 
  observeEvent(input$pcaButton,{
    updateSelectInput(session, "staticInput", label = "New label", 
                      #choices = pcs_color_options,
                      choices = c('c4','c5','c6'),
                      selected = 'c5')
  })
  
  observeEvent(input$pcaButton, {
    tot_pcs <- 
    updateSliderInput(session, "x_axis_pc", min = 1,
                      max=dim(pca_data()$x)[2], value = 1, step = 1)
    updateSliderInput(session, "y_axis_pc", min = 1,
                      max=dim(pca_data()$x)[2], value = 2, step = 1)
    updateSelectInput(session, "pcs_color_opts",
                      choices = pcs_color_options(), selected = "Sample_ID")
  })
  
  
  
  #output$reactiveAttempt 
  
  # Perform PCA
  pca_data <- eventReactive(input$pcaButton, {
    if (input$scaleButton == "scaled") {
      scaleBoolean <- TRUE
    } else { # need to make sure this is working right
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

  
  # Test thingy for reactive here!
  output$reactiveAttempt <- renderText({
    paste('input$checkPCASamples are:',dim(pve()))})
  
  # PCs plot
  output$pcs_plot <- renderPlotly({
    p <- autoplot(pca_data(), data = for_plot(), x = input$x_axis_pc, y = input$y_axis_pc, 
             colour = input$pcs_color_opts, alpha = 0.75,
             #colour = NULL,
             main = paste('PC',input$y_axis_pc,' by PC', input$x_axis_pc,
                          ' colored by ',input$pcs_color_opts, sep = "")) +  
      #scale_color_manual(values = cols) + 
      theme(
        axis.line.y = element_line(colour = "black", size=0.5),
        axis.line.x =  element_line(color = "black", size = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))
    if (input$pcs_color_opts == 'Sample_ID') {
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
  
  # Table of selected dataset ----
  output$table <- renderTable({
    pve()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

