# Megan Grout
# 20191211
# CycIF: Filtering dashboard

library(shiny)
library(tidyverse)
library(fivethirtyeight)
library(plotly)

##load the biopics data
#data(biopics)
#biopics <- biopics %>% filter(!is.na(box_office))
data <- read.csv("../r_shiny/repro_PCA_test.csv")
data <- data %>% mutate('AF555_Cell_Intensity_Average' = round(AF555_Cell_Intensity_Average))

samples <-  data %>% select(Sample_ID) %>% unique() %>% 
   deframe() %>% as.character() %>% str_sort(numeric=TRUE)


## Filtering values ##
# Nucleus size
min_size <- min(data$Nucleus_Size)
max_size <- max(data$Nucleus_Size)
# AF 555
min_af <- min(data$AF555_Cell_Intensity_Average)
max_af <- max(data$AF555_Cell_Intensity_Average)
# others...



# Define UI for application that plots 
ui <- fluidPage(
   
   # Application title
   titlePanel("CycIF Workflow: Filtering"),
   
   fileInput("dataFile", "Choose data csv file to upload", accept = ".csv"),
   # change this...
   checkboxGroupInput(inputId = "checkSamples", label ="Choose samples to display in plots", 
                      choiceNames = samples,
                      choiceValues = samples,
                      selected = samples, inline=TRUE),
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(5, sliderInput("size_filter", "Select nucleus size range", min = min_size,
                           max=max_size, value = c(min_size, max_size))),
     column(7, plotlyOutput("nucleus_size_histogram"))
   ),
   fluidRow(
     column(5, sliderInput("af555cell_filter","Select AF555 cell range", min = min_af,
                           max = max_af, value = c(min_af, max_af))),
     column(7, plotlyOutput("af555cell_histogram"))
   ),
   fluidRow(
     column(5, sliderInput("af555cyto_filter","Select AF555 cytoplasm range", min = min_af,
                           max = max_af, value = c(min_af, max_af))),
     column(7, plotlyOutput("af555cyto_histogram"))
   ),
   fluidRow(
     column(5, sliderInput("af555nuc_filter","Select AF555 nucleus range", min = min_af,
                           max = max_af, value = c(min_af, max_af))),
     column(7, plotlyOutput("af555nuc_histogram"))
   ),
   fluidRow(
     sidebarLayout(
       sidebarPanel(
         selectInput("ft1_choice","Choose feature 1 to plot", 
                                choices = NULL, selected = NULL),
       sliderInput("ft1_slider","Select feature range", min = 1, max = 10, value = 1)
       ),
       mainPanel(
         plotlyOutput("ft1_hist")
       )
     )
     
   ),
   sidebarLayout(
      sidebarPanel(
        
        sliderInput("af555nucleus_filter","Select AF555 nucleus range", min = min_af,
                    max = max_af, value = c(min_af, max_af)),
        sliderInput("af555cytoplasm_filter","Select AF555 cytoplasm range", min = min_af,
                    max = max_af, value = c(min_af, max_af)),
        # sliderInput("ft1_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft2_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft3_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft4_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft5_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft6_filter", min = NULL,
        #             max = NULL, value = NULL),
        # sliderInput("ft7_filter", min = NULL,
        #             max = NULL, value = NULL),
        sliderInput("ft8_filter", label = NULL, min = 1,
                    max = 10, value = c(2, 3))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
         
         
         plotlyOutput("ft2_hist"),
         plotlyOutput("ft3_hist"),
         plotlyOutput("ft4_hist"),
         plotlyOutput("ft5_hist"),
         plotlyOutput("ft6_hist"),
         plotlyOutput("ft7_hist"),
         plotlyOutput("ft8_hist")
      )
   )
)

##Server is where all of the computations happen
server <- function(session,input, output) {
  
   data <- reactive({
      inFile <- input$dataFile
      if (is.null(inFile)) {
         d <- read.csv("../r_shiny/repro_PCA_test.csv")
      } else {
         d <- read.csv(inFile$datapath)
      }
      d <- d %>% mutate('AF555_Cell_Intensity_Average' = round(AF555_Cell_Intensity_Average))
      return(d)
   })
   
  data_filtered <- reactive({
    sample_data() %>% filter(Nucleus_Size > input$size_filter[1], 
                    Nucleus_Size < input$size_filter[2]) %>% 
        filter(AF555_Cell_Intensity_Average > input$af555cell_filter[1],
                AF555_Cell_Intensity_Average < input$af555cell_filter[2]) #%>% 
        # filter(Sample_ID %in% input$checkSamples)
   
  })

   sample_data <- eventReactive(input$checkSamples,{
      data() %>% filter(Sample_ID %in% input$checkSamples)
   })
  
   #observeEvent(input$checkSamples, {
   #observeEvent(sampledata(),{
    reactive({ 
     #nuc_min <- min(sample_data()$Nucleus_Size)
      #nuc_max <- max(sample_data()$Nucleus_Size)
      af555_min <- min(data_filtered()$AF555_Cell_Intensity_Average)
      af555_max <- max(data_filtered()$AF555_Cell_Intensity_Average)
      #updateSliderInput(session,"size_filter",
      #                  min = nuc_min, max = nuc_max, value = c(nuc_min, nuc_max),
      #                     )
      updateSliderInput(session,"af555cell_filter", 
                        min = af555_min, max = af555_max, value = c(af555_min, af555_max))
  })
    
    reactive({
      nuc_min <- min(sample_data()$Nucleus_Size)
      nuc_max <- max(sample_data()$Nucleus_Size)
      updateSliderInput(session,"size_filter",
                        min = nuc_min, max = nuc_max, value = c(nuc_min, nuc_max),
      )
    })
  
 
   # Nucleus size histogram
   output$nucleus_size_histogram <- renderPlotly({
      g <- data_filtered() %>% ggplot(aes(x = Nucleus_Size)) +
         geom_histogram(aes(),fill = "Blue", color = "Blue", alpha = 0.6) + 
         geom_density(color = 'dodgerblue4') +
         labs(title = "Distribution of Nucleus sizes", y = "Count", xlab = "Nucleus size") +
         theme(
            axis.line.y = element_line(colour = "black", size=0.5),
            axis.line.x =  element_line(color = "black", size = 0.5),
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.5, linetype = "solid"))
      g
         })
   
   # AF555 cell histogram
   output$af555cell_histogram <- renderPlotly({
      g <- data_filtered() %>% ggplot(aes(x = AF555_Cell_Intensity_Average)) +
         geom_histogram(aes(),fill = "Red", color = "firebrick", alpha = 0.6) + 
         geom_density(color = 'darkred') +
         labs(title = "Distribution of AF555 cell average intensity value", 
              y = "Count", xlab = "AF555 cell average intensity") +
         theme(
            axis.line.y = element_line(colour = "black", size=0.5),
            axis.line.x =  element_line(color = "black", size = 0.5),
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.5, linetype = "solid"))
      g
   })

   # AF555 cyto histogram
   output$af555cyto_histogram <- renderPlotly({
     g <- data_filtered() %>% 
       #ggpot(aes(aes(x = AF555_Cytoplsm_Intensity_Average)) +
       ggplot(aes(x = Rad51_Cell_Intensity_Average)) +
       geom_histogram(aes(),fill = "Green", color = "firebrick", alpha = 0.6) + 
       geom_density(color = 'darkred') +
       labs(title = "FIX - Distribution of AF555 cyto average intensity value", 
            y = "Count", xlab = "AF555 cyto average intensity") +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # AF555 nuc histogram
   output$af555nuc_histogram <- renderPlotly({
     g <- data_filtered() %>% 
      # ggplot(aes(x = AF555_nuc_Intensity_Average)) +
       ggplot(aes(x = TP53_Cell_Intensity_Average)) +
       geom_histogram(aes(),fill = "Green", color = "firebrick", alpha = 0.6) + 
       geom_density(color = 'darkred') +
       labs(title = "FIX - Distribution of AF555 nucleus average intensity value", 
            y = "Count", xlab = "AF555 nucleus average intensity") +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # AF555 nuc histogram
   output$ft1_hist <- renderPlotly({
     g <- data_filtered() %>% 
       # ggplot(aes(x = AF555_nuc_Intensity_Average)) +
       ggplot(aes(x = TP53_Cell_Intensity_Average)) +
       geom_histogram(aes(),fill = "Green", color = "firebrick", alpha = 0.6) + 
       geom_density(color = 'darkred') +
       labs(title = "FIX - Distribution of AF555 nucleus average intensity value", 
            y = "Count", xlab = "AF555 nucleus average intensity") +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
      
}

# Run the application 
shinyApp(ui = ui, server = server)

