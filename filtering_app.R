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
data <- read.csv("repro_PCA_test.csv")
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
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput(inputId = "checkSamples", label ="Choose samples to display in plots", 
                            choiceNames = samples,
                            choiceValues = samples,
                            selected = samples, inline=TRUE),
        sliderInput("size_filter", "Select nucleus size range", min = min_size,
                    max=max_size, value = c(min_size, max_size)),
        sliderInput("af555cell_filter","Select AF555 cell range", min = min_af,
                    max = max_af, value = c(min_af, max_af)),
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
         plotlyOutput("nucleus_size_histogram"),
         plotlyOutput("af555cell_histogram"),
         plotlyOutput("ft1_hist"),
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
         d <- read.csv("repro_PCA_test.csv")
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
  
   observeEvent(input$checkSamples, {
      nuc_min <- min(sample_data()$Nucleus_Size)
      nuc_max <- max(sample_data()$Nucleus_Size)
      af555_min <- min(data_filtered()$AF555_Cell_Intensity_Average)
      af555_max <- max(data_filtered()$AF555_Cell_Intensity_Average)
      updateSliderInput(session,"size_filter",
                        min = nuc_min, max = nuc_max, value = c(nuc_min, nuc_max),
                           )
      updateSliderInput(session,"af555cell_filter", 
                        min = af555_min, max = af555_max, value = c(af555_min, af555_max))
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
   
   # AF555 histogram
   output$af555cell_histogram <- renderPlotly({
      g <- data_filtered() %>% ggplot(aes(x = AF555_Cell_Intensity_Average)) +
         geom_histogram(aes(),fill = "Red", color = "firebrick", alpha = 0.6) + 
         geom_density(color = 'darkred') +
         labs(title = "Distribution of AF555 average intensity value", 
              y = "Count", xlab = "AF555 average intensity") +
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

