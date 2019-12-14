# Megan Grout
# 20191211
# CycIF: Filtering dashboard

library(shiny)
library(tidyverse)
library(plotly)

##load the biopics data
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




ui <- fluidPage(
   
   # Application title
   titlePanel("CycIF Workflow: Filtering"),
   
   fileInput("dataFile", "Choose data csv file to upload", accept = ".csv"),
   fluidRow(column(8,
   checkboxGroupInput(inputId = "checkSamples", label ="Choose samples to display in plots", 
                      choiceNames = samples,
                      choiceValues = samples,
                      selected = samples, inline=TRUE)),
   column(4,fluidRow(actionButton("goButton", "Go!")))), 
   fluidRow(
     h4("Choose features to display"),
     sidebarLayout(
       sidebarPanel(
         sliderInput("size_filter", "Select nucleus size range", min = min_size,
                     max=max_size, value = c(min_size, max_size))
       ),
       mainPanel(plotlyOutput("nucleus_size_histogram"))
     )
   ),
   
   sidebarLayout(
     sidebarPanel(
       sliderInput("af555cell_filter","Select AF555 cell range", min = min_af,
                              max = max_af, value = c(min_af, max_af))),
     mainPanel(
       plotlyOutput("af555cell_histogram"))
   ),
   sidebarLayout(
     sidebarPanel(sliderInput("af555cyto_filter","Select AF555 cytoplasm range", min = min_af,
                              max = max_af, value = c(min_af, max_af))),
     mainPanel(plotlyOutput("af555cyto_histogram"))
   ),
  sidebarLayout(
    sidebarPanel(sliderInput("af555nuc_filter","Select AF555 nucleus range", min = min_af,
                             max = max_af, value = c(min_af, max_af))),
    mainPanel(plotlyOutput("af555nuc_histogram"))
  ),
   fluidRow(
     sidebarLayout(
       sidebarPanel(
         selectInput("ft1_choice","Choose feature 1 to plot", 
                                choices = c("Nucleus_Size")),
       sliderInput("ft1_slider","Select feature range", min = 1, max = 10, value = c(1,2))
       ),
       mainPanel(plotlyOutput("ft1_hist"))
     )
   ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft2_choice","Choose feature 2 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft2_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft2_hist"))
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput("ft3_choice","Choose feature 3 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft3_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft3_hist"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft4_choice","Choose feature 4 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft4_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft4_hist"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft5_choice","Choose feature 5 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft5_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft5_hist"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft6_choice","Choose feature 6 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft6_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft6_hist"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft7_choice","Choose feature 7 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft7_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft7_hist"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("ft8_choice","Choose feature 8 to plot", 
                  choices = c("Nucleus_Size")),
      sliderInput("ft8_slider","Select feature range", min = 1, max = 10, value = c(1,2))
    ),
    mainPanel(plotlyOutput("ft8_hist"))
  ),
)


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
                AF555_Cell_Intensity_Average < input$af555cell_filter[2]) %>% 
      # issue with string literal, so couldn't do filter with input$X as column name
      filter(between(.[,input$ft1_choice] , input$ft1_slider[1], input$ft1_slider[2])) %>% 
      filter(between(.[,input$ft2_choice] , input$ft2_slider[1], input$ft2_slider[2])) %>% 
      filter(between(.[,input$ft3_choice] , input$ft3_slider[1], input$ft3_slider[2])) %>% 
      filter(between(.[,input$ft4_choice] , input$ft4_slider[1], input$ft4_slider[2])) %>% 
      filter(between(.[,input$ft5_choice] , input$ft5_slider[1], input$ft5_slider[2])) %>% 
      filter(between(.[,input$ft6_choice] , input$ft6_slider[1], input$ft6_slider[2])) %>% 
      filter(between(.[,input$ft7_choice] , input$ft7_slider[1], input$ft7_slider[2])) %>% 
      filter(between(.[,input$ft8_choice] , input$ft8_slider[1], input$ft8_slider[2])) 
   
  })

   sample_data <- eventReactive(input$goButton,{
      data() %>% filter(Sample_ID %in% input$checkSamples)
   })
   
   samples <- eventReactive(data(),{
     #req(input$dataFile)
     data() %>% select(Sample_ID) %>% unique() %>% 
       deframe() %>% as.character() %>% str_sort(numeric=TRUE)
   })
   
   observeEvent(samples(),{
     updateCheckboxGroupInput(session, 
                              inputId = "checkSamples", 
                              choiceNames = samples(), # should be set once we have data
                              choiceValues = samples(), # should be set once we have data
                              selected = samples(), inline = TRUE)
   })
   
  
   #### Choose filter options ####

   menu_choices <- eventReactive(data(),{
     names(data()) %>% sort()
   })
   
   observe({
     updateSelectInput(session, "ft1_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft2_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft3_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft4_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft5_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft6_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft7_choice",
                       choices = menu_choices(),
                       selected = NULL)
     updateSelectInput(session, "ft8_choice",
                       choices = menu_choices(),
                       selected = NULL)
   })
   
   ### Update slider ranges
   observe({
     #req(sample_data())
     min <- min(sample_data()$Nucleus_Size)
     max <- max(sample_data()$Nucleus_Size)
     updateSliderInput(session,"size_filter",
                       min = min, max = max, value = c(min, max),
     )
   })
   
   # Reactive for AF555cell min
   #observeEvent(input$checkSamples, {
   #observeEvent(sampledata(),{
    observe({ 
      #req(sample_data())
      min <- min(sample_data()$AF555_Cell_Intensity_Average)
      max <- max(sample_data()$AF555_Cell_Intensity_Average)
      updateSliderInput(session,"af555cell_filter", 
                        min = min, max = max, value = c(min, max))
  })
    
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft1_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft1_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft1_slider", 
                        min = min, max = max, value = c(min, max))
    })
    
    observe({
      min <- sample_data() %>% select(input$ft2_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft2_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft2_slider", 
                        min = min, max = max, value = c(min, max))
    })
    
    observe({
      min <- sample_data() %>% select(input$ft3_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft3_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft3_slider", 
                        min = min, max = max, value = c(min, max))
    })
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft4_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft4_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft4_slider", 
                        min = min, max = max, value = c(min, max))
    })
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft5_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft5_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft5_slider", 
                        min = min, max = max, value = c(min, max))
    })
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft6_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft6_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft6_slider", 
                        min = min, max = max, value = c(min, max))
    })
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft7_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft7_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft7_slider", 
                        min = min, max = max, value = c(min, max))
    })
    observe({
      #input$ft1_choice
      min <- sample_data() %>% select(input$ft8_choice) %>% min() %>% round()#min(sample_data()$ft1_name()) 
      max <- sample_data() %>% select(input$ft8_choice) %>% max() %>% round() #max(sample_data()$ft1_name())
      updateSliderInput(session,"ft8_slider", 
                        min = min, max = max, value = c(min, max))
    })

  
    
   # Nucleus size histogram
   output$nucleus_size_histogram <- renderPlotly({
     #req(data_filtered())
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
     if ("AF555_Cytoplasm_Intensity_Average" %in% names(data_filtered())) {
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
     g}
     else {
       return(NULL)}
   })
   
   # AF555 nuc histogram
   output$af555nuc_histogram <- renderPlotly({
     if ("AF555_Nucleus_Intensity_Average" %in% names(data_filtered())) {
     g <- data_filtered() %>% 
      # ggplot(aes(x = AF555_nuc_Intensity_Average)) +
       ggplot(aes(x = AF555_Nucleus_Intensity_Average)) +
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
     g}
     else{
       return(NULL)
     }
   })
   
   # Ft 1 
   output$ft1_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft1_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft1_choice), 
            y = "Count", xlab = input$ft1_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # Ft 2
   output$ft2_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft2_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft2_choice), 
            y = "Count", xlab = input$ft2_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # Ft 3
   output$ft3_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft3_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft3_choice), 
            y = "Count", xlab = input$ft3_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # Ft 4
   output$ft4_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft4_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft4_choice), 
            y = "Count", xlab = input$ft4_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # Ft 5
   output$ft5_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft5_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft5_choice), 
            y = "Count", xlab = input$ft5_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   
   # Ft 6
   output$ft6_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft6_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft6_choice), 
            y = "Count", xlab = input$ft6_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   # Ft 7
   output$ft7_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft7_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft7_choice), 
            y = "Count", xlab = input$ft2_choice) +
       theme(
         axis.line.y = element_line(colour = "black", size=0.5),
         axis.line.x =  element_line(color = "black", size = 0.5),
         panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"))
     g
   })
   # Ft 8
   output$ft8_hist <- renderPlotly({
     g <- data_filtered() %>% 
       ggplot(aes_string(x = input$ft8_choice)) +
       geom_histogram(aes(),fill = "dodgerblue2", color = "dodgerblue4", alpha = 0.6) + 
       labs(title = paste0("Distribution of ",input$ft8_choice), 
            y = "Count", xlab = input$ft8_choice) +
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

