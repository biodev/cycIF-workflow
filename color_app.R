library(shiny)
library(shinyjs)
library(reshape2)
library(ggplot2)
library(colourpicker)

# From this very helpful stakoverflow question:
# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot

dat <- data.frame(matrix(rnorm(60, 2, 3), ncol=3))
dat <- melt(dat)

# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

runApp(shinyApp(
  ui = fluidPage(
    selectizeInput("select", "Select:", 
                   choices = as.list(levels(dat$variable)), 
                   selected = "X1", 
                   multiple = TRUE), 
    uiOutput('myPanel'),
    plotOutput("plot"),
    downloadButton('downloadplot', label = 'Download Plot'),
    actionButton("reset", "Default colours", icon = icon("undo"))
  ),
  server = function(input, output, session) {
    
    output$myPanel <- renderUI({ 
      lev <- sort(unique(input$select)) # sorting so that "things" are unambigious
      cols <- gg_fill_hue(length(lev))
      
      # New IDs "colX1" so that it partly coincide with input$select...
      lapply(seq_along(lev), function(i) {
        colourInput(inputId = paste0("col", lev[i]),
                    label = paste0("Choose colour for ", lev[i]), 
                    value = cols[i]
        )        
      })
    })
    
    
    output$plot <- renderPlot({
      cols <- paste0("c(", paste0("input$col", sort(input$select), collapse = ", "), ")")
      # print(cols)
      cols <- eval(parse(text = cols))
      # print(cols)
      
      # To prevent errors
      req(length(cols) == length(input$select))
      
      dat <- dat[dat$variable %in% input$select, ]
      ggplot(dat, aes(x = variable, y = value, fill = variable)) + 
        geom_boxplot() +
        scale_fill_manual(values = cols)
      
    })
    
    
    observeEvent(input$reset, {
      # Problem: dynamic number of widgets
      # - lapply, do.call
      
      lev <- sort(unique(input$select))
      cols <- gg_fill_hue(length(lev))
      
      lapply(seq_along(lev), function(i) {
        do.call(what = "updateColourInput",
                args = list(
                  session = session,
                  inputId = paste0("col", lev[i]),
                  value = cols[i]
                )
        )
      })
    })
    
    
    
    
    output$downloadplot <- downloadHandler(
      filename = "plot.pdf",
      content = function(file) {
        pdf(file, width = 12, height = 6.3)
        print(testplot())
        dev.off()
      })
  }
))