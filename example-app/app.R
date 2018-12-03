library(shiny)
library(dplyr)
library(DT)
library(ggplot2)

dat <- read.csv2("example1.csv")

ui <- fluidPage(
  titlePanel("Just app"),
  DT::dataTableOutput("results_table"),
  plotOutput("chart")
)

server <- function(input, output) {
  
  proxy = dataTableProxy("results_table")
  
  observeEvent(input[["results_table_cell_edit"]], {
    info <- input[["results_table_cell_edit"]]
    i <- info[["row"]]
    j <- info[["col"]]
    v <- info[["value"]]
    dat[i, j] <<- DT::coerceValue(v, dat[i, j])
    replaceData(proxy, dat, resetPaging = FALSE)  # important
  })
  
  dt_r <- reactive({
    datatable(dat, options = list(pageLength = 24), editable = TRUE) %>% 
      formatStyle(c("Var1", "Var2"),
                  backgroundColor = styleEqual(c("T", "N"), c("indianred", "lightblue")))
  })
  
  output[["results_table"]] <- DT::renderDataTable({
    dt_r()
  })
  
  output[["chart"]] <- renderPlot({
    input[["results_table_cell_edit"]]
    
    ggplot(dat, aes(x = Var1)) +
      geom_bar()
  })
  
}


shinyApp(ui = ui, server = server)
