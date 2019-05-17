# demo app - async

library(tidyverse)
library(shiny)
library(future)
library(promises)
plan(multiprocess)

# clean up previously run results
file.remove(dir(pattern = "data[0-9]*\\.csv", recursive = T))

ui <- basicPage(
  selectInput("slc_dataset", "Dataset",
              choices = c("storms", "population", "economics")),
  numericInput("num_repeat", "Repeat Times",
               value = 100, min = 10, max = 1000),
  actionButton("btn_write", "Write to CSV"),
  actionButton("btn_praise", "Praise me"),
  verbatimTextOutput("txt_praise"),
  wellPanel(
    uiOutput("ui_files_out")
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()
  files_out <- reactiveVal()
  
  observe({
    df <- get(input$slc_dataset)
    n <- as.integer(input$num_repeat)
    
    df_mega <- map_df(seq_len(n), ~ df)
    data(df_mega)
  })
 
  observeEvent(input$btn_write, {
    begTime <- Sys.time()
    df <- data()
    csv_out <- paste0("data", as.integer(begTime), ".csv")
    # print(str(df))
    # print(csv_out)
    showNotification(
      paste0("Begin Time: ", begTime), duration = 6,
      type = "message"
    )

    future({
      write_excel_csv(df, csv_out)
    })

    endTime <- Sys.time()
    showNotification(
      paste0("End Time: ", endTime), duration = 6,
      type = "message"
    )
    
    # Display R processing duration
    showNotification(
      paste0("You had to wait for: ",
             round(difftime(endTime, begTime, units = "secs"), 2), " s"),
      duration = NULL,
      type = "message"
    )
    
  })
  
  # something fun that the user can do while waiting
  nice_words <- eventReactive(input$btn_praise, {
    praise::praise()
  })
  
  output$txt_praise <- renderText({
    nice_words()
  })
  
  # check and display files every 5 sec
  output$ui_files_out <- renderUI({
    invalidateLater(5000, session)
    ui_time <- h4(paste("Now is", Sys.time()))
    df_files <- dir(pattern = "data[0-9]*\\.csv", full.names = T) %>%
      file.info() %>%
      select(size, ctime)
    if (nrow(df_files) > 0) { 
      ui_files <- HTML(htmlTable::htmlTable(df_files))
    } else {
      ui_files <- p("No files yet.")
    }
    tagList(ui_time, ui_files)
  })
   
}

shinyApp(ui, server)