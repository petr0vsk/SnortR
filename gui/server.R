server <- function(input, output) {
  
  output$contents <- DT::renderDT({
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
   #(df[, input$show_vars, drop = FALSE])
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    output$value <- renderPrint({
        req(input$file1)
        str(output$contents)
    })
   
    
    
    
  }) #--renderDT(..
  
}