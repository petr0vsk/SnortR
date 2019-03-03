ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # Application title
  titlePanel("Analysis and visualization of network traffic time sequences"),
  
  
  fluidRow(
    
    column(6,
           fileInput("file1", "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Checkbox if file has header ----
           checkboxInput("header", "Header", TRUE),
           
           # Input: Select separator ----
           radioButtons("sep", "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ","),
           
           # Input: Select quotes ----
           radioButtons("quote", "Quote",
                        choices = c(None = "",
                                    "Double Quote" = '"',
                                    "Single Quote" = "'"),
                        selected = '"'),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Select number of rows to display ----
           radioButtons("disp", "Display",
                        choices = c(Head = "head",
                                    All = "all"),
                        selected = "all")
           
    ),
    
    column(3, verbatimTextOutput("value")),
    
    column(3, 
           h3("Average for file"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets."))
    
     
  ),
  
  fluidRow(
    
    column(12, 
           tabsetPanel(
             tabPanel("Plot 1", helpText("Note1: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.")),
             tabPanel("Plot 2", helpText("Note2: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.")),
             tabPanel("Plot 3", helpText("Note3: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets.Note: help text isn't a true widget,", 
                                       "but it provides an easy way to add text to",
                                       "accompany other widgets."))
           ))
    
    
  ),
  
  fluidRow(
    column(12, 
           h3("DT here"),
           tableOutput("contents")
  )))


# Define server logic required to draw a histogram
