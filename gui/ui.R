ui <- fluidPage(
  tags$head(
    # подключим свою таблицу стилей
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  ui <- fluidPage(
    # Заголовок
    titlePanel("Анализ и визуализация параметров трафика TCP/IP"),
    # Боковая панель для выбора файла для анализа
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Выбери CSV или TXT файл",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Ввод: если файл имеет заголовок ----
        checkboxInput("header", "Header", TRUE),
        
        # Ввод: выбор разделителя ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        ###
        # Ввод выбор переноса ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        # Вывод: всю талбилцу или только head ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "all")
      ),
      #
      mainPanel(
        tabsetPanel(
          tabPanel("Общая статитстика", helpText("Здесь отображается содержимое трафика"),
                                       h3("Трафик"),
                                       # вывод файла/таблицы с трафиком
                                       #tableOutput("contents")
                                       DT::DTOutput("contents")
                                       ),
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
      
        
      ) #  mainPanel
    ),
  fluidRow(
    column(12, 
           h3("Что-то еще на всю ширину")
           # вывод файла/таблицы с трафиком
           #tableOutput("contents")
           #DT::DTOutput("contents")
    ))
  )#


