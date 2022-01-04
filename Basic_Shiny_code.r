library(shiny)
library(shinyWidgets)

ui <- shinyUI(fluidPage(

  titlePanel("Test app"),

  sidebarPanel(

    fileInput(
      'file', 
      'Choose a file to upload.'
    ),

    selectInput(
      "y_input",
      label = h5("Select Outcome variable"),
      ""
    ),

    pickerInput(
      "x_input", 
      multiple = TRUE,
      label = h5("Select Confounder(s)"),
      ""
    ),

    selectInput(
      "t_input", 
      label = h5("Select Treatment"),
      ""
    )
  ),

mainPanel (
      h2('Descriptive stats'),
      tableOutput("Sample_content"),
      h4('mean of the selected outcome'),
      verbatimTextOutput("mean_y"),
      h4('Summary of the regression model'),
      verbatimTextOutput("lmSummary")
  )

))


library(shiny)

server <- shinyServer(function(input, output, session) {

  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })

  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read.csv(inFile()$datapath)
    }
  })

  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=names(myData()))

  })

  observe({
    updatePickerInput(
      session,
      "x_input",
      choices=names(myData()))

  })

   observe({
    updateSelectInput(
      session,
      "t_input",
      choices=names(myData()))

  })

output$Sample_content <- renderTable(
        
       head(myData())
     )

output$mean_y <- renderPrint({
        req(myData())
        df <- myData()
        df1 <- df[,input$y_input]
        mean(df1)
      })

lmModel <- reactive({
    req(myData(),input$x_input,input$y_input)
    x <- as.numeric(myData()[[as.name(input$x_input)]])
    y <- as.numeric(myData()[[as.name(input$y_input)]])
    current_formula <- paste0(input$y_input, " ~ ", paste0(input$x_input, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = myData(), na.action=na.exclude)
    return(model)
  })

  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())[4]
  })

})

shinyApp(ui = ui, server = server)
