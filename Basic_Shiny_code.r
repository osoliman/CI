library(shiny)

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

    selectInput(
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
      verbatimTextOutput("mean_y")
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
    updateSelectInput(
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
        df <- myData()
        df1 <- df[,input$y_input]
        mean(df1)
      })

})

shinyApp(ui = ui, server = server)
