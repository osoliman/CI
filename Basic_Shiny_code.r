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
      h2('Preview of the uploaded data'),
      tableOutput("Sample_content"),
      
      h4('mean of the selected outcome'),
      verbatimTextOutput("mean_y"),
      
      h4('Summary of the regression model'),
      verbatimTextOutput("lmSummary"),
      
      h4('Mean Outcome in Treated group'),
      verbatimTextOutput("Mean_treated"),
      
      h4('Mean Outcome in Control (untreated) group'),
      verbatimTextOutput("Mean_control")
  )

))


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
        mean(df1, na.rm = TRUE)
      })

newdf2 <- reactive({
    req(myData())
    newdf <- myData()
    #Creating a new dataframe based on the input, then use that new dataset to run the model
    y <- newdf[,input$y_input]
    x <- newdf[,input$x_input]
    Treatment <- newdf[,input$t_input]
    newdf2 <- data.frame(y,x,Treatment)
    return(newdf2)
    })

lmModel <- reactive({
    model <- lm(y ~., data = newdf2(), na.action=na.exclude)
    return(model)
  })

  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())[4]
  })


output$Mean_treated <- renderPrint({
     req(lmModel()) 
     mean(predict(lmModel(), newdata = subset(newdf2(),Treatment == 1)), na.rm = TRUE)
   })

output$Mean_control <- renderPrint({
     req(lmModel()) 
     mean(predict(lmModel(), newdata = subset(newdf2(),Treatment == 0)), na.rm = TRUE)
   })

})

shinyApp(ui = ui, server = server)
