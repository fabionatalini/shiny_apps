library(shiny)

######################################## ui ########################################
my_ui <- fluidPage(
  # Application title
  titlePanel("CorrelaciÃ³n cruzada", windowTitle = "CorrelaciÃ³n cruzada"),
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    # Inputs
    sidebarPanel(
      fileInput("file_1","Upload your dataset"),
      radioButtons("sep", "Column separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      numericInput("lags", "Lags:", 21, min = 1, max = 40, step = 1)
    ),
    # Outputs
    mainPanel(
      uiOutput("select_columns"),
      uiOutput("start_button"),
      tableOutput("resultado"),
      plotOutput("grafico"),
      uiOutput("get_the_result"),
      br()
    )
  )
)

######################################## server ########################################
my_server <- function(input, output) {
  
  # define the main functions for calculations
  import_data <- function(){return(read.table(input$file_1$datapath,header=TRUE,sep=input$sep))}
  main_function <- function(){
    tb <- import_data()
    correlac <- ccf(tb[,input$selected_col[1]], tb[,input$selected_col[2]], input$lags, plot=FALSE)
    mioutput <- data.frame("lag"=correlac$lag, "ccorr"=correlac$acf)
    mioutput$var1 <- input$selected_col[1]
    mioutput$var2 <- input$selected_col[2]
    # mioutput <- mioutput[mioutput$lag>=0,]
    return(mioutput)
  }
  plot_function <- function(){
    tb <- import_data()
    data_for_chart <- ccf(tb[,input$selected_col[1]], tb[,input$selected_col[2]], input$lags, plot=FALSE)
    return(data_for_chart)
  }
  
  # Adding the select_columns input selection
  output$select_columns <- renderUI({
    req(input$file_1, input$sep)
    foo <- import_data()
    selectInput("selected_col", "Select columns", names(foo), multiple=TRUE)
  })

  # Adding the start button
  output$start_button <- renderUI({
    req(input$file_1, input$sep)
    actionButton("start_button", "Do it!")
  })
  
  # reactive expressions
  table_reactive <- eventReactive(input$start_button,{print(main_function())})
  plot_reactive <- eventReactive(input$start_button,{plot_function()})
  
  # show the result
  output$resultado <- renderTable({table_reactive()},digits=5)
  output$grafico <- renderPlot({plot(plot_reactive(),main="")})

  # Add downloadButton to download the result
  output$get_the_result <- renderUI({
    req(input$start_button)
    downloadButton("download_data", "Download the result")
  })
  
  # Handle the downloadButton
  output$download_data <- downloadHandler(
    # set the name of the exported file
    filename = function() {paste0("resultado_",gsub(":","-",gsub(" ","_",Sys.time())),".txt")},
    # content of the exported file
    content = function(file) {
      write.table(main_function(), file, row.names=FALSE, quote=FALSE, sep="\t")
    }
  )
}

######################################## run ########################################
shinyApp(ui = my_ui, server = my_server)
