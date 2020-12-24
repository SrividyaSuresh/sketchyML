library(shiny)

ui<-fluidPage(
  titlePanel("My first Shiny Application"),
  sidebarLayout(
    mainPanel(
      selectInput("select1", "months:",c("jan","feb","nov","dec"),"nov")
      ,
      textOutput(outputId = "txtout2"),
      plotOutput("plt1"),
      # dataTableOutput("data")
      tableOutput("table1"),
      textOutput("txt1"),
      dataTableOutput("table2")
    ),
    sidebarPanel(
      sliderInput(inputId = "sld1", label = "No of months",min = 1,max = 12, value = 6)
      # ,
      # selectInput("select2","Select the religion", choices = hr_data$religion)
      ,
      fileInput("f1","Upload the file here",buttonLabel = "Browse",multiple = TRUE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      numericInput("num1","enter the number",value = 50),
      numericInput("num2","enter the number",value = 50),
      actionButton(inputId = "actbtn","calculate"),
      radioButtons("radio1","enter something",c(1,2,3,4)),
      checkboxGroupInput("check","check it out", c("a","b","c")),
      textOutput(outputId = "txtout1"),
      textInput("title","Enter the title")
      
                ),
  position = c("left","right"),
  fluid = TRUE
                )
)

server1 <- function(input,output){
  output$txtout2 = renderText({print(input$select1)})
  output$txtout1 = renderText({print(input$sld1)})
  output$plt1 = renderPlot({hist(rnorm(input$num1),main=input$title)})
  # output$table1 = renderTable({
  #   religionfilter <- subset(hr_data,hr_data$religion==input$select2)})
  calculate <- eventReactive(input$actbtn,{input$num1+input$num2})
  output$txt1 <- renderText({calculate()})
  df <- reactive(read.csv(input$f1$datapath))
  output$table2 <- renderDataTable({req(input$f1) return(df())})
  }

shinyApp( ui = ui,  server = server1)
