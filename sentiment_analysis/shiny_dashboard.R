library(shiny)
# install.packages("shinydashboard")
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "home",icon = icon("dashboard")),
      menuItem("About",tabName = "about", icon = icon("th")),
      menuItem("Word Cloud", tabName = "wc", icon = icon("cloud",lib = "font-awesome")),
      menuItem("Sentiment Analysis", tabName = "sa", icon = icon("analytics",lib = "font-awesome"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       h1("Twitter Sentiment Analysis"))
              ),
              fluidRow(
                column(width = 12,
                       fileInput("f1","Upload the data"),buttonLabel = "Browse",multiple = TRUE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"))
              ),
              fluidRow(
                column(width = 12,
                       dataTableOutput("table1"))
              )
              ),
      tabItem(tabName = "about",
              fluidRow(
                box(width = 8,plotOutput("plot1", height = 250)),
                box(width = 4,title = "Controls",
                    sliderInput("slider","Observations:",min = 1,
                                max = 150, value = 50)
                   )
                      )
             ),
      tabItem(tabName = "wc",
              fluidRow(
                box(width = 8,plotOutput("plot2", width = "100%", height = "400px"))
              )
      )
      )
    )
  )

server <- function(input, output){
  output$plot1 = renderPlot({hist(rnorm(input$slider))})
  df <- reactive({read.csv(input$f1$datapath)})
  output$table1 = renderDataTable({if(is.null(input$f1)) {
                                                          return(NULL)}
                                   else 
                                   {return(df())}})
  # twi_df <- ldply(df, function(x) x$toDataFrame())
  # twi_text <- sapply(df, function(x) x$getText())
  # twi_text <- as.list(df)
  # twi_text <- twi_text[2]
  # twi_text <- unlist(twi_text)
  # twi_text <- reactive({df()$x})
  library(wordcloud)
  tweets2 <- reactive({
    twi_text <- df()$x
    tweets1 <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', " ", twi_text) # retweets
    tweets1 <- gsub('http[^[:blank:]]+', " ", tweets1) # html links
    tweets1 <- gsub('@\\w+', " ", tweets1) # people id
    
    # Creating word corpus and cleaning
    library(tm)
    tweets2 <- Corpus(VectorSource(tweets1))
    tweets2 <- tm_map(tweets2, removePunctuation)
    tweets2 <- tm_map(tweets2, content_transformer(tolower))
    tweets2 <- tm_map(tweets2, removeWords, stopwords("english"))
    tweets2 <- tm_map(tweets2, stripWhitespace)
    # sentence2 <- unique(sentence2)
    # pal <- brewer.pal(8, "Dark2")
    # wordcloud
    
  })   
  
  pal <- brewer.pal(8, "Dark2")
  wc = reactive({wordcloud(tweets2(), min.freq = 100, max.freq = 200, width = 1000, 
                           height = 1000, random.order = FALSE, color = pal)})
  output$plot2 = renderPlot({wc()})
  
  }


shinyApp(ui, server)