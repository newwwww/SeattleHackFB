library(shiny)
library(shinyTime)
shinyUI(fluidPage(
  # Application title
  titlePanel("Seattle Hack - Smart Street Parking Assistant"),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(id="info",

      textInput("Destination", "Destination", "Facebook Seattle", width = "1000px"),
      dateInput("date", "Arrival Date:", value = "2017-10-07"),
      #textInput("time", "Arrival Time", "18:00"),
      timeInput("time2", "Arrival Time", value = strptime("12:00:00", "%T")),
      verbatimTextOutput("value")),
      
      fluidRow(
        column(6, wellPanel(
          actionButton(inputId = "run", label = "Find Parking", colors = "blue")
          
        ))  ),
 
      width = 10),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      #textOutput('coordinate'),
      #plotOutput("JobDistPlot"),
     dataTableOutput('table')
    
  )
  
)
)
)

