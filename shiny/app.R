#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Return sweep model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
     
     
      sidebarPanel(

       
       sidebarPanel(
         sliderInput("S_X0",  label= "Intercept of systematic error (in deg):",
                     min= 0, max= 0.2, step= 0.01, value= 0.07712),
       width = 6),
       
       sidebarPanel(
         sliderInput("S_X",  label= "Slope of systematic error (in deg):",
                     min= -0.3, max= 0.3, step= 0.01, value= -0.07710),
         width = 6),
       
       sidebarPanel(
         sliderInput("R_X0",  label= "Intercept of random error SD:",
                     min= 0, max= 2, step= 0.01, value= 0.11840),
         width = 6),
       
       sidebarPanel(
         sliderInput("R_X",  label= "Slope of random error SD:",
                     min= 0, max= 2, step= 0.01, value= 0.08248),
         width = 6),
       
       
       # Define the sidebar with one input
       sidebarPanel(
         
         # Select dataset:
         selectInput("dataset",  label= "Dataset:", multiple= F,
                     choices= c("Font size", "Provo", "Oz", "Comprehension", "Abbreviations")),
       width = 6)
       
       
       
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

   
   output$distPlot <- renderPlot({
     
     # load relevant dataset based on user choice
     if(input$dataset== "Font size"){
       filename<-  "datasets/Font_size.csv"
     }
     if(input$dataset== "Provo"){
       filename<-  "datasets/Provo.csv"
     }
     
     if(input$dataset== "Oz"){
       filename<-  "datasets/Oz.csv"
     }
     
     if(input$dataset== "Comprehension"){
       filename<-  "datasets/Comprehension.csv"
     }
     
     if(input$dataset== "Abbreviations"){
       filename<-  "datasets/Abbreviations.csv"
     }
     
     
     dat<- read.csv(file = filename, header = T)
     
     
     # # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
       hist(dat$LandStartVA, breaks = 50, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

