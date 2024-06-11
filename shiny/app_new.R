

# Shiny app for Return-sweep model:
# Martin Vasilev, 2020 (duh)


library(shiny)
library(shinydashboard)
library(ggplot2)

widget_style <-
  "display: inline-block;
  display: flex;
justify-content: space-between;
  align-items: center;
  vertical-align: text-top;
  width: 60%;
  padding: 1%;
  border: solid;
  border-width: 1px;
  border-radius: 4px;
  border-color: #CCC;"

ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Return sweeper"),
  dashboardSidebar(disable = T),
  dashboardBody(
    

    fluidRow(
      #column(
      # Parameters:
      box(
        title = "Parameters", width = 3, solidHeader = TRUE, status = "primary", collapsible = T,
        sliderInput("S_X0",  label= "Intercept of systematic error (in deg):",
                    min= 0, max= 0.2, step= 0.01, value= 0.07712),
        sliderInput("S_X",  label= "Slope of systematic error (in deg):",
                    min= -0.3, max= 0.3, step= 0.01, value= -0.07710),
        sliderInput("R_X0",  label= "Intercept of random error SD:",
                    min= 0.01, max= 2, step= 0.01, value= 0.11840),
        sliderInput("R_X",  label= "Slope of random error SD:",
                    min= 0, max= 2, step= 0.01, value= 0.08248),
        sliderInput("pCorr_mu",  label= "Mean of CDF for prob. of making a corrective saccade (in letters):",
                    min= -5, max= 15, step= 0.5, value= 2),
        sliderInput("pCorr_sigma",  label= "SD of CDF for prob. of making a corrective saccade  (in letters):",
                    min= 1, max= 20, step= 0.5, value= 7),
        box(title = "Visualisation", width = 12, collapsible = T, collapsed = T,
            plotOutput("plot1", height = 250)
            )
        ),
      
      tabBox(
        title = "", width = 7,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1",
        tabPanel("Landing position", "Return-sweep landing positions",
                     plotOutput("LandPlot")
                 ),
        tabPanel("Tab2", "Tab content 2",
                 plotOutput("SLPlot"))
      ),
      
      
      
      box(title = "Settings", footer = "Note: parameters have been fit to the 'Font size' data",
          width = 2, solidHeader = F, status = 'primary', collapsible = T, collapsed = T,
          
          selectInput("dataset",  label= "Dataset:", multiple= F,
                      choices= c("Font size (Vasilev et al., 2020)", "Provo (Luke & Christianson, 2018)",
                                 "Oz (Slattery & Vasilev, 2019)", "Comprehension (Slattery & Parker, 2019)", "Abbreviations (Slattery et al., in prep.)")),
          numericInput("nsim", "# of simulation runs:", 5,
                       min = 1, max = 1000)
          ),
      box(title= "Model statistics:", width =2, collapsible = T
#          dataTableOutput('table')
          ),
      
      box(title = "Trial-level results", width = 7, collapsible = T, collapsed = T, 
          div(style = widget_style,
          numericInput("item_num", "Item #:", value = 1, min = 1, max = 100, width = 80),
          numericInput("cond_num", "Cond #:", value = 1, min = 1, max = 4, width = 80),
          numericInput("sub_num", "Subject #:", value = 1, min = 1, max = 64, width = 80),
          sliderInput(inputId = "opt.cex",
                      label = "Figure scaling",                            
                      min = 0.5, max = 2, step = 0.1, value = 1.1, width =200)),
          
          plotOutput("Stimulus", height = 200, width = "100%")#,
          #checkboxInput("stim_plotAllFix", "Plot all fixations", value = FALSE, width = NULL)
          )
      


    )
    
  )
  
    
    
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # load model:
  source('models/Return_sweeper1.R')
  
  # load relevant dataset based on user choice
  R_dat<- reactive({
    if(input$dataset== "Font size (Vasilev et al., 2020)"){
      filename<-  "datasets/Font_size.csv"
    }
    if(input$dataset== "Provo (Luke & Christianson, 2018)"){
      filename<-  "datasets/Provo.csv"
    }
    
    if(input$dataset== "Oz (Slattery & Vasilev, 2019)"){
      filename<-  "datasets/Oz.csv"
    }
    
    if(input$dataset== "Comprehension (Slattery & Parker, 2019)"){
      filename<-  "datasets/Comprehension.csv"
    }
    
    if(input$dataset== "Abbreviations (Slattery et al., in prep.)"){
      filename<-  "datasets/Abbreviations.csv"
    }
    
    
    dat<- read.csv(file = filename, header = T)
    
  })
  
   output$LandPlot <- renderPlot({

     dat<- R_dat()
     load("datasets/word_pos_FS.Rda")    
     #sims<- Return_sweeper1(dat, word_pos = word_pos, RS_target = 0, nsim= 10)
     
     # histogram:
     hist(dat$LandStartVA, breaks = 50, col = 'darkgray', border = 'white')
   })
   
   
   output$SLPlot <- renderPlot({
     hist(dat$launchDistVA, breaks = 50, col = 'darkgray', border = 'white')
   })
   
   output$plot1 <- renderPlot({
     
     set.seed(3000)
     xseq<-seq(-10, 50, .1)
     densities<-dnorm(xseq, 0,1)
     cumulative<-pnorm(xseq, input$pCorr_mu, input$pCorr_sigma)
     plot(xseq, cumulative, col="darkorange", xlab="Distance to target (letters)", ylab="Cumulative Probability",type="l",
          lwd=2, cex=2, main="Corrective saccade probability",  
          cex.axis=.8, family= 'serif', cex.axis= 1.2, cex.lab=1.4, ylim =c(0,1), xlim= c(-10, 50))
   })
   
   output$Stimulus<- renderPlot({ 
     
     add.alpha <- function(col, alpha=1){
       if(missing(col))
         stop("Please provide a vector of colours.")
       apply(sapply(col, col2rgb)/255, 2,
             function(x)
               rgb(x[1], x[2], x[3], alpha=alpha))
     }
     
       corpus<- read.delim("corpora/font_size.txt")
       item<- input$item_num
       cond<- input$cond_num
       sub<- input$sub_num
       row_n<- which(corpus$item== item & corpus$Cond==cond)
       stim<- paste(corpus$Line1[row_n], "\n", corpus$Line2[row_n], sep= '')
       lines<- unlist(strsplit(stim, "\n"))
       font_size<- ifelse(is.element(cond, c(1,3)), input$opt.cex, input$opt.cex+ input$opt.cex*(0.35*(input$opt.cex/1)))
       
       # get data frame
       dat<- R_dat()
       # trial obs:
       trial<- subset(dat, sub== input$sub_num & item== input$item_num & input$cond_num)
       
       op <- par(mar = rep(0, 4))
       plot(0,type='n', axes=F, ann=F, xlim= c(1, 800), ylim= c(200, 600))
       par(op)
       
       Ys<- c(500, 400) #seq(from = 500, by= 100, to = 500 - length(lines)*100+ 100)
       ppl<- strwidth("a", family = "mono", cex= font_size)
       
       for(i in 1:length(lines)){
         text(x= 10, y= Ys[i], labels= lines[i], adj= 0, family= "mono", cex= font_size)
         #points(x=10+ strwidth(lines[i], family = "mono", cex= font_size), font=2, cex=2, y= Ys[i])
       }
       
       # line-final:
       points(x= 10+trial$prevChar*ppl, y= Ys[1]+20, pch= 16, col= add.alpha("green",0.20), 
              cex= font_size + font_size*(trial$prev_fix_dur /100))
       points(x= 10+trial$prevChar*ppl, y= Ys[1]+20, pch= 16, col= "green", 
              cex= 0.75*font_size)
       text(x = 10+trial$prevChar*ppl, y = Ys[1]+70, labels = 1)
       
       # return-sweep:
       points(x= 10+trial$LandStartLet*ppl, y= Ys[2]+20, pch= 16, col= add.alpha("green",0.20), 
              cex= font_size + font_size*(trial$fix_dur /100))
       points(x= 10+trial$LandStartLet*ppl, y= Ys[2]+20, pch= 16, col= "green", 
              cex= 0.75*font_size)
       text(x = 10+trial$LandStartLet*ppl, y = Ys[2]+70, labels = 2)
       
       # return-sweep saccade:
       arrows(x0 = 10+trial$prevChar*ppl, x1 = 10+trial$LandStartLet*ppl, y0 = Ys[1]+20, y1 = Ys[2]+20,
              length = 0, col= "darkgreen")
       
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

