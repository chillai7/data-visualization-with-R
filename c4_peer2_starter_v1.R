library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

#####Make your app

ui <- navbarPage(
        title = 'My Application',
        tabPanel('Page 1',
                 sliderInput("myideo5",   
                             "Select Five Point Ideology (1 = Very liberal, 5 = Very conservative)",
                             min = 1,   
                             max = 5,   
                             value = 3),
                 tabsetPanel(
                         tabPanel('Tab 1', plotOutput("congress_barplot") ),
                         tabPanel('Tab 2', plotOutput("trump_barplot"))
                 )
                 
        ),
        tabPanel('Page 2',
                 sidebarLayout(
                         sidebarPanel(
                                 checkboxGroupInput("gender", label = "Select gender", 
                                                    choices = list("1" = 1, "2" = 2),
                                                   )
                         ),
                         plotlyOutput("scatter")
                 ),
                 
                 mainPanel=mainPanel(plotlyOutput("scatter"))),
                         
        tabPanel('Page 3',
                 sidebarLayout(
                         sidebarPanel(
                                 selectInput(inputId = 'region',
                                             'Select Region',
                                             choices = c(1, 2, 3,4),
                                             multiple = TRUE)),
                         
                         mainPanel(dataTableOutput("table", height=500)
                                   
                         )
                 )
        )
)
                 
        
                 
        

                 
                 

        
        
        
        
        
server<-function(input,output){
        
        output$congress_barplot <- renderPlot({      
                ggplot(   
                        filter(dat,ideo5==input$myideo5),
                        aes(x=pid7))+  
                        geom_bar()+   
                        
                        xlab("7 Point Party ID, 1 = Very D, 7 = Very R")   
        })  
        
        output$trump_barplot <- renderPlot({      
                ggplot(   
                        filter(dat,ideo5==input$myideo5),
                        aes(x=CC18_308a))+  
                        geom_bar()+   
                        
                        xlab("Trump Support")
        })
                
                
        output$scatter<-renderPlotly({
                
                ggplot(filter(dat,gender %in% input$gender),aes(x=educ,y=pid7))+
                        geom_jitter()+
                        geom_smooth(method = "lm")+
                        geom_point()
        })
        
                
                
                
                
         output$table <- renderDataTable(
                 filter(dat,region %in% input$region))
         
         
                
                
                
        }  
        
        
        
        
  
  #####Hint: when you make the data table on page 3, you may need to adjust the height argument in the dataTableOutput function. Try a value of height=500
  
 

shinyApp(ui = ui,server = server)