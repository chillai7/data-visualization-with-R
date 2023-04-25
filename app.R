library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)


# plot prototype

ggplot(
        dat,
        aes(x = pid7)
)+
        geom_bar()+
        facet_wrap(~ideo5)+
        xlab('7 Point Party ID, 1 = Very D, 7 = Very R')


#apps setting

ui <- fluidPage(      
        # Application title   
        #titlePanel("Ideology in Congress"),      
        # Sidebar with a slider input for number of bins    
        sidebarLayout(   
                sidebarPanel(   
                        sliderInput("myideo5",   
                                    "Select Five Point Ideology (1 = Very liberal, 5 = Very conservative)",
                                    min = 1,   
                                    max = 5,   
                                    value = 3)  
                ),      
                # Show a plot of the generated distribution   
                mainPanel(   
                        plotOutput("congress_barplot")   
                )   
        )  
) 

server <- function(input, output) {      
        output$congress_barplot <- renderPlot({      
                ggplot(   
                        filter(dat,ideo5==input$myideo5),   
                        #####if the slider was set to the 93, this filter line would   look like:   #filter(dat,Congress==93),   #The value from the slider, assigned to "my_cong" in the UI goes   into input$my_cong in the server function.      
                        aes(x=pid7))+  
                        geom_bar()+   
                        
                        xlab("7 Point Party ID, 1 = Very D, 7 = Very R")   
        })  
}  

# Run the application   
shinyApp(ui = ui, server = server)
