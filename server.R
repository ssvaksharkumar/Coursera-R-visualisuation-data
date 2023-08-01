# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    data <- read.csv("adult.csv")
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x = input$radio_continous)) +
        geom_histogram(bins=30) +  # histogram geom
        labs(title=paste("Trend of",input$radio_continous),y="Number of people")+ # labels
        facet_wrap(~prediction)
    } # facet by prediction
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$radio_continous)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(title=paste("How",input$radio_continous,"value is spread"),y="Number of people",x="Age")+   # labels
        facet_wrap(~prediction)   # facet by prediction
    }
    
  })

  
   #TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    data <- read.csv("adult.csv")
     #Bar chart
    p <- ggplot(df_country(), aes(x = input$radio_categorical)) +
     labs(title=paste("Trend of",input$radio_categorical),y="Number of people",x="Age") + # labels
     theme(axis.text.x= element_text(angle=45),legend.position = "bottom")   # modify theme to change text angle and legend position
    
    if (input$is_stacked) {
      p + geom_bar(fill="prediction")  # add bar geom and use prediction as fill
    }
    else{
      p + 
        geom_bar(fill=input$radio_categorical) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)   # facet by prediction
    }
  })
  
})
