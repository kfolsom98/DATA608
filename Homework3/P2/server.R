
# Question 2: Often you are asked whether particular States are improving their mortality rates (per cause) 
# faster than, or slower than, the national average. Create a visualization that lets your clients see this 
# for themselves for one cause of death at the time. Keep in mind that the national average should be 
# weighted by the national population. 

# Definition: CRUDE DEATH RATE is the total number of deaths to residents in a specified geographic area (country, state, county, etc.) divided by the total population 
# for the same geographic area (for a specified time period, usually a calendar year) and multiplied by 100,000.

# Define a server for the Shiny app
function(input, output) {
  
  selectedData <- reactive({
    
    state_df <- df %>% filter( ICD.Chapter == input$icd, State == input$state) 
    nation_df <- nation_avg_df %>% filter( ICD.Chapter == input$icd)
    
    selected <- as.data.frame(bind_rows(state_df, nation_df))
    
  })
  
  # Fill in the spot we created for a plot
  output$plot1 <- renderPlotly({
    
 
    ggplotly(ggplot(selectedData(), aes(Year,Crude.Rate,color=State)) + 
            geom_point() +
            geom_smooth(data=selectedData(), aes(Year,Crude.Rate,color=State),method=lm,se=FALSE) +
            labs(x = "Year", y = "Crude Rate") + labs(fill="") + theme(legend.title = element_blank()) + 
            ggtitle(sprintf("Scatterplot - %s vs. National Avg", input$state )) + 
            scale_x_continuous(breaks = seq(1999, 2010, 2), limits = c(1999, 2010)) 
            
    )

  })
  
  output$plot2 <- renderPlotly({


    nation_slope <-  filter(nation_model, ICD.Chapter == input$icd) %>% 
                     select(slope) %>% unlist()

    state_slope  <- filter(state_model, ICD.Chapter == input$icd, State == input$state) %>% 
                    select(slope) %>% unlist()

    plot_ly(  x = "National Avg" , y = nation_slope, type = 'bar', name = 'National Avg') %>%
      add_trace(x = input$state, y = state_slope, name = input$state) %>%
      layout(yaxis = list(title = 'Slope'), barmode = 'group',
             title="Mortality Rate of Change")  
    
  })
  
}




