
# Question 1: As a researcher, you frequently compare mortality rates from particular causes across different States. 
# You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause 
# (for example, Neoplasms, which are effectively cancers). 
# Create a visualization that allows you to rank States by crude mortality for each cause of death

#Definition: CRUDE DEATH RATE is the total number of deaths to residents in a specified geographic area (country, state, county, etc.) divided by the total population 
#for the same geographic area (for a specified time period, usually a calendar year) and multiplied by 100,000.

# Define a server for the Shiny app
function(input, output) {
  
  selectedData <- reactive({
    dfSlice <- df %>% filter( ICD.Chapter == input$icd) %>% arrange(desc(Crude.Rate))
  })
  
  # Fill in the spot we created for a plot
  output$plot1 <- renderGvis({
    
   chart <- gvisGeoChart(selectedData(), locationvar="State", colorvar="Crude.Rate",
                        # layout(title="2010 Mortality Rates Per State"),
                         options=list(region="US",
                                 displayMode="regions",
                                 resolution="provinces",
                                 width=600, height=400))
   
   table_data <- selectedData()  %>% 
                 select(State, Deaths, Population, Crude.Rate) %>% arrange(desc(Crude.Rate)) %>% top_n(20)
   
   table <- gvisTable(table_data, #layout(title="Top 20 States"), 
                           options=list(width=280, height=300))
   
   GT <- gvisMerge(chart,table, horizontal=TRUE) 
   
    
  })
  

  
}




