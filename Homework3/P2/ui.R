
# Use a fluid Bootstrap layout
fluidPage(

  # Give the page a title
  titlePanel("Mortality Rates: State vs. Nation"),

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(
      selectInput("state", "State:",        choices=unique(df$State)),
      selectInput("icd", "Cause of Death:", choices=sort(unique(df$ICD.Chapter))),
      hr(),
      helpText("Source: https://wonder.cdc.gov/ucd-icd10.html"),
      helpText("The Underlying Cause of Death database contains mortality and population counts for all U.S. counties"),
      hr(),
      helpText("Crude Death Rate (Crude Rate) is the total number of deaths to residents in a specified geographic area (country, state, county, etc.) divided by the total population 
               for the same geographic area (for a specified time period, usually a calendar year) and multiplied by 100,000")
    ),

    mainPanel(
              fluidRow(
                splitLayout(cellWidths = c("40%", "60%"), 
                            plotlyOutput("plot2"), plotlyOutput("plot1"))
              ))
    

  )
)

#http://rstudio.github.io/shinydashboard/structure.html