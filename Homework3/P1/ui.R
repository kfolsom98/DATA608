# Use a fluid Bootstrap layout
fluidPage(

  # Give the page a title
  titlePanel("2010 State Mortality Rates"),

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(
      selectInput("icd", "Cause of Death:",
                  choices=sort(unique(df$ICD.Chapter))),
      hr(),
      helpText("Source: https://wonder.cdc.gov/ucd-icd10.html"),
      helpText("The Underlying Cause of Death database contains mortality and population counts for all U.S. counties"),
      hr(),
      helpText("Crude Death Rate (Crude Rate) is the total number of deaths to residents in a specified geographic area (country, state, county, etc.) divided by the total population 
               for the same geographic area (for a specified time period, usually a calendar year) and multiplied by 100,000")
    ),

    # Create a spot for the charts
    mainPanel("2010 Mortality Rates Per State",
      htmlOutput("plot1")
    )

  )
)
