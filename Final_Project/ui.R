ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "PubMed Explorer"),
  
  dashboardSidebar(

    span(uiOutput("TA"), style = "color:blue"),
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Publications", tabName = "pubs",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "pub_glance", selected = T),
        menuSubItem("Authors", tabName = "pub_authors"),
        menuSubItem("Journals", tabName = "pub_journals"),
        menuSubItem("Abstract Analysis", tabName = "pub_words")
        
      ),
      
      
      menuItem("Info", tabName = "info",icon = icon("info")),
      menuItem("Code",icon = icon("code-fork"),
               href = "https://github.com/kfolsom98/DATA608/tree/master/Final_Project")
    )
  ),
  dashboardBody(tabItems(
    ### Publictaions section
    
    tabItem(
      "pub_glance",
      
      fluidRow(
        column(width = 5, tags$head(tags$style(HTML(".small-box {height: 190px}"))),
               valueBoxOutput("headline", width = 30)),
        
        
        #fluidRow(
        column(width = 3,
               infoBoxOutput("pubsBox", width = 25)),  ##
        column(width = 3,
               infoBoxOutput("leadAuthorsBox", width = 25)),
        column(width = 3,
               infoBoxOutput("JournalsBox", width = 25)),
        column(width = 3,
               infoBoxOutput("coAuthorsBox", width = 25))
        #  )
        ,
        
        
        box(
          width = 5,title = "Five Year Publication Trend", solidHeader = TRUE, status = 'warning',
          collapsible = TRUE, collapsed = FALSE,
          showOutput("year_trend_chart", "highcharts")
          
        )
        
        
        ,
        box(
          width = 3, status = "info",
          title = "Top Cited Authors",
          tableOutput("topAuthorsCited")
        ),
        box(
          width = 3, status = "info",
          title = "Most Collaborative Authors",
          tableOutput("topAuthorsCollaboration")
        ),
        
        
        ##
        
        box(
          width = 6, title = "Country of Publication - Global View", solidHeader = TRUE, status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          
          htmlOutput("global_map", width="15") # good
          
        ),
        
        box(
          width = 6, title = "Most Frequent Abstract Words", solidHeader = TRUE, status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          
          DT::dataTableOutput("abstract_top_words") 
          
        )
        
      )#,
      
      # fluidRow(
      # 
      #    column( 
      #      width = 5,
      #      box(
      #         title = "Most Frequent Abstract Words", solidHeader = TRUE,status = 'success',
      #        collapsible = TRUE, collapsed = FALSE,
      #        renderText("text") #DT::dataTableOutput("mf_top_words")  # good
      #      )
      #    )
      #   
      #   
      # )
      
    ),
    tabItem("pub_journals", style = "overflow-y:scroll; max-height: 600px; position:relative;",
            
            fluidRow(
              tabBox(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "800px",title = "Journals", width =
                  12,side = "left",
                
                
                tabPanel("Top Journals Publishing About This Disease State",
                         
                         fluidRow(
                           column(9,
                                  h5("Select the Region: "),
                                  radioButtons("radio_pubs_region",NULL,c("All","US", 'Ex-US'), inline=T),
                                  
                                  # grVizOutput('diagram', width = "100%", height = "760px") ), #scatterD3Output("scatterPlot")), #ggvisOutput("pl_strikeRate")),
                                  showOutput("USPubsbyJournal", "highcharts")),
                           column(3, 
                                  tableOutput("JTtoTA"))
                         )),
                
                
                
                tabPanel("Publication Trends Geographically",
                         fluidRow(
                           box(
                             width = 22,
                             title = paste0("Regional Trend - Place Of Publication", c()) , 
                             solidHeader = TRUE, status = 'info',
                             collapsible = TRUE, collapsed = FALSE,
                             
                             showOutput("pubsBarChart1", "nvd3")
                             
                           )
                         )),
                
                tabPanel("Raw Data (PubMed)",
                         box(
                           width = 22,
                           title = paste0("Publications Sourced From PubMed", c()) , 
                           solidHeader = TRUE, status = 'info',
                           collapsible = TRUE, collapsed = FALSE,
                           DT::dataTableOutput("pubs_raw")))
                
              )
            )),
    tabItem("pub_words", style = "overflow-y:scroll; max-height: 600px; position:relative;",
            
            fluidRow(
              tabBox(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "600px",title = "Abstract Analysis", width =
                  12,side = "left",
                tabPanel("Abstract Words",
                         fluidRow(
                           column(10, 
                                  
                                  box(
                                    width = 12, title = "Most Common Words", solidHeader = TRUE, status = 'info',
                                    collapsible = TRUE, collapsed = TRUE,
                                    
                                    plotlyOutput("unigrams"))), 
                           
                           column(10, 
                                  
                                  box(
                                    width = 12, title = "Most Common Bigrams", solidHeader = TRUE, status = 'info',
                                    collapsible = TRUE, collapsed = FALSE,
                                    
                                    plotlyOutput("bigrams")))
                           
                           
                         )),
                
                
                tabPanel("Sentiment", #style = "overflow-y:scroll;",
                         # fluidRow(
                         fluidRow(
                           column(10, 
                                  
                                  box( width = 20, title = "Five Year Trend In Abstract Sentiment", 
                                       solidHeader = TRUE, status = 'info',
                                       collapsible = TRUE, collapsed = FALSE,
                                       
                                       showOutput("sentimentChart", "nvd3") ))
                           
                         ),
                         fluidRow(  
                           
                           column(10,
                                  box( width = 20, title = "Top Articles By Sentiment Type", solidHeader = TRUE, 
                                       status = 'info',
                                       collapsible = TRUE, collapsed = TRUE,
                                       
                                       DT::dataTableOutput("dt_sentiment"))
                           )
                         )),
                tabPanel("Word Trend", 
                         box( width = 20, title = "Abstract Top Word Trend", solidHeader = TRUE, status = 'info',
                              collapsible = TRUE, collapsed = FALSE,#
                              
                              timevisOutput("word_timeline"))
                )
                
              )
            )),
    
    tabItem("pub_authors", style = "overflow-y:scroll; max-height: 800px; position:relative;",
            
            fluidRow(
              tabBox(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "1000px",
                title = "Author Networks", width = 12, side = "left",
                
                tabPanel("By Collaborations",
                         fluidRow(
                           column(8, 
                                  
                                  box(
                                    width = 18, status = "info", solidHeader = TRUE,
                                    
                                    title = "Top Authors Collaborating on Publications ",
                                    h5("Select by: "),
                                    radioButtons("radio_collab_metric",NULL,c("Most Connections","Most Influence"), inline=T),
                                    bubblesOutput("CollabBubblePlot", width = "100%", height = 600)
                                    
                                  )),
                           column(4, 
                                  
                                  box(
                                    width = 20, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = TRUE,
                                    title =  "Author Ranking",
                                    DT::dataTableOutput("author_rank")),
                                  
                           #),
                           #column(4, 
                                  
                                  box(
                                    width = 20, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = TRUE,
                                    title = "Author Collaboration Detail",
                                    DT::dataTableOutput("dt_coauthorships"))
                                  
                           )
                         )),
                
                tabPanel("By Citation",
                         fluidRow(
                           column(9,
                                  
                                  box(
                                    width = 20, status = "info", solidHeader = TRUE, 
                                    title = "Cited Authors",
                                    h5("Select by: "),
                                    radioButtons("radio_cited_metric", NULL,c("Most Cited","Most Influential"), inline=T),
                                    showOutput("cited_connections_chart", "nvd3"))
                           ),
                           
                           column(3, 
                                  
                                  box(
                                    width = 20, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = TRUE,
                                    title =  "Author Ranking",
                                    DT::dataTableOutput("author_rank_cited")),
                                  
                           #),
                           #column(3, 
                                  
                                  box(
                                    width = 20, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = TRUE,
                                    title = "Author Collaboration Detail",
                                    DT::dataTableOutput("dt_cited_by"))
                                  
                           )
                         )),
                
                
                tabPanel(" Citation Network View",
                         
                         fluidRow(
                           column(9, 
                                  
                                  box(
                                    width = 18, status = "info", solidHeader = TRUE,
                                    title = "Citation Network",
                                    selectizeInput("visAuthorName",label="Select an author to see their citation network:", 
                                                   choices = mf_AuthorList, 
                                                   selected= mf_AuthorList[1], multiple = FALSE),
                                    
                                    #verbatimTextOutput("oText"),  # show authorname in text output
                                    
                                    visNetworkOutput("network", height = "400px"))
                           ),
                           
                           column(3, 
                                  
                                  box(
                                    width = 20, status = "warning", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = FALSE,
                                    title =  "Authors Citing this Author",
                                    DT::dataTableOutput("author_cited_by"))
                                  
                           ))
                )
              )
            ))
    
    
    
    
    
  ) # tabItems
  ) # body
) # page
