shinyServer(function(input, output,session) {
  
  
  authname <- eventReactive(input$visAuthorName, {
    return (input$visAuthorName)
  })
  
  diseaseState <- eventReactive(input$TA, {
    return (input$TA)
  })
  
  pubs_region <- eventReactive(input$radio_pubs_region, {
    
    return (input$radio_pubs_region)
  })
  
  collab_metric_type <- eventReactive(input$radio_collab_metric, {
    
    return (input$radio_collab_metric)
  })
  
  cited_metric_type <- eventReactive(input$radio_cited_metric, {
    
    return (input$radio_cited_metric)
  })
  
  selectedPubs <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis' = mf_publications_df,
           'Polycythemia vera' = pv_publications_df,
           'GVHD' =              gvhd_publications_df) 
    
  })
  
  
  # return authors df based on user selection
  selectedAuthors <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis'  =    mf_authors_df,
           'Polycythemia vera' = pv_authors_df,
           'GVHD' =              gvhd_authors_df)
    
  })
  
  
  
  # # return authors df based on user selection
  selectedAuthorsList <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis'  =    mf_AuthorList,
           'Polycythemia vera' = pv_AuthorList,
           'GVHD' =              gvhd_AuthorList)
    
  })
  
  observeEvent(input$TA, {
    
    # Can also set the label and select items
    updateSelectInput(session, "visAuthorName",
                      choices = selectedAuthorsList(),
                      selected = selectedAuthorsList()[1,]
    )
  })
  
  # return authors df based on user selection
  selectedCollabGraph <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis'  =    mf_collab_graph1,
           'Polycythemia vera' = pv_collab_graph1,
           'GVHD' =              gvhd_collab_graph1)
    
  })
  
  # return authors df based on user selection
  selectedCollabMetrics <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis'  =    mf_collab_metrics,
           'Polycythemia vera' = pv_collab_metrics,
           'GVHD' =              gvhd_collab_metrics)
    
  })
  
  # return citation network graph based on user selection
  selectedCitedGraph <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis'  =    mf_citation_graph2,
           'Polycythemia vera' = pv_citation_graph2,
           'GVHD' =              gvhd_citation_graph2)
    
  })
  
  # return citation network graph based on user selection
  selectedCitedMetrics <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis' =     mf_citation_metrics,
           'Polycythemia vera' = pv_citation_metrics,
           'GVHD' =              gvhd_citation_metrics)
    
  })
  
  # return citation network graph based on user selection
  selectedCitedUnigrams <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis' =     mf_top20_unigram,
           'Polycythemia vera' = pv_top20_unigram,
           'GVHD' =              gvhd_top20_unigram)
    
  })
  
  # return citation network graph based on user selection
  selectedCitedUnigramsYear <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis' =     mf_top20_unigram_year,
           'Polycythemia vera' = pv_top20_unigram_year,
           'GVHD' =              gvhd_top20_unigram_year)
    
  })
  
  
  # return citation network graph based on user selection
  selectedCitedBigrams <- eventReactive(input$TA, {
    
    switch(input$TA,
           'Myelofibrosis' =     mf_top20_bigram,
           'Polycythemia vera' = pv_top20_bigram,
           'GVHD' =              gvhd_top20_bigram)
    
  })
  
  
  
  # observeEvent(input$obsEventButton,{
  #   output$oText <- renderText({ runif(1) })
  #   
  # })
  # 
  observeEvent(input$visAuthorName,{
    output$oText <- renderText({ paste("Connections for ", input$visAuthorName) })
    
  })
  
  
  
  
  formulaText <- reactive({
    paste("Variable:", input$PhysicLayoutId)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    paste(someName)
  })
  
  #someName <- observeEvent(input$PhysicLayoutId, { return(input$PhysicLayoutId) })
  
  
  output$TA <- renderUI ({
    inputPanel(
      selectInput(
        "TA", "Select A Disease State", DiseaseStateChoice, multiple = FALSE, selected = "Myelofibrosis"
      )
    )
  })
  
  
  
  output$CollabBubblePlot <- renderBubbles({
    
    metric_type <- collab_metric_type()
    
    if (metric_type == "Most Connections") {
      selectedCollabMetrics() %>% select(AuthorName, deg) %>% 
        rename(metric_type = deg) %>% arrange(desc(metric_type)) %>% head(100) -> df
    }
    
    if (metric_type == "Most Influence") {
      
      selectedCollabMetrics() %>% select(AuthorName, eig) %>% 
        rename(metric_type = eig) %>%  arrange(desc(metric_type)) %>% head(100) -> df
    }
    
    bubbles(df$metric_type, df$AuthorName, key = df$AuthorName, color = rainbow(100, alpha=NULL)[sample(20)])
    
  })
  
  
  output$global_map <- renderGvis({
    
    t <- selectedPubs() %>% group_by(Country) %>% summarise(Total = n())
    gvisGeoChart(t,locationvar='Country',colorvar='Total',
                 options=list(dataMode='regions',
                              width=500,
                              height=300))
    
  })
  
  
  
  
  output$topAuthorsCited <- renderTable({  # pkgData()
    
    selectedPubs() %>% dplyr::filter(Citations > 0) %>% 
      group_by(LeadAuthorName) %>% summarise(n = sum(Citations)) %>%
      #tally() %>%
      arrange(desc(n)) %>%
      select("Author" = LeadAuthorName, "# Citations" = n) %>%
      as.data.frame() %>%
      head(5)
  }, digits = 0)
  
  
  output$topAuthorsCollaboration <- renderTable({  
    selectedCollabMetrics() %>%  
      arrange(desc(deg)) %>%
      select("Author" = AuthorName, "Collaborations" = deg) %>%
      as.data.frame() %>%
      head(5)
  }, digits = 0)
  
  
  output$JTtoTA <- renderTable({   
    
    pubs <- selectedPubs()
    if (pubs_region() == 'US') { 
      pubs %>% dplyr::filter(Country == "United States") -> data
    }
    else if(pubs_region() == 'Ex-US') { 
      pubs %>% dplyr::filter(Country != "United States") -> data
    }
    else { 
      data <- pubs }
    
    data %>% #mf_publications_df %>% dplyr::filter(Country == "United States") %>%
      group_by(Year,  MedlineTA, JournalTitle) %>% dplyr::summarise(Total = n()) %>%
      dplyr::arrange(Year, desc(Total)) %>% as.data.frame() %>% 
      head(10) %>% select (MedlineTA, JournalTitle)  %>% arrange(MedlineTA) %>%
      rename("Journal Abbr" = MedlineTA,
             "Journal Title" = JournalTitle) 
    
    
    
  }, digits = 1)
  
  # visNetwork
  
  output$network <- renderVisNetwork({
    
    val <- authname()
    
    if (length(val) == 0) {return()}
    
    net <- selectedCitedGraph() # mf_citation_graph2
    
    
    # create a subgraph based on the selected author# 
    g_sub <- induced.subgraph(graph=net,vids=unlist(neighborhood(graph=net,order=1,nodes=val)))  #
    
    g_sub <- simplify(g_sub)
    
    
    ##graphe <- visIgraph(g_sub, idToLabel = TRUE, layout = "layout_in_circle")  
    
    graphe <-  visIgraph(g_sub,idToLabel = TRUE) %>% visNodes(fixed = T) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 1)      
    
    visNetwork::visOptions(graph= graphe, manipulation = FALSE,
                           nodesIdSelection = TRUE,
                           highlightNearest = TRUE )
    
  })
  
  
  output$sentimentChart <- renderChart({
    
    selectedPubs() %>% filter(Year < 2017) %>% 
      mutate(Date =  ymd(parse_date_time(paste0(Year), orders="y")) ) %>%  #, Month)
      select(Date, anger:positive) %>%
      gather(variable, "value", -Date)  %>% 
      group_by(Date, variable) %>% summarise(total = sum(value)) -> data
    
    p10 <- nPlot(total ~ Date , group =  'variable', data = data, 
                 type = 'stackedAreaChart', id = 'chart')
    #('%b %Y')
    p10$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
    
    p10$xAxis( axisLabel = "Year of Publication", width = 60)
    p10$yAxis( axisLabel = "Aggregate Sentiment Score", width = 60 )
    p10$set(dom =  'sentimentChart' )
    p10
    
    
    
  }) 
  
  output$cited_connections_chart<- renderChart({
    
    
    data <- switch(cited_metric_type(),
                   "Most Cited" = selectedCitedMetrics() %>% 
                     dplyr::arrange(desc(deg)) %>% rename(metric_type = deg),
                   "Most Influential" = selectedCitedMetrics() %>% 
                     dplyr::arrange(desc(eig)) %>% rename(metric_type = eig))
    
    #selectedCitedMetrics() %>% dplyr::arrange(desc(deg)) %>% 
    data %>% 
      dplyr::select(AuthorName, metric_type) %>% 
      dplyr::slice(1:20) -> cnetwork
    
    n1 <- nPlot(metric_type ~ AuthorName, data = cnetwork, type="discreteBarChart")
    n1$chart(color = "#! function(d){ return 'blue'} !#")
    #n1$chart(reduceXTicks = FALSE)
    #n1$xAxis(staggerLabels = TRUE)
    n1$chart(color = c('red', 'blue', 'purple', 'brown', 'orange', '#594c26', 'green'))
    n1$xAxis(rotateLabels=-45)
    n1$set(dom =  'cited_connections_chart' )
    
    return (n1)
    
  }) 
  
  output$year_trend_chart <- renderChart2({
    
    selectedPubs() %>% 
      mutate(Region = ifelse(Country == 'United States', 'US', 'Ex-US')) %>%
      group_by(Year, Region) %>% 
      tally() %>% filter(Year < 2017) -> data
    
    h1 <- hPlot(x = "Year", y = "n", group = "Region", data = data)
    h1$yAxis(title = list(text = "Number oF Articles Published"), #min = 100, max = 300, 
             tickInterval = 50)
    
    
    h1$xAxis(title = list(text = "Year of Publication"))
    
    h1$legend(verticalAlign = "top", align = "right", layout = "vertical", title = list(text = "Region"))
    h1$plotOptions(series = list(lineWidth = 3))
    h1$chart(width = 400, height = 300)
    
    return (h1)
    
  })
  
  
  
  output$pubsBarChart1<- renderChart({
    
    
    selectedPubs()  %>% dplyr::group_by(Year, Region) %>% 
      summarise(Total = n()) %>%
      dplyr::filter(Region != 'Africa') -> data 
    # for stacking to work, need equal numbers in each group
    
    p6 <-  nPlot(Total ~ Year , group = 'Region',  data = data, 
                 stacked = 'Stacked',
                 type = "multiBarChart", dom = 'pubsBarChart1', width = 800) 
    
    
    p6$yAxis( axisLabel = "Total Publications", width = 60)
    p6$xAxis( axisLabel = "Year of Publication", width = 60 )
    p6$set(dom =  'pubsBarChart1' )
    
    return(p6)
    
  })
  
  
  
  output$USPubsbyJournal <- renderChart2 ({  
    
    title <- c()
    
    pubs <- selectedPubs() 
    
    if (pubs_region() == 'US') { 
      pubs %>% dplyr::filter(Country == "United States") -> data
      title <- "Top Journals Published within the United States"
    }
    else if(pubs_region() == 'Ex-US') { 
      pubs %>% dplyr::filter(Country != "United States") -> data
      title <- "Top Journals Published outside the United States"
    }
    else { 
      data <- pubs 
      title <- "Top Journals Published Worldwide"}
    
    
    t <- data %>% 
      group_by(Year,  MedlineTA) %>% dplyr::summarise(Total = n()) %>%
      arrange(Year, desc(Total)) %>% dplyr::filter(row_number() < 11)
    
    
    h <- hPlot(Year ~ Total , data = t, type = "bubble", title = title, 
               subtitle = "2012 - 2017", size = "Total", group = "MedlineTA", dom = 'USPubsbyJournal')
    
    h$xAxis(axisLabel = "Total Publications")
    #h$set(width = 300, height = 200)
    
    h$set(dom =  'USPubsbyJournal' )
    
    
    return (h)
    
    
    # Couldn't get scatterD3 to work with rCharts; 
    # s <- scatterD3(x=t$Year, y=t$JournalTitle, size_var = t$Total, col_var = t$JournalTitle,
    #           lab = t$Total , labels_size = 9, left_margin = 200, legend_width = 0,
    #           ylab = 'Journal', xlab= "Year",
    #           size_range = c(100,1000), point_opacity = 0.7,
    #           transitions = TRUE, #dom =  'scatterPlot',
    #           tooltip_text = tooltips,
    #           caption = list(title = "Caption title",
    #                          subtitle = "Caption subtitle"))
    
  })
  
  
  ## -------------------------------------
  ## Abstract Analysis: Abstract unigrams
  ## -------------------------------------
  output$unigrams<- renderPlotly ({  
    
    #https://plot.ly/ggplot2/user-guide/
    
    p <- ggplot(selectedCitedUnigrams(), aes(x=reorder(word, Pct), y=Pct, fill=word)) + 
      geom_bar(stat="identity") + coord_flip() +
      ylab('% Frequency') + xlab('Words (bigrams)') +
      guides(fill=FALSE) # +  ggtitle('Most Common Words')
    
    # p +   theme(legend.position="none") %>% hide_colorbar() , showscale = FALSE
    p <- ggplotly(p)  #showscale = FALSE
    
    #p <- plotly_build(p)
    #str(p)
    #str(p$layout)
    #p$layout$legend$showlegend <- 'FALSE'
    #p
    return (p)
    
  })
  
  ## ---------------------------------
  ## Abstract bigrams 
  ## ---------------------------------
  output$bigrams<- renderPlotly ({  
    
    mf_top20_bigram
    p <- ggplot(selectedCitedBigrams(), aes(x=reorder(word, Pct), y=Pct, fill=word)) +
      geom_bar(stat="identity") + coord_flip() +
      ylab('% Frequency') +  guides(fill=FALSE) +   xlab('') 
    ggtitle('Most Common Bigrams')
    
    ggplotly(p)
    
  })
  
  ## ---------------------------------
  ## Abstract comomon word trend
  ## ---------------------------------
  output$abstract_word_trend <- renderPlotly ({  
    
    #mf_top20_unigram_year 
    
    selectedCitedUnigramsYear() %>% filter(Year != 2017) %>% 
      ggplot( aes(x = Year, y = Pct)) + geom_point() +
      stat_smooth(method = "lm", se = FALSE) + facet_wrap(~Keyword) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> p
    
    ggplotly(p)
    
  })
  
  output$exUSPubsbyJournal <- renderPlotly ({  
    
    #
    t <- selectedPubs() %>% dplyr::filter(Country != "United States") %>% 
      group_by(Year,  MedlineTA) %>% dplyr::summarise(Total = n()) %>%
      arrange(Year, desc(Total)) %>% dplyr::filter(row_number() < 11)
    
    t$Year <- as.character(t$Year)
    
    
    s <- ggplot(t, aes(Year, MedlineTA, size=Total)) +   
      geom_point(aes(size=Total, color=Total)) + scale_size(range=c(1, 7))
    
    s +  scale_color_gradient(low = 'red', high = 'blue')
    
    #s <- ggplotly(s)
    
    ggplotly(s) %>% 
      layout(width = 600, autosize=TRUE)
    
    #return (ggplotly(s))
    
  })
  # =====================================
  # DataTable Output # 
  # =====================================
  output$dt_sentiment <- DT::renderDataTable({
    
    # probably a better way to assemble
    
    selectedPubs() %>% 
      select(PMID, LeadAuthorName, Year, Month, JournalTitle, anger:trust) %>%
      gather(Sentiment, "Val", -PMID, -LeadAuthorName, -Year, -Month, -JournalTitle) %>%
      group_by(Sentiment) %>% arrange(desc(Val)) %>% slice(1:5) %>% 
      select(Sentiment, PMID, LeadAuthorName, Year, Month, JournalTitle) %>%
      
      # future add: include link to the pubmed article
      
      #data$URL<- sprintf('<a href="https://www.ncbi.nlm.nih.gov/pubmed/?term=%s" target="_blank">Info</a>',data$PMID)
      
      
      DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = FALSE, info=FALSE))
    
  }) 
  
  
  output$mf_top_words <- DT::renderDataTable({
    
    mf_top20_unigram %>%  
      arrange(desc(count)) %>% select(word) %>% slice(1:5) %>%
      
      DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                    colnames= 'Word',
                    options= list( bLengthChange=0, paging = F , searching = FALSE,info=FALSE))
    
    
    # options=list(iDisplayLength=5,                    # initial number of records
    #              aLengthMenu=c(5,10),                  # records/page options
    #              bLengthChange=0,                       # show/hide records per page dropdown
    #              bFilter=0,                                    # global search box on/off
    #              bInfo=0,                                      # information on/off (how many records filtered, etc)
    #              bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
    #              aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size                       
    # )
    
    # tauchartsOutput("leadingGoalscorers_tau")
    
    # DT::datatable(
    #   rownames = F,  
    #   colnames = c('Word', 'Count'),
    #   options = list(
    #                  searching = FALSE,info = FALSE, length
    #     columnDefs = list(list(
    #       className = 'dt-center', targets = 0
    #     )),
    #     pageLength = 10,
    #     lengthMenu = c(5, 10)
    #   )
    #)
    
  })
  
  output$dt_cited <- DT::renderDataTable({
    
    # probably a better way to assemble
    
    selectedCollabMetrics() %>% 
      select(AuthorName, deg) %>% arrange(desc(deg))  %>%
      
      DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                    colnames= c("Author", "# Connections"),
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = FALSE, info=FALSE))
    
  }) 
  
  
  output$pubs_raw <- DT::renderDataTable({
    
    selectedPubs() %>% 
      select(PMID, LeadAuthorName, Year, Month, JournalTitle, MedlineTA, PublicationType:Language, Citations ) %>%
      arrange(desc(Year), desc(Month)) %>%
      
      DT::datatable(rownames=FALSE,
                    class='compact stripe hover row-border', #filter = 'top', 
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = FALSE, info=FALSE,
                                   pageLength = 10, autoWidth = TRUE))
    
  }) 
  
  output$dt_coauthorships <- DT::renderDataTable({
    
    as_data_frame(selectedCollabGraph(), what = "edges") %>%
      
      DT::datatable(rownames=FALSE, colnames = c("Lead Author", "Co-Author"), 
                    class='compact stripe hover row-border', #filter = 'top', 
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = TRUE, info=TRUE,
                                   pageLength = 20, autoWidth = TRUE))
    
  }) 
  
  output$dt_cited_by <- DT::renderDataTable({
    
    as_data_frame(selectedCitedGraph(), what = "edges") %>%
      
      DT::datatable(rownames=FALSE, colnames = c("Author", "Cited By Author"), 
                    class='compact stripe hover row-border', #filter = 'top', 
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = TRUE, info=TRUE,
                                   pageLength = 20, autoWidth = TRUE))
    
  }) 
  
  
  output$author_rank <- DT::renderDataTable({
    
    metric_type <- collab_metric_type()
    
    data <- 
      switch(metric_type,
             'Most Connections' = selectedCollabMetrics() %>% arrange(desc(deg)),
             'Most Influence'   = selectedCollabMetrics() %>% arrange(desc(eig))
      )
    
    data %>% select(AuthorName) %>%
      
      DT::datatable(rownames=T, colnames = "Author Name", 
                    class='compact stripe hover row-border', #filter = 'top', 
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = TRUE, info=TRUE,
                                   pageLength = 20, autoWidth = TRUE))
    
  }) 
  
  
  output$author_cited_by <- DT::renderDataTable ({
    
    authorname <- authname()
    
    if (length(authorname) == 0) {return()}
    
    as_data_frame(selectedCitedGraph(), what = "edges") %>% filter(from == authorname) %>% 
      DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                    colnames= c("Author", "Cited By"),
                    options= list( bLengthChange=0, paging = TRUE, pageLength = 20,
                                   searching = FALSE, info=FALSE))
  }) 
  
  output$author_rank_cited <- DT::renderDataTable({
    
    data <- 
      switch(cited_metric_type(),
             'Most Cited' = selectedCitedMetrics() %>% arrange(desc(deg)),
             'Most Influential'   = selectedCitedMetrics() %>% arrange(desc(eig))
      )
    
    data %>% select(AuthorName) %>%
      
      DT::datatable(rownames=T, colnames = "Author Name", 
                    class='compact stripe hover row-border', #filter = 'top', 
                    options= list( bLengthChange=0, paging = TRUE,
                                   searching = TRUE, info=TRUE,
                                   pageLength = 20, autoWidth = TRUE))
    
  }) 
  
  
  # ## abstract top words using timevis
  output$word_timeline <- renderTimevis({

    selectedCitedUnigramsYear() %>%  group_by(Year) %>%
      dplyr::arrange(desc(Count)) %>% dplyr::top_n(5, Count) %>%
      mutate(start = paste0(Year, '-01-01'),
             end   = paste0(Year, '-12-31'),
             content = Keyword)  %>% as.data.frame() -> d

    d$id <- rownames(d)


    timevis(d)

  })
  
  output$headline <- renderValueBox({
    

    valueBox(
      value = tags$p(HTML(sprintf('PubMed Results for: <br><center>%s</center>', diseaseState())), 
                          
                          style = "font-size: 18pt;"), 
      
      subtitle = HTML("<br>Source: PubMed 2012 - 2017 <br> As of: May 12, 2017"),
      icon = icon("area-chart"),
      color =   "blue"
    )
  })
  
  
  output$pubsBox <- renderInfoBox({
    infoBox("Total Publications", formatC(count(selectedPubs()), format="d", big.mark=","), 
            icon = icon("book"), color = "orange")
    #color = "orange"
  })
  
  output$JournalsBox <- renderInfoBox({
    infoBox("Distinct Journals", 
            length(unique(selectedPubs()$MedlineTA)), icon = icon("bookmark"), 
            color = "orange")
  })
  
  
  output$leadAuthorsBox <- renderInfoBox({
    infoBox(
      "Lead Authors", formatC(
                         nrow(unique(subset(selectedAuthors(), order==1, select=c("LastName", "ForeName")))),
                      format="d", big.mark=","), 

      icon = icon("institution"),color = "light-blue" 
    )
    
  })
  
  output$coAuthorsBox <- renderInfoBox({
    infoBox(
      "Co-Authors", formatC(
                       nrow(unique(subset(selectedAuthors(), order>1, select=c("LastName", "ForeName")))),
                       format="d", big.mark=","), 
            icon = icon("handshake-o"),color = "light-blue" 
    )
    
  })
})