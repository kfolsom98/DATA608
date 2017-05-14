## pubmed_authors.R ##


library(dplyr)
library(igraph)

## Load the publication datasets from GitHub
GitHubPath <- "https://github.com/kfolsom98/DATA608/raw/master/Final_Project/"

mf_pubs_data <- load(url(paste0(GitHubPath, "myelofibrosis.RData")))
pv_pubs_data <- load(url(paste0(GitHubPath, "polycythemia_vera.RData")))
gvhd_pubs_data <- load(url(paste0(GitHubPath, "gvhd.RData")))


## ----------------------------------------------
##
## ----------------------------------------------
get_lead_author_index <- function(authors_df) {
  
  # Author Index #
  authors_df %>% filter(order == 1) %>% 
    select(PMID, AuthorName) -> lead_author_index
  
  return (lead_author_index)
  
}

get_citation_index <- function(pubs_df, lead_authors) {
  
  
  # Citation Index #
  select(pubs_df, PMID, Citations) %>% 
    inner_join(lead_authors, by='PMID') %>%  
    arrange(AuthorName, desc(Citations)) -> citation_index
  
  return (citation_index)
  
}

##  -------------------------------------
## Citation Network
##
collaboration_network <- function(authors_df) {
  
  #authors_df <- mf_authors_df
  
  # filter the lead authors into their own dataframe
  # lead author are identified where order = 1
  leads <- get_lead_author_index(authors_df) #authors_df %>% filter(order == 1) %>% select(PMID, AuthorName)
  
  # store coauthors in a separate dataframe
  co_authors <-  authors_df %>% filter(order != 1) %>% select(PMID, AuthorName)
  
  # create a dataframe of lead authors and who they have coauthored with
  collaboration_df <- 
    leads %>% inner_join(co_authors, by="PMID") %>% 
     select(AuthorName.x, AuthorName.y)   
  
  collaboration_df$LeadAuthor <- collaboration_df$AuthorName.x  #paste(collaboration_df$ForeName.x, collaboration_df$LastName.x)
  collaboration_df$CoAuthor   <- collaboration_df$AuthorName.y  #paste(collaboration_df$ForeName.y, collaboration_df$LastName.y)
  
  # reduce this to two field - Lead Author and CoAuthor (full names)
  collaboration_df <- collaboration_df[c("LeadAuthor", "CoAuthor")]
  
  # generate a directed graph using igraph
  g <- graph.data.frame(collaboration_df, directed=F) # orginally T
  
  #g <- simplify(g, remove.multiple = F, remove.loops = T) 
  
  return (g)
}

citation_network <- function(cited_df, lead_author_df) {
  
# Citation Network Analysis #
# ------------------------- # 

  cited_df %>% 
    inner_join(lead_author_df, by="PMID")  %>%
    inner_join(lead_author_df, by=c("CITED_BY_PMID"="PMID")) %>%
    select(AuthorName.x, AuthorName.y) -> citations_df
  
  # adjust the column names
  names(citations_df) <- c("AuthorName", "CitedByAuthorName")
  
  # generate a directed graph using igraph
  g <- graph.data.frame(citations_df, directed=F)
  
  #g <- simplify(g, remove.multiple = F, remove.loops = T) 
  
  return (g)
  
}



network_metrics <- function(igraph) {
  
  cn <- data.frame()
  
  cn <- data.frame(
    deg = degree(igraph),
    bet = betweenness(igraph),
    clo = closeness(igraph),
    eig = evcent(igraph)$vector,
    cor = graph.coreness(igraph),
    pageRank = page.rank(igraph)$vector
  )
  
  cn$AuthorName <- rownames(cn)
  rownames(cn) <- NULL
  
  # reorder the columns so the AuthorName is first
  return (select(cn, AuthorName, deg:pageRank))
  
  
}



calc_author_scholar_indexes <- function(pub_df, lead_author_df) {
  
  # create an index of citations 
  # because we're using only the authors associated with the given disease state some 
  # zero values are assigned
  select(pub_df, PMID, Citations) %>% inner_join(lead_author_df, by='PMID')  %>%
    arrange(AuthorName, desc(Citations)) %>% 
    filter(Citations > 0) %>% 
    group_by(AuthorName) %>% dplyr::mutate(id = row_number()) -> citation_index  
  
  
  citation_index %>% group_by(AuthorName) %>% 
    arrange(desc(Citations)) %>%
    dplyr::mutate(square = id^2,
                  sums = cumsum(Citations))  -> citation_index 
  
  # calculate the g-index value
  filter(citation_index, square < sums) %>% 
    group_by(AuthorName) %>% dplyr::summarise(Gindex = max(id)) -> author_gindex 
  
  # calculate the h-index value
  filter(citation_index, id <= Citations) %>% 
    group_by(AuthorName)   %>% dplyr::summarise(Hindex = max(id)) -> author_hindex 
  
  return (list(author_gindex, author_hindex))
  
}


## MF ## 

# Co-Authorship Collaborative Network #
# ----------------------------------- #

# create the network graph of collaborations between authors and coauthors
mf_collab_graph1 <- collaboration_network(mf_authors_df)

#generate the network metrics such as degree, centrality, etc.

mf_collab_metrics <- network_metrics(mf_collab_graph1)

#View(mf_collab_metrics)


# Influence Network by Citation  #
# ------------------------------ #

mf_lead_authors <- get_lead_author_index(mf_authors_df)
#View(lead_authors)

mf_citation_graph2 <- citation_network(mf_citations_df, mf_lead_authors) 

# generate network metrics 
mf_citation_metrics <- network_metrics(mf_citation_graph2)

View(mf_citation_metrics)


# Calculate H and G Indexes for Lead Authors  #
# ------------------------------------------- #

#citation_index <- get_citation_index(mf_publications_df, mf_lead_authors)

#View(citation_index)

mf_author_Indexes <- calc_author_scholar_indexes(mf_publications_df, mf_lead_authors)

#str(author_Indexes)

# list contains the g-index dataframe and the h-index dataframe
#View(author_Indexes[[1]]) 

#View(author_Indexes[[2]]) 


## PV ## 

# Co-Authorship Collaborative Network #
# ----------------------------------- #

# create the network graph of collaborations between authors and coauthors
pv_collab_graph1 <- collaboration_network(pv_authors_df)

#generate the network metrics such as degree, centrality, etc.

c <- network_metrics(pv_collab_graph1)

#View(mf_collab_metrics)


# Influence Network by Citation  #
# ------------------------------ #

pv_lead_authors <- get_lead_author_index(pv_authors_df)
#View(lead_authors)

pv_citation_graph2 <- citation_network(pv_citations_df, pv_lead_authors) 

# generate network metrics 
pv_citation_metrics <- network_metrics(pv_citation_graph2)

#View(pv_citation_metrics)


# Calculate H and G Indexes for Lead Authors  #
# ------------------------------------------- #


pv_author_Indexes <- calc_author_scholar_indexes(pv_publications_df, pv_lead_authors)

#str(author_Indexes)

# list contains the g-index dataframe and the h-index dataframe
#View(author_Indexes[[1]]) 

#View(author_Indexes[[2]]) 




## GVHD ## 

# Co-Authorship Collaborative Network #
# ----------------------------------- #

# create the network graph of collaborations between authors and coauthors
gvhd_collab_graph1 <- collaboration_network(gvhd_authors_df)   #View(gvhd_authors_df)

#generate the network metrics such as degree, centrality, etc.

gvhd_collab_metrics <- network_metrics(gvhd_collab_graph1)

#View(mf_collab_metrics)


# Influence Network by Citation  #
# ------------------------------ #

gvhd_lead_authors <- get_lead_author_index(gvhd_authors_df)
#View(lead_authors)

gvhd_citation_graph2 <- citation_network(gvhd_citations_df, gvhd_lead_authors) 

# generate network metrics 
gvhd_citation_metrics <- network_metrics(gvhd_citation_graph2)

#View(pv_citation_metrics)


# Calculate H and G Indexes for Lead Authors  #
# ------------------------------------------- #


gvhd_author_Indexes <- calc_author_scholar_indexes(gvhd_publications_df, gvhd_lead_authors)

#str(author_Indexes)

# list contains the g-index dataframe and the h-index dataframe
#View(author_Indexes[[1]]) 

#View(author_Indexes[[2]]) 

## Save to RData files 

save(mf_collab_graph1, mf_citation_graph2,  mf_collab_metrics,
     mf_citation_metrics, mf_author_Indexes, file = "mf_network.RData")

save(pv_collab_graph1, pv_citation_graph2, pv_collab_metrics,
     pv_citation_metrics, pv_author_Indexes, file = "pv_network.RData")

save(gvhd_collab_graph1, gvhd_citation_graph2, gvhd_collab_metrics,
     gvhd_citation_metrics, gvhd_author_Indexes, file = "gvhd_network.RData")

# END --------------------------


