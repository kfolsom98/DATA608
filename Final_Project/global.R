library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(googleVis)
library(rCharts)
library(visNetwork)
library(plotly)
library(DT)

#library(DiagrammeR)

library(igraph)
library(lubridate)
library(bubbles)
library(timevis)
library(htmlwidgets)


DiseaseStateChoice <- c("Myelofibrosis", "Polycythemia vera", "GVHD")


#localPath <- "C:\\Users\\keith\\Google Drive\\Data Science\\CUNY\\DATA608 - Data Visualization\\Final Project\\"

#mf_pubs_data <- load(paste0(localPath, "myelofibrosis.RData"))
#pv_pubs_data <- load(paste0(localPath, "polycythemia_vera.RData")
#gvhd_pubs_data <- load(paste0(localPath, "gvhd.RData")

## Load the publication datasets from GitHub
GitHubPath <- "https://github.com/kfolsom98/DATA608/raw/master/Final_Project/data/"

mf_pubs_data <- load(url(paste0(GitHubPath, "myelofibrosis.RData")))
pv_pubs_data <- load(url(paste0(GitHubPath, "polycythemia_vera.RData")))
gvhd_pubs_data <- load(url(paste0(GitHubPath, "gvhd.RData")))

#Abstracts

mf_abstract_text <- load(url(paste0(GitHubPath, "mf_text.RData")))
pv_abstract_text <- load(url(paste0(GitHubPath, "pv_text.RData")))
gvhd_abstract_text <- load(url(paste0(GitHubPath, "gvhd_text.RData")))

# Author Networks #

## Load the Author network datasets 
mf_network <- load(url(paste0(GitHubPath, "mf_network.RData")))
pv_network <- load(url(paste0(GitHubPath, "pv_network.RData")))
gvhd_network <- load(url(paste0(GitHubPath, "gvhd_network.RData")))


dim(mf_publications_df)
dim(pv_publications_df)
dim(gvhd_publications_df)

dim(mf_authors_df)
dim(pv_authors_df)
dim(gvhd_authors_df)

dim(mf_citations_df)
dim(pv_citations_df)
dim(gvhd_citations_df)


## Load the Author network datasets 
#mf_network <- load("C:\\Users\\keith\\Google Drive\\Data Science\\CUNY\\DATA608 - Data Visualization\\Final Project\\mf_network.RData")
#pv_network <- load("C:\\Users\\keith\\Google Drive\\Data Science\\CUNY\\DATA608 - Data Visualization\\Final Project\\pv_network.RData")
#gvhd_network <- load("C:\\Users\\keith\\Google Drive\\Data Science\\CUNY\\DATA608 - Data Visualization\\Final Project\\gvhd_network.RData")


mf_AuthorList <- arrange(mf_citation_metrics, desc(deg)) %>% select(AuthorName) %>%  slice(1:20)
pv_AuthorList <- arrange(pv_citation_metrics, desc(deg)) %>% select(AuthorName) %>%  slice(1:20)
gvhd_AuthorList <- arrange(gvhd_citation_metrics, desc(deg)) %>% select(AuthorName) %>%  slice(1:20)