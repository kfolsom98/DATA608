library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(purrr)

##################################################################################################################################

citation_trend <- function(publications_df, citations_df) {
  
   pubs <- select(publications_df, PMID, Year, LeadAuthorName)
   
   citations_df %>% inner_join(pubs, by=c("CITED_BY_PMID" = "PMID")) %>%
     rename(YearCited = Year,
            CitedByAuthorName = LeadAuthorName) %>%
     inner_join(pubs, by="PMID") -> cited
  
   # get counts by year   
   cited %>% select(LeadAuthorName, YearCited) %>% 
     group_by(LeadAuthorName, YearCited) %>% 
     summarise(Total = n())  -> df
   
   ## 2
   df %>% spread(YearCited, Total, fill = 0) %>% gather(Year, 'Total', -LeadAuthorName) -> df2
   
   # create the model function
   doModel <- function(dat) lm(Total ~ Year, dat)
   getSlope <- function(mod) coef(mod)[2]
   
   
   df2 %>% group_by(LeadAuthorName) %>% nest %>%
     mutate(model = map(data, doModel)) %>% 
     mutate(slope = map(model, getSlope)) -> cited_author_model
   
   cited_author_model$SLOPE <- unlist(cited_author_model$slope)
   
   return (cited_author_model[c("LeadAuthorName", "SLOPE")])
   
}

mf_citation_trend <- citation_trend(mf_publications_df, mf_citations_df)
pv_citation_trend <- citation_trend(pv_publications_df, pv_citations_df)

saveRDS(mf_citation_trend, "mf_citation_trend.rds")
saveRDS(pv_citation_trend, "pv_citation_trend.rds")

## ============================================================
## 
publication_trend <- function(publications_df) {
  
  ## 1
  publications_df %>% select(LeadAuthorName, Year) %>% group_by(LeadAuthorName, Year) %>% summarise(Total = n())  -> pubs
  
  ## 2: create 0 rows for years where no publications occurred for an author
  pubs %>% spread(Year, Total, fill = 0) %>% gather(Year, 'Total', -LeadAuthorName) -> pubs2
  
  
  df2 %>% group_by(LeadAuthorName)%>% nest
  
  
  doModel <- function(dat) lm(Total ~ Year, dat)
  getSlope <- function(mod) coef(mod)[2]
  
  
  pubs2 %>% group_by(LeadAuthorName) %>% nest %>%
    mutate(model = map(data, doModel)) %>% 
    mutate(slope = map(model, getSlope)) -> pubs_model
  
  
  pubs_model$SLOPE <- unlist(pubs_model$slope)
  
  return (pubs_model[c("LeadAuthorName", "SLOPE")])
  
}

mv_publication_trend <- publication_trend(mf_publications_df)
pv_publication_trend <- publication_trend(pv_publications_df)

saveRDS(mv_publication_trend, "mv_publication_trend.rds")
saveRDS(pv_publication_trend, "mv_publication_trend.rds")

##################################################################################################################################



