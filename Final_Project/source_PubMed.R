# source_PubMed.R #

# Reference to MedLine data dictionary
# https://www.nlm.nih.gov/bsd/mms/medlineelements.html

library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(RISmed)
library(rentrez)
library(XML)
library(syuzhet)
library(countrycode)

#
# Functions to support extracting PubMed datasets:
#      1. Publications
#      2. Authors
#      3. Cited Publications (Citations)
#


## ------------------------------------------------------------------------
## get_citations:
##   Accepts: 
##            1.) list of PubMed IDs
##
##   Returns: data frame of citations (PMID level)
##          
## ------------------------------------------------------------------------

get_citations <- function(PMIDs) {
  
  citations_df <- data.frame(PMID = character(),
                             CITED_BY_PMID = character())  
  
  # use Entrez's linked database API to return the citation detail in XML format
  citation_links <- entrez_link(dbfrom='pubmed', id=PMIDs, db='pubmed', by_id=TRUE)
  
  # extract the PMIDs from the XML into a list
  citation_list <- lapply(citation_links, function(x) x$links$pubmed_pubmed_citedin)
  names(citation_list) <- PMIDs
  
  #filter out null entries
  citation_list <- Filter(Negate(is.null), citation_list)
  
  ### http://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  indx <- sapply(citation_list, length)
  
  # Load citations into a data frame
  res <- data.frame(do.call(rbind,lapply(citation_list, `length<-`, max(indx))), stringsAsFactors = F)
  
  # for a given year, likely more current years, there may not be any citations 
  # if this occurs, return an empty data frame
  
  if (nrow(res) > 0) {
    res$PMID <- rownames(res)
    citations_df <- res %>% gather(col, value="CITED_BY_PMID", -PMID, na.rm=TRUE) %>% select(PMID, CITED_BY_PMID)  
  }

  return (citations_df)
  
}

## ------------------------------------------------------------------------
## get_pubmed_info:
##   Accepts: 
##            1.) search criteria 
##            2.) a year constraints
##   Returns: list of data frames
##            - publications
##            - authors
##            - citations at the PMD ID level
## ------------------------------------------------------------------------

get_pubmed_info <- function(condition, pubYear) {
  
  #condition <-  'Polycythemia vera'
  #pubYear <- 2017
  
  r <- EUtilsSummary(condition, type='esearch', db='pubmed', mindate=pubYear, maxdate=pubYear)
  
  # fetch the MedLine publication details based on the search criteria
  fetch <- EUtilsGet(r, type="efetch", db="pubmed")
  
  # load the PubMed IDs (PMID) into a list
  PMIDs <- PMID(fetch)
  
  ## Publications ##
  
  ## create a dataframe containing the publications 
  pubs_df <- data.frame(PMID = PMIDs,
                        Year = YearPubmed(fetch),
                        Month = MonthPubmed(fetch),
                        Day  =  DayPubmed(fetch),
                        JournalTitle = RISmed::Title(fetch),
                        Abstract = AbstractText(fetch),
                        PublicationStatus = PublicationStatus(fetch),
                        PublicationType = unlist(map(PublicationType(fetch), 1)), ##PublicationType(fetch),
                        Country = Country(fetch),
                        Language = Language(fetch),
                        GrantID = GrantID(fetch),
                        Agency = Agency(fetch), 
                        MedlineTA = MedlineTA(fetch),
                        stringsAsFactors = F)
  
  ## Citations ##
  
  # this fetch returns the number of times the particular publication has been cited in total
  citations = Cited(fetch)
  citations_df <- data.frame(PMID = names(citations), Citations = citations, stringsAsFactors = F)
  
  # add the citation count to the publications data frame
  pubs_df <- cbind(pubs_df, citations_df$Citations)
  
  # rename the newly added citations column #
  names(pubs_df)[names(pubs_df)=="citations_df$Citations"] <- "Citations"
  
  # append the search term and extract date to the dataframe
  pubs_df$SearchTerm = condition
  pubs_df$ExtractDate = format(Sys.time(), "%Y%m%d")
  
  ## Authors ##
  
  # Fetch all authors listed on a publication .  This will be the lead author plus co-authors as determined by order
  AuthorList <- Author(fetch)
  
  for (i in 1:nrow(pubs_df)) {
    
    AuthorList[[i]]$PMID <-  pubs_df$PMID[[i]]
    AuthorList[[i]]$Year <- pubYear 
    
  }
  
  # convert to a dataframe 
  authors_df <- rbindlist(AuthorList)
  
  # there are variations of the same author on publications over time
  # create 
  authors_df$AuthorName <- paste(substr(authors_df$ForeName, 1, 1), authors_df$LastName )
  
  ## Citation Detail ##
  
  # getting the details of which publication cited which is done using the Rentrez package
  # Not exposed through RISmed directly
  # Wait somre more just in case
  Sys.sleep(3)
  citations_df <- get_citations(PMIDs)
  
  
  # return a list of the 3 data frame
  return (list(pubs_df, authors_df, citations_df))
  
}

## ------------------------------------------------------------------------
## set_lead_author:
##   Accepts: 
##            1.) publication dataframe
##            2.) author dataframe
##   Returns: df with Lead Author appended
##
## ------------------------------------------------------------------------

set_lead_author <- function(publications_df, authors_df) {
  
  lead_authors <- filter(authors_df, order == 1)  %>%
                   select(PMID, AuthorName) %>% rename(LeadAuthorName = AuthorName)
  
  publications_df %>% left_join(lead_authors, by="PMID") -> publications_df
  
  return (publications_df)
}

## ------------------------------------------------------------------------
## set_sentiment:
##   Accepts: 
##            1.) publication dataframe
##            2.) a year constraints
##   Returns: df with append nrc sentiment attributes
##
## ------------------------------------------------------------------------

set_sentiment <- function(publications_df) {
  
  #library(syuzhet)
  
  # apply the syuzhet nrc sentiment function 
  nrc_data <- get_nrc_sentiment(publications_df$Abstract)
  
  # append nrc sentiment attributes to the publication dataframe
  publications_df <- cbind(publications_df, nrc_data)
  
  # remove syuzhet package since this is causing some issues
  # with other functions 
  #detach("package:syuzhet", unload=TRUE)
  
  return (publications_df)
  
}


## ===============================================
## Myelofibrosis                                ## 
## - Get PubMed data for MF                     ##    
## ===============================================


mf_publications_df <- data.frame()
mf_authors_df      <- data.frame()     
mf_citations_df    <- data.frame() 

for (i in 2012:2017){
  # pause between requests per NCBI guidelines
  Sys.sleep(3)
  
  pubs_list <- get_pubmed_info('myelofibrosis', i)
  
  mf_publications_df <-  rbindlist(list(mf_publications_df, pubs_list[[1]]), use.names=T, fill = F) 
  
  mf_authors_df <- rbindlist(list(mf_authors_df, pubs_list[[2]]), use.names=T, fill = F) 
  
  mf_citations_df <- rbindlist(list(mf_citations_df, pubs_list[[3]]), use.names=T, fill = F) 
  
}

View(mf_publications_df)
View(mf_authors_df)
View(mf_citations_df)

## set the lead author for each pub
mf_publications_df <- set_lead_author(mf_publications_df, mf_authors_df)

## append sentiment analysis of the abstract
mf_publications_df <- set_sentiment(mf_publications_df)

# set the region using the countrycode package
custom_match <- c('England' = 'United Kingdom', 'Northern Ireland' = 'United Kingdom', 'Scotland' = 'United Kingdom')
mf_publications_df$Region <- countrycode(mf_publications_df$Country, "country.name.en",  'continent', custom_match = custom_match )


# Save the MF dataframes 
save(mf_publications_df, mf_authors_df, mf_citations_df, file = "myelofibrosis.RData")
saveRDS(mf_publications_df, "mf_publications.rds")
saveRDS(mf_authors_df, "mf_authors.rds")
saveRDS(mf_citations_df, "mf_citations.rds")

setwd("~/R/Code/Pubmed/Scripts")
#load(file = "myelofibrosis.RData")


## ===============================================
## Polycythemia vera                            ## 
## - Get PubMed data for PV                     ##    
## ===============================================

pv_publications_df <- data.frame()
pv_authors_df      <- data.frame()     
pv_citations_df    <- data.frame() 

for (i in 2012:2017){
  # pause between requests per NCBI guidelines
  Sys.sleep(3)
  
  pubs_list <- get_pubmed_info('Polycythemia vera', i)
  
  pv_publications_df <-  rbindlist(list(pv_publications_df, pubs_list[[1]]), use.names=T, fill = F) 
  
  pv_authors_df <- rbindlist(list(pv_authors_df, pubs_list[[2]]), use.names=T, fill = F) 
  
  pv_citations_df <- rbindlist(list(pv_citations_df, pubs_list[[3]]), use.names=T, fill = F) 
  
}

View(pv_publications_df)
View(pv_authors_df)
View(pv_citations_df)


## set the lead author for each pub
pv_publications_df <- set_lead_author(pv_publications_df, pv_authors_df)

## append sentiment analysis of the abstract
pv_publications_df <- set_sentiment(pv_publications_df)

# set the region using the countrycode package
custom_match <- c('England' = 'United Kingdom', 'Northern Ireland' = 'United Kingdom', 'Scotland' = 'United Kingdom')
pv_publications_df$Region <- countrycode(pv_publications_df$Country, "country.name.en",  'continent', custom_match = custom_match )


# Save the PV dataframes 
save(pv_publications_df, pv_authors_df, pv_citations_df, file = "polycythemia_vera.RData")
saveRDS(pv_publications_df, "pv_publications.rds")
saveRDS(pv_authors_df, "pv_authors.rds")
saveRDS(pv_citations_df, "pv_citations.rds")



## ===============================================
## GVHD                                         ## 
## - Get PubMed data for GVHD                   ##    
## ===============================================

gvhd_publications_df <- data.frame()
gvhd_authors_df      <- data.frame()     
gvhd_citations_df    <- data.frame() 

for (i in 2012:2017){
  # pause between requests per NCBI guidelines
  Sys.sleep(3)
  
  pubs_list <- get_pubmed_info('GVHD', i)
  
  gvhd_publications_df <-  rbindlist(list(gvhd_publications_df, pubs_list[[1]]), use.names=T, fill = F) 
  
  gvhd_authors_df <- rbindlist(list(gvhd_authors_df, pubs_list[[2]]), use.names=T, fill = F) 
  
  gvhd_citations_df <- rbindlist(list(gvhd_citations_df, pubs_list[[3]]), use.names=T, fill = F) 
  
}

View(gvhd_publications_df) dim(gvhd_publications_df) 
View(gvhd_authors_df)
View(gvhd_citations_df)

## set the lead author for each pub
gvhd_publications_df <- set_lead_author(gvhd_publications_df, gvhd_authors_df)

## append sentiment analysis of the abstract
gvhd_publications_df <- set_sentiment(gvhd_publications_df)

# set the region using the countrycode package
custom_match <- c('England' = 'United Kingdom', 'Northern Ireland' = 'United Kingdom', 'Scotland' = 'United Kingdom')
gvhd_publications_df$Region <- countrycode(gvhd_publications_df$Country, "country.name.en",  'continent', custom_match = custom_match )

# Save the data frames #
save(gvhd_publications_df, gvhd_authors_df, gvhd_citations_df, file = "gvhd.RData")

saveRDS(gvhd_publications_df, "gvhd_publications.rds")
saveRDS(gvhd_authors_df, "gvhd_authors.rds")
saveRDS(gvhd_citations_df, "gvhd_citations.rds")

dim(pv_citations_df)
dim(mf_citations_df)
dim(gvhd_citations_df)



# c1 <- get_citations(gvhd_publications_df$PMID[1:300])
# c2 <- get_citations(gvhd_publications_df$PMID[301:600])
# c3 <- get_citations(gvhd_publications_df$PMID[601:900])
# c4 <- get_citations(gvhd_publications_df$PMID[901:1290])
# 
#  rbindlist(list(c1, c2, c3, c4))

