## Load the publication datasets from GitHub
GitHubPath <- "https://github.com/kfolsom98/DATA608/raw/master/Final_Project/"

mf_pubs_data <- load(url(paste0(GitHubPath, "myelofibrosis.RData")))
pv_pubs_data <- load(url(paste0(GitHubPath, "polycythemia_vera.RData")))
gvhd_pubs_data <- load(url(paste0(GitHubPath, "gvhd.RData")))


library(quanteda)


## Abstract Analysis ##

abstract_analysis <- function(publications_df) {
  
  publications_df <- mf_publications_df
  
  abs <- publications_df$Abstract 
  names(abs) <- publications_df$PMID
  
  # create the corpus #
  abstract_corpus <- corpus(abs)
  
  # add year docvar
  docvars(abstract_corpus, "Year") <- publications_df$Year
  
  # create a dfm of unigrams; no stemming
  pub_dfm1 <- quanteda::dfm(abstract_corpus, 
                            removeSeparators = TRUE, 
                            removeNumbers = TRUE, 
                            ignoredFeatures = stopwords("english"))
  
  # top 20 features
  top20_unigram <- topfeatures(pub_dfm1, 20) 
  
  # convert to dataframe #
  df_unigram <- data.frame(word=names(top20_unigram), 
                           count=top20_unigram,
                           Total_Words = dim(pub_dfm1)[2], 
                           Pct = top20_unigram / dim(pub_dfm1)[2])
  
  # create a dfm of bigrams; no stemming
  pub_dfm2 <- quanteda::dfm(abstract_corpus, 
                            ngrams = 2, removeSeparators = TRUE, 
                            removeNumbers = TRUE, 
                            ignoredFeatures = stopwords("english"))
  
  top20_bigram <- topfeatures(pub_dfm2, 20) 
  
  # convert to dataframe #
  df_bigram <- data.frame(word=names(top20_bigram), 
                          count=top20_bigram,
                          Total_Words = dim(pub_dfm2)[2], 
                          Pct = top20_bigram / dim(pub_dfm2)[2])
  
  # find the most frequently used words for each year in the corpus
  unigram_year_df <- data.frame()
  
  for (i in unique(publications_df$Year) ) {  
    
    #summary(corpus_subset(c, Year == i))
    
    print(i)
    
    # subset the corpus to a single year #
    csub <-  subset(abstract_corpus, Year == i)

    pub_dfm <- quanteda::dfm(csub,  
                             removeSeparators = TRUE, 
                             removeNumbers = TRUE, 
                             ignoredFeatures = stopwords("english"))
    
    
    #pub_dfm$Dim
    
    top20 <- topfeatures(pub_dfm, 20) 
    
    #csub
    
    unigram_year_df <- rbind(unigram_year_df, 
                            data.frame(Year = i, 
                                       Keyword = names(top20), 
                                       Count = top20, 
                                       Total_Words = dim(pub_dfm)[2], Pct = top20/ dim(pub_dfm)[2]))
    
  }  
  
  unigram_year_df$row.names <- NULL
  
  return (list(abstract_corpus, df_unigram, df_bigram, unigram_year_df))
}

## Generate the MF corpus and text analysis
# ===========================================================
abstract_out <- abstract_analysis(mf_publications_df)

mf_corpus <- abstract_out[[1]]
mf_top20_unigram <- abstract_out[[2]]
mf_top20_bigram <- abstract_out[[3]]
mf_top20_unigram_year <- abstract_out[[4]]

#View(mf_top20_bigram)

#View(mf_top20_unigram_year)

# may need to change this to return dfms and calc top values in shiny
save(mf_corpus, mf_top20_unigram, mf_top20_bigram, mf_top20_unigram_year, file="mf_text.RData")



## Generate the PV corpus and text analysis
# ===========================================================
abstract_out <- abstract_analysis(pv_publications_df)  

pv_corpus <- abstract_out[[1]]
pv_top20_unigram <- abstract_out[[2]]
pv_top20_bigram <- abstract_out[[3]]
pv_top20_unigram_year <- abstract_out[[4]]


# may need to change this to return dfms and calc top values in shiny
save(pv_corpus, pv_top20_unigram, pv_top20_bigram, pv_top20_unigram_year, file="pv_text.RData")



## Generate the GVHD corpus and text analysis
# ===========================================================
abstract_out <- abstract_analysis(gvhd_publications_df)  

gvhd_corpus <- abstract_out[[1]]
gvhd_top20_unigram <- abstract_out[[2]]
gvhd_top20_bigram <- abstract_out[[3]]
gvhd_top20_unigram_year <- abstract_out[[4]]


# may need to change this to return dfms and calc top values in shiny
save(gvhd_corpus, gvhd_top20_unigram, gvhd_top20_bigram, gvhd_top20_unigram_year, file="gvhd_text.RData")
