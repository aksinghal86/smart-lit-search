if (!require(pacman)) install.packages('pacman')
pacman::p_load( 
  tidyverse, 
  easyPubMed
)

entrez_id <- easyPubMed::get_pubmed_ids('PFOA OR "perfluorooctanoid acid" AND "cancer" AND "2017":"2022"')

t.start <- Sys.time()
abstracts_xml <- entrez_id %>% 
  fetch_pubmed_data() %>% 
  articles_to_list() %>% 
  map(~ article_to_df(.x, max_chars = -1, getAuthors = T, getKeywords = F)) %>% 
  bind_rows() %>% 
  group_by(pmid) %>%
  slice(1) %>%
  select(pmid, doi, title, abstract, keywords, lastname)
t.end <- Sys.time()
print(t.end-t.start)


abstracts_xml


pacman::p_load( 
  parallel, 
  foreach, 
  doParallel
)

t.start <- Sys.time()
records <- entrez_id %>% fetch_pubmed_data() %>% articles_to_list()
# Start a cluster with 3 cores
cl <- makeCluster(3)
registerDoParallel(cl)

# Perform operation (use foreach)
# The .combine argument guides result aggregation
fullDF <- tryCatch(
  
  {foreach(x = records, 
           .packages = 'easyPubMed',
           .combine = rbind) %dopar% article_to_df(pubmedArticle = x, 
                                                   autofill = F, 
                                                   max_chars = -1, 
                                                   getKeywords = F, 
                                                   getAuthors = T)}, 
  error = function(e) {NULL},
  finally = {stopCluster(cl)})

# Final time: record
t.stop <- Sys.time()

# How long did it take?
print(t.stop - t.start)