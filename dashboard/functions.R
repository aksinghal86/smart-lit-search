# Function to clean text. 
clean.text <- function(dirty_text) { 
  dirty_text %>% 
    tolower() %>% 
    removePunctuation() %>% 
    removeNumbers() 
}

# Use POS to tag text using {cleanNLP}
annotate.text <- function(df) { 
  cleanNLP::cnlp_init_udpipe()
  cleanNLP::cnlp_annotate(input = df, text = 'text', doc_name = 'pmid')  
}

# get lemmae from annotated text. Used later for bigrams and trigrams
lemmatize <- function(annotated_text) { 
  annotated_text$token %>% 
    select(token, lemma) %>% 
    distinct(token, .keep_all = T)
}

# Get unigram lemmae from annotated text
unigrams <- function(annotated_text) {
  annotated_text$token %>% 
    filter(upos %in% c("NOUN", "PROPN")) %>% 
    select(pmid = doc_id, word = lemma) %>% 
    anti_join(stop_words, 'word') %>% 
    rename(token = word)
}

# Get bigrams. Some processing to remove stopwords
bigrams <- function(df, lemmae) { 
  df %>% 
    unnest_tokens(token, text, token = 'ngrams', n = 2) %>% 
    separate(token, into = c('token1', 'token2'), sep = ' ') %>% 
    filter(!token1 %in% stop_words$word,
           !token2 %in% stop_words$word,
           !str_detect(token1, 'mml|http|xmlns'), 
           !str_detect(token2, 'mmm|http|xmlns')) %>% 
    left_join(lemmae, by = c('token1' = 'token')) %>% 
    left_join(lemmae, by = c('token2' = 'token')) %>% 
    filter(!lemma.x %in% stop_words$word, 
           !lemma.y %in% stop_words$word) %>% 
    unite(token, c(lemma.x, lemma.y), sep = ' ') %>% 
    distinct(pmid, token)
}

# GEt trigrams. Some processing to remove stopwords. 
trigrams <- function(df, lemmae) { 
  df %>% 
    unnest_tokens(token, text, token = 'ngrams', n = 3) %>% 
    separate(token, into = c('token1', 'token2', 'token3'), sep = ' ') %>% 
    filter(!token1 %in% stop_words$word,
           !token2 %in% stop_words$word,
           !token3 %in% stop_words$word, 
           !str_detect(token1, 'mml|http|xmlns'), 
           !str_detect(token2, 'mmm|http|xmlns'), 
           !str_detect(token3, 'mmm|http|xmlns')) %>% 
    left_join(lemmae, by = c('token1' = 'token')) %>% 
    left_join(lemmae, by = c('token2' = 'token')) %>% 
    left_join(lemmae, by = c('token3' = 'token')) %>% 
    filter(!lemma.x %in% stop_words$word, 
           !lemma.y %in% stop_words$word, 
           !lemma %in% stop_words$word) %>% 
    unite(token, c(lemma.x, lemma.y, lemma), sep = ' ') %>% 
    distinct(pmid, token)
}

# Get tokens (unigrams, bigrams, trigrams) from a given dataframe of text
tokenize.text <- function(df) {

  annotated_text <- annotate.text(df)
  lemmae <- lemmatize(annotated_text)
  
  bind_rows(
    unigrams(annotated_text), 
    bigrams(df, lemmae), 
    trigrams(df, lemmae)
  )
  
}

cosine.similarity <- function(m) { 
  csim <- m / sqrt(rowSums(m * m)) 
  csim <- csim %*% t(csim)
}

get.cluster.summary <- function(dtm, hc) { 
  cluster_docs <- tibble(cluster = hc$cluster, document = names(hc$cluster))
  
  df_dtm <- tidy(dtm) %>% 
    left_join(cluster_docs, by = 'document')

  cluster_terms <- df_dtm %>% 
    group_by(cluster, term) %>% 
    summarize(count = mean(count))
  
  return( list(cluster_docs = cluster_docs, cluster_terms = cluster_terms))
}
