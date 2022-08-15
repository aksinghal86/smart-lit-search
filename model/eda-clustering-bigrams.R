if (!require(pacman)) install.packages('pacman') 

pacman::p_load(
  tidyverse, 
  tidytext, 
  tm, 
  factoextra
)

# Initiate cleanNLP 
cleanNLP::cnlp_init_udpipe()

# Function to clean abstract text
clean.abstracts <- function(abstract) { 
  abstract %>% 
    tolower() %>% 
    removePunctuation() %>% 
    removeNumbers() 
}

# LOad pre-saved PubMed search output. Only used during dev
abstracts_xml <- read_rds('model/abstracts_xml.rds') %>% 
  filter(!is.na(abstract)) %>% 
  unite(abstract, c(title, abstract), sep = ' ') %>% 
  mutate(abstract = clean.abstracts(abstract))

# TODO: add my own stopwords 
# TODO: add search string items as words that should be kept 

# Create annotations and POS tagging. Only being used for lemmatization. 
# Quite slow -- not sure if it is worth it. 
# May not afford significant improvement relative to performance. 
annotation <- cleanNLP::cnlp_annotate(input = abstracts_xml, text = 'abstract', doc_name = 'pmid') 

# Create a dataframe of lemmatizations
lemmae <- annotation$token %>% 
  select(token, lemma) %>% 
  distinct(token, .keep_all = T)

# Filter down to relevant tokens
unigrams <- annotation$token %>% 
  filter(upos %in% c("NOUN", "PROPN")) %>% 
  select(pmid = doc_id, word = lemma) %>% 
  anti_join(stop_words, 'word') %>% 
  rename(token = word) 

bigrams <- abstracts_xml %>%
  unnest_tokens(token, abstract, token = 'ngrams', n = 2) %>% 
  separate(token, into = c('token1', 'token2'), sep = ' ') %>% 
  filter(!token1 %in% stop_words$word,
         !token2 %in% stop_words$word,
         !str_detect(token1, 'mml|http|xmlns'),
         !str_detect(token2, 'mml|http|xmlns')
         ) %>%
  left_join(lemmae, by = c('token1' = 'token')) %>%
  left_join(lemmae, by = c('token2' = 'token')) %>% 
  filter(!lemma.x %in% stop_words$word,
         !lemma.y %in% stop_words$word) %>% 
  unite(token, c(lemma.x, lemma.y), sep = ' ')

tokens <- unigrams %>% bind_rows(bigrams %>% select(pmid, token))

# 30 most frequent bigrams
tokens %>% 
  ungroup() %>% 
  count(token) %>% 
  arrange(desc(n)) %>% 
  slice(1:30) %>% 
  ggplot(aes(x = reorder(token, n), y = n)) + 
  geom_col() + 
  coord_flip() +
  theme_bw()


# Bag of words,i.e., simple counts
bow <- tokens %>% count(pmid, token) 

set.seed(123)
wordcloud::wordcloud(words = bow$token, freq = bow$n, min.freq = 10, max.words = 100, 
                     random.order = F, colors = RColorBrewer::brewer.pal(8, 'Dark2'))

# TF-IDF to identify tokens that are more frequent relative to the whole corpus
tfidf <- bow %>% bind_tf_idf(pmid, token, n) 

# Create a document-term matrix
dtm <- tfidf %>% 
  cast_dtm(pmid, token, tf_idf) %>% 
  tm::removeSparseTerms(0.95) %>% 
  as.matrix()

# Cosine similarity 
# The dot product of two positive-valued unit-length vectors is the cosine similarity 
# between the two vectors
cosine.similarity <- function(m) { 
  csim <- m / sqrt(rowSums(m * m))
  csim <- csim %*% t(csim)
}

cos_sim <- cosine.similarity(dtm)

# Compute a cosine dissimilarity matrix (aka distance matrix). 
# Clustering functions typically use a distance matrix.
dist_mat <- as.dist(1 - cos_sim)

# Visualize the distances
fviz_dist(dist_mat, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#### Clustering ----------------------------------------------------------------

## K-means clustering
kmc <- kmeans(dtm, 7, nstart = 25)
fviz_cluster(kmc, data = dtm, ggtheme = theme_minimal())

## Hierarchical clustering
hc <- hcut(dist_mat, k =7, stand = T, method = 'ward.D2')
fviz_dend(hc, rect = TRUE, cex = 0.5)


## Cluster profiling
df_dtm <- dtm %>% 
  as.data.frame() %>% 
  mutate(cluster = hc$cluster) # using HC clustering
  
df_terms <- df_dtm %>% 
  group_by(cluster) %>% 
  summarize_all('mean') %>% 
  pivot_longer(!contains(c('cluster')), names_to = 'token', values_to = 'tf_idf') %>%
  filter(tf_idf>0)


df_docs <- data.frame(cluster = hc$cluster) %>% 
  rownames_to_column('pmid') %>% 
  tibble()

get_top_words <- function(tokens, tf_idf, n_words = 7) { 
  sorted_tokens <- tokens[ order(tf_idf, decreasing = T) ]
  return(paste0(sorted_tokens[1: n_words], collapse = '\n'))
}

cluster_summary <- df_terms %>% 
  group_by(cluster) %>% 
  summarize(top_words = get_top_words(unique(token), unique(tf_idf))) %>% 
  left_join(
    df_docs %>% 
      group_by(cluster) %>% 
      summarize(size = n(), 
             docs = paste0(pmid, collapse = ', ')), 
    by = 'cluster'
  )
cluster_summary


#### Graphs

# MDS
points <- cmdscale(dist_mat, k = 2)

tibble(x = points[, 1], y = points[, 2]) %>% 
  add_column(df_docs) %>% 
  left_join(cluster_summary %>% select(cluster, top_words, size), by = 'cluster') %>% 
  ggplot(aes(x = x, y = y, color = factor(cluster), size = size)) + 
  geom_point(alpha = 0.8) +
  # stat_ellipse(size = 0.4) + 
  scale_color_brewer(palette = 'Dark2') + 
  theme_void()

## Word cloud by cluster

df_terms %>% 
  group_by(cluster) %>% 
  arrange(-tf_idf) %>% 
  slice(1:10) %>% 
  cast_tdm(token, cluster, tf_idf) %>% 
  as.matrix() %>% 
  wordcloud::comparison.cloud(scale = c(2, .4), random.order = F, 
                              colors = RColorBrewer::brewer.pal(7, 'Dark2'),
                              title.size = 1.4, match.colors = F)



# 
# #### Network analysis
# 
# links <- as.matrix(dist_mat) %>% 
#   as.data.frame() %>% 
#   rownames_to_column('from') %>% 
#   pivot_longer(!contains('from'), names_to = 'to', values_to = 'weight') %>% 
#   filter(weight > 0.95)
# 
# g <- graph_from_data_frame(d = links, directed = F)
# g <- graph.data.frame(links, directed = F)
# g <- simplify(g, remove.multiple = T, remove.loops = T)
# cutoff <- median(links$weight)
# g2 <- delete.edges(g, E(g)[weight<cutoff])
# plot(g2)
# 
# deg <- degree(g2, mode = 'all') 
# hist(deg, breaks = 1:vcount(g2)-1)
# 
# degree(g2, mode = 'in')
# centr_degree(g2, mode = 'in', normalized = T)
# 
# closeness(g2, mode = 'all', weights = NA)
# centr_clo(g2, mode = 'all', normalized = T)
# 
# eigen_centrality(g2, directed=T, weights=NA)
# centr_eigen(g2, directed=T, normalized=T) 
# 
# betweenness(g2, directed=T, weights=NA)
# edge_betweenness(g2, directed=T, weights=NA)
# centr_betw(g2, directed=T, normalized=T)
# 
# 
# g3 <- as.undirected(g2, mode = 'collapse', edge.attr.comb = list(weight = 'sum', 'ignore'))
# sapply(cliques(g3), length)
# largest_cliques(g3)
# 
# vcol <- rep('grey80', vcount(g3))
# vcol[unlist(largest.cliques(g3))] <- 'gold'
# plot(as.undirected(g3), vertex.label = V(g3)$name, vertex.color = vcol)
# 
# ceb <- cluster_fast_greedy(as.undirected(g2))
# dendPlot(ceb, mode = 'hclust')
# 
# plot(ceb, g2)
# membership(ceb)
# modularity(ceb)
# crossing(ceb, g2)
# 
# kc <- coreness(g, mode = 'all') 
# plot(g, vertex.label = kc)
# 
# 
# edges <- data.frame(
#   name = V(g)$name, 
#   group = edge.betweenness.community(g)$membership, 
#   betweenness = (betweenness(g, directed = F, normalized = T) * 115) + 0.1
# )
# 
# 
# # Network plot
# hc_cluster_widths <- hc$silinfo$widths
# edges <- hc_cluster_widths %>% 
#   group_by(from = as.integer(cluster), to = neighbor) %>% 
#   summarize(weight = mean(sil_width)^2) 
# nodes <- cluster_summary %>% select(id = cluster, label = docs, size) %>% 
#   mutate(size = size*10)
# 
# clusters_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = F)
# plot(clusters_igraph, layout = layout_with_graphopt)
# 
# nodes_d3 <- mutate(nodes, id = id-1)
# edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
# 
# networkD3::forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
#              NodeID = "label", Group = "id", Value = "weight", width ='auto',
#              Nodesize = 'size', charge = -100, 
#              opacity = 1, fontSize = 12, zoom = TRUE)

# set.seed(123)
# 
# df_dtm_long %>% 
#   group_by(cluster) %>% 
#   group_walk( ~ wordcloud::wordcloud(.x$token, .x$tf_idf, scale = c(2, .3), 
#                                      max.words = 20, rot.per = 0, random.order = F, 
#                                      colors = RColorBrewer::brewer.pal(8, 'Dark2'))) 
# plot_wordcloud <- function(c) { 
#   plotdata <- df_dtm_long %>% filter(cluster == c)
#   wordcloud::wordcloud(plotdata$token, plotdata$tf_idf, max.words = 20, 
#                        scale = c(2, .5), rot.per = 0, random.order = F, 
#                        colors = RColorBrewer::brewer.pal(8, 'Dark2'))
# }
# walk(unique(df_dtm_long$cluster), plot_wordcloud)
# 
# # Network plot
# c1 <- df_dtm_long %>% filter(cluster == 1)
# word_graph <- df_dtm_long %>% 
#   separate(token, c("token1", "token2"), sep = ' ', fill = 'right') %>% na.omit() %>% 
#   tidygraph::as_tbl_graph() 
# 
# ggraph::ggraph(word_graph, layout = 'fr') +
#   ggraph::geom_edge_link() + 
#   ggraph::geom_node_point(aes()) + 
#   ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1)
# 
# nodes <- tibble(label = unique(rownames(dtm))) %>% rowid_to_column('id')
# nodes
#         
# 
