if (!require(pacman)) install.packages('pacman')

pacman::p_load(
  # Shiny and related UI features
  shiny,  
  waiter, # for loading screen
  tippy, # for tooltips
  # promises, # ASYNC
  # future, # ASYNC
  easyPubMed, # Pubmed search 
  tidyverse, 
  tidytext, # for text processing
  tm, # for text processing, 
  ggiraph, # for interactive graphs
  wordcloud
  # reactable # for interactive table
)

source('functions.R')

# plan(multisession)

api_key <- read_lines(here::here('data/ncbi-key.txt'))

# PFOA or 'perfluorooctanoic acid' and 'cancer'