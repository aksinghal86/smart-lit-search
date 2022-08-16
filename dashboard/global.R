if (!require(pacman)) install.packages('pacman')
# if (!require(particlesjs)) devtools::install_github("dreamRs/particlesjs")

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
api_key <- read_lines('assets/ncbi-key.txt')