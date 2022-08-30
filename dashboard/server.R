server <- function(input, output, session) { 
  
  rvs <- reactiveValues(
    top_terms = NULL, 
    finaldata = NULL
  )
  
  shinyjs::hide("options") 
  shinyjs::hide("custom-range-options")
  
  observe({ 
    shinyjs::toggleState('submit', condition = input$query != '')
    if(input$submit == 0) hideTab(inputId = 'tabs', target = 'Results')
  })
  
  observeEvent(input$opts, { 
    shinyjs::toggle('options')
  })
  
  observeEvent(input$range, { 
    if (input$range == 'custom')
      shinyjs::show('custom-range-options')
    else 
      shinyjs::hide('custom-range-options')
  })
  
  
  # Waiting screen
  search_wait_screen <- div(
    # style = 'color: gray;',
    h4('Creating your smart search'),
    h4('This may take up to a minute'),
    spin_3circles(),
    br(), 
    br(), 
    h5('Fetching results from Pubmed...')
  )
  
  analyze_wait_screen <- div(
    h4('Creating your smart search'),
    h4('This may take up to a minute'),
    spin_3circles(), 
    br(), 
    br(), 
    h5('Analyzing documents and clustering...')
  )
  
  # wait_screen <- div(
  #   # style = 'color: black;', 
  #   h4('Fetching results from PubMed'),
  #   attendantBar(
  #     'the-bar', 
  #     width = 500,
  #     height = 25,
  #     text = ''
  #   ),
  #   h5('This may take a few seconds...')
  # )
  # Initialize the waiter
  w <- Waiter$new(
    html = search_wait_screen, 
    color = 'white'
  )
  
  # att <- Attendant$new('the-bar')
  
  observeEvent(input$submit, {
    
    # Create the query
    from <- case_when (
      input$range == 'last-year' ~ Sys.Date() - 365, 
      input$range == 'last-5-years' ~ Sys.Date() - 365*5, 
      input$range == 'custom' ~ input$from
    )
    from <- str_replace_all(from, "-", '/')
    
    to <- case_when (
      input$range %in% c('last-year', 'last-5-years') ~ Sys.Date(), 
      input$range == 'custom' ~ input$to
    )
    to <- str_replace_all(to, "-", '/')
    
    query <- paste0(
      str_replace_all(input$query, "\'", '\"'), 
      ' AND' , 
      ' english[language]', 
      ' AND' ,
      ' (', from, "[PDAT]", ':', to, "[PDAT]", ')'
    )
    print(query)
    
    # Show the waiter
    w$show()
    
    entrez_ids <<- get_pubmed_ids(HTML(query), api_key = api_key)
    
    if (entrez_ids$Count != '0') { 
      pubmed_results <<- entrez_ids %>%
        fetch_pubmed_data(retmax = input$n) %>%
        articles_to_list() %>%
        map(~ article_to_df(.x, max_chars = -1, getAuthors = T, getKeywords = F)) %>%
        bind_rows() %>%
        tibble()
      
      abstracts_df <<- pubmed_results %>%
        group_by(pmid, doi, title, abstract, keywords, year, month, day, jabbrv) %>%
        summarize(authors = paste0(lastname, ', ', substr(firstname, 1, 1), collapse = '; ')) %>%
        filter(!is.na(abstract)) %>%
        mutate(keywords = str_replace_all(keywords, '; ', ' ')) %>%
        unite(text, c(title, abstract, keywords), sep = ' ', remove = F) %>%
        mutate(text = clean.text(text),
               link = paste0("https://doi.org/", doi)) %>%
        ungroup()
      
      # Update the waiter to show that text is being analyzed now
      w$update(html = analyze_wait_screen)
      
      # Get tokens
      tokens <<- tokenize.text(abstracts_df)
      # tokens <<- read_rds(here::here('data/tokens.rds'))
      
      # Get TF-IDF 
      tfidf <- tokens %>% 
        count(pmid, token) %>% 
        bind_tf_idf(pmid, token, n)
      
      # Document-term matrix
      dtm <- tfidf %>% 
        cast_dtm(pmid, token, tf_idf) %>% 
        tm::removeSparseTerms(0.99) 
      
      # Cosine similarity and distance matrices
      cos_sim <- cosine.similarity(as.matrix(dtm))
      dist_mat <<- as.dist(1 - cos_sim)
      
      # TODO: refine the number of clusters (k)
      records <- nrow(abstracts_df)
      k <- case_when(records <= 120 ~ ceiling(records/15), 
                     TRUE ~ 8)
      
      hc <- factoextra::hcut(dist_mat, k = k, stand = T, method = 'ward.D2') 
      
      cluster_summary <<- get.cluster.summary(dtm, hc)
      
      rvs$top_terms <- cluster_summary$cluster_terms %>%
        arrange(cluster) %>% 
        group_by(cluster) %>%
        arrange(-count) %>%
        slice(1:5)
      
      # Multi-dimensional scaling
      mds_points <- cmdscale(dist_mat, k = 2)
      rvs$finaldata <- tibble(x = mds_points[, 1], y = mds_points[, 2]) %>% 
        bind_cols(cluster_summary$cluster_docs) %>% 
        left_join(abstracts_df, by = c('document' = 'pmid')) %>% 
        left_join(rvs$top_terms %>% group_by(cluster) %>% summarize(terms = paste0(term, collapse = ', ')), by = 'cluster') %>% 
        arrange(cluster) %>% 
        group_by(cluster = factor(cluster)) %>% 
        mutate(size = n())
    }
    
    w$hide()
  
  })
  
  observeEvent(input$submit, {
    # If search is successful show Results tab; otherwise show modal and go back to search results
    if (entrez_ids$Count != "0") {
      showTab(inputId = 'tabs', target = 'Results')
      updateTabsetPanel(session = session, inputId = 'tabs', selected = 'Results')
    } else if (entrez_ids$Count == '0') { 
      showModal(modalDialog(
        title = 'No results found',
        "PubMed returned zero results for your query. Please check spelling or modify your search parameters.",
        easyClose = F,
        footer = actionButton('closeModal', 'OK')
      ))
      shinyjs::reset('submit')  
      w$hide()
    }
  })
  
  observeEvent(input$closeModal, { 
    removeModal()
    showTab(inputId = 'tabs', target = 'Search')
    updateTabsetPanel(session = session, inputId = 'tabs', selected = 'Search')
  })

  
  output$wordcloud <- renderPlot({
    
    term_matrix <- rvs$top_terms %>%
      cast_tdm(term, cluster, count) %>%
      as.matrix()

    comparison.cloud(
      term_matrix, scale = c(0.9, 0.5), random.order = F, title.size = 2,
      colors = brewer.pal(length(unique(rvs$top_terms$cluster)), 'Dark2'),
      title.bg.colors = 'white'
    )
    # ggplot(rvs$top_terms, aes(label = term, color = factor(cluster)))  +
    #   ggwordcloud::geom_text_wordcloud(
    #     eccentricity = 0.75, shape = 'circle', seed = 123, size = 4
    #   ) +
    #   # facet_wrap(~ factor(cluster), nrow = 1) +
    #   scale_color_brewer(palette = 'Dark2') +
    #   theme_void()
    
  })
  
  output$clusterPlot <- renderGirafe({
    oc <- sprintf(
      paste0("const pmid = this.getAttribute(\"data-id\");", 
      "const element = document.getElementById(pmid);", 
      "element.scrollIntoView({behavior: \"smooth\", inline: \"start\"});")
    )
    plotdata <- rvs$finaldata %>% 
      mutate(#onclick =  sprintf("window.open(\"%s%s\")", "https://doi.org/", doi),
             onclick = oc, 
             tooltip = paste0(str_extract(authors, '[^;]+'), ' et al. (', year, ')')
             )
    
    plt <- ggplot(plotdata, aes(x = x, y = y, color = cluster)) + 
      geom_point_interactive(aes(tooltip = tooltip, data_id = document, onclick = onclick), alpha = 0.7, size = 4) +
      # ggforce::geom_mark_hull(aes(label = cluster), size = 0.3)+
      scale_color_brewer(palette = 'Dark2') +
      theme_void() + 
      theme(legend.position = 'none')
    
    girafe(
      ggobj = plt,
      options = list(
        opts_hover(css = 'opacity:0.9'),
        opts_hover_inv(css = 'opacity:0.3;'),
        opts_selection(type = 'single'), 
        opts_sizing(rescale = T)
      )
    )
    
  })
  
  observeEvent(input$submit, {
    termspal <- function (x) brewer.pal(length(unique(rvs$finaldata$cluster)), 'Dark2')[x]
    
    pmap(with(rvs$finaldata, list(document, cluster, title, link, authors, year, abstract, terms)),
         function(document, cluster, title, link, authors, year, abstract, terms) {
           insertUI(
             '#results-table', 
             'afterBegin',
             ui = wellPanel(
               class = 'table-text',
               tags$a(href = link, target = '_blank', class = 'jtitle', id = document, title), 
               tags$div(
                 class = 'jauthors', 
                 str_extract(authors, '[^;]+'), ' et al. ', tags$span(class = 'jyear', year),
               ), 
               tags$div(
                 class = 'jabstract', 
                 substr(abstract, 1, 300), tags$span(class = 'ellipsis', '...'), 
               ),
               tags$div(
                 class = paste0('terms '),
                 style = paste0('background: ', termspal(cluster), ';', 
                                'color: white;'), 
                 terms
               )
             )
           )
         }
    )
  })

  # observe({
  #   # print(input$clusterPlot_selected)
  #   # print(input$n)
  #   # print(input$from)
  # })
}