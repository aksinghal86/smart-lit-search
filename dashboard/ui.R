head <- tagList(
  tags$link(
    href = paste0("https://fonts.googleapis.com/css?family=", "Raleway"),
    rel = "stylesheet"
  ),
  tags$style(
    paste0("*{font-family: '", "Raleway", "', sans-serif;}")
  ),
  tags$script(
    src = "https://unpkg.com/micromodal/dist/micromodal.min.js"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://use.fontawesome.com/releases/v5.7.2/css/all.css", 
    integrity = "sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr",
    crossorigin = "anonymous"
  ),
  tags$style(
    paste0(".pushbar{background-color:", "white", ";}")
  )
)

ui <- navbarPage(
  title = 'smartLYT', 
  fluid = T, 
  inverse = T, 
  windowTitle = "smartLYT", 
  header = head,
  theme = shinythemes::shinytheme("paper"), 
  id = "tabs", 
  
  tabPanel(
    "Search",
    shinyjs::useShinyjs(),
    includeScript('www/script.js'),
    useWaiter(),
    useAttendant(),
    
    div(
      class = "container",
      tags$head(includeCSS('www/styles.css')),

      style = "min-height:90vh;",
      div(
        style = "width: 100%; height: 300px; position: relative;z-index:-9;",
        div(
          id = "particles-target",
          style = "position: absolute; top: 0; bottom: 0; right: 0; left: 0;"
        ),
        div(
          style = "padding-top:60px;",
          h3("Smart PubMed Literature Finder", class = "center")
        )
      ),
      particlesjs::particles(target_id = 'particles-target', element_id = 'particles'),

      fluidRow(
        column(
          1, br(),
          actionButton("opts", "", icon = icon('plus')),
          tippy_this("opts", "More options")
        ),
        column(
          9,
          textInput("query", "", width = "100%", placeholder = "Enter your PubMed search here."),
        ),
        column(
          2,
          br(),
          actionButton(
            "submit",
            "Search",
            icon = icon("magnifying-glass"),
            width = "100%",
            class = "btn btn-primary"
          )
        )
      ),

      #### Options ---------------------------------------------------------
      div(
        id = "options",
        style = "display: none;",
        h3("Options"),
        fluidRow(

          # Number of papers
          column(
            6,
            sliderInput(
              "n",
              label = "Number of papers",
              min = 20,
              max = 250,
              value = 150,
              step = 20,
              width = "100%"
            ),
            tippy_this("n", "Number of titles to fetch")
          ),

          # Date range
          column(
            6,
            selectInput("range", "Range",
              choices = c(
                "Last year" = "last-year",
                "Last 5 years" = "last-5-years",
                "Custom" = "custom"
              ),
              selected = "last-year",
              width = "100%"
            ),
            div(
              id = 'custom-range-options',
              style = "display: none;",
              column(6,
                dateInput('from', 'From', value = Sys.Date() - 365)
              ),
              column(
                6,
                dateInput('to', 'To')
              )
            )
          )
        )
      ),
      div(
        style = "position:fixed;bottom:0px;right:30%;",
        p(
          class = "center",
          "Created by ",
          a(
            "Ankur Singhal",
            href = "https://github.com/aksinghal86",
            target = "_blank"
          ),
          "of The Empirical Solutions Consulting, LLC."
        )
      )
    )
  ), 
  
  tabPanel(
    'Results',
    div(
      class = 'results', 
      sidebarLayout(
        position = 'right', 
        sidebarPanel(
          width = 5,
          id = 'results-table',
        ),
        mainPanel(
          width = 7,
          fluidRow(
            ggiraphOutput('clusterPlot')
          ),
          fluidRow(
            plotOutput('wordcloud')
          )
        )
      )
    )
  ), 
  
  tabPanel(
    'About', 
    div(
      class = 'container', 
      fluidRow(
        tags$p(
          'Created by Ankur Singhal as a fun side project to focus the scope of research when deep diving into a new topic.', 
          'As environmental consultants at ', tags$a('Environmental Health & Engineering, Inc.,', href = "https://eheinc.com/", target = "_blank"),
          'we are often asked to provide advice on environmental and human health impacts of chemicals. The idea for this tool came to help our team', 
          'be more efficient when getting started on such projects.',
          br(),
          br(),
          'Code for this app is available ', tags$a('here.', href = 'https://github.com/aksinghal86/smart-lit-search', target = "_blank"), 
          'Please visit my other projects on', tags$a('Github.', href = 'https://www.github.com/aksinghal86', target = "_blank"),
        ), 
        tags$p(
          tags$h5(
            'Coming soon:'
          ), 
          tags$ul(
            tags$li('A detailed description on how the document clusterer was built.'), 
            tags$li('Time permitting, advancement and further tuning of the clustering algorithm to a) provide better results; and b) be faster at doing so.'), 
            tags$li('More advanced UI.')
          )
        ),
        tags$p(
          'Built in ', tags$a('R Shiny.', href = 'https://shiny.rstudio.com', target = "_blank"), br(), 
          "Design was inspired by John Coene's ", tags$a('chirp tool', href = 'https://shiny.john-coene.com/chirp/', target = "_blank"), 
          'for Twitter Network Exploring.'
        )
      )
    )
  )
)
