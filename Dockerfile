# SmartLYT docker file
FROM rocker/shiny

# system libraries
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev \
    libssl-dev \
    zlib1g-dev \
    libxml2-dev \
    make \
    cmake \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    pandoc \
    libicu-dev \
    python3 \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    pacman \
    shiny \
    shinyjs \ 
    shinythemes \ 
    factoextra \
    waiter \
    tippy \
    easyPubMed \ 
    tidyverse \
    tidytext \
    tm \
    ggiraph \ 
    wordcloud \
    ggforce \
    udpipe \
    cleanNLP
    
#RUN R -e 'install.packages(c(\
 #   "pacman", \ 
  #  "shiny", \
#    "shinyjs", \
#    "shinythemes", \
#    "factoextra", \
#    "waiter", \
#    "tippy", \
#    "easyPubMed", \
#    "tidyverse", \
#    "tidytext", \
#    "tm", \
#    "ggiraph", \
#    "wordcloud", \
#    "ggforce", \
#    "udpipe", \
#    "cleanNLP" \
#    ), \
#    repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
#)'

RUN R -e 'remotes::install_github("dreamRs/particlesjs")'

#VEXPOSE 3838

# COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY ./dashboard/ /srv/shiny-server/

CMD ["/usr/bin/shiny-server"]