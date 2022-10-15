FROM --platform=linux/amd64 rocker/r-ver:4.2.1
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libxml2-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  zlib1g-dev \
  libudunits2-dev \
  libjq-dev \
  libprotobuf-dev \
  libmagick++-dev \
  libpoppler-cpp-dev \
  libgeos-dev \
  libgeos++-dev \
  libgdal-dev \
  gdal-bin \
  libproj-dev \
  protobuf-compiler \
  libprotobuf-dev \
  wget \
  && rm -rf /var/lib/apt/lists/*
ARG GITHUB_PAT
RUN wget https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN tar xjf ./phantomjs-2.1.1-linux-x86_64.tar.bz2 
RUN ln -s ./phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/bin/phantomjs
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("tidyverse", upgrade="never", version = "1.3.2")'
RUN Rscript -e 'remotes::install_version("DT", upgrade="never", version = "0.23")'
RUN Rscript -e 'remotes::install_version("shiny", upgrade="never", version = "1.7.2")'
RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer@dd8178db99cac78f0abbd236e83e07bf1f22ba18")'
RUN Rscript -e 'remotes::install_github("datasketch/parmesan@e1ae0769b2663725fc2fa29a48b89e6248be224c")'
RUN Rscript -e 'remotes::install_github("datasketch/hgchmagic@f8e1b5efcb1d25a9a442ae816eaf4c6eae9d328e")'
RUN Rscript -e 'remotes::install_github("datasketch/shinypanels@ce26c64f9749d1fbe90c992b1c15e83af576b305")'
RUN Rscript -e 'remotes::install_github("datasketch/dsmodules@5e9a9860ae27aad2cbecf3492be5eab1545e5ff5")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');daneApp::run_app()"