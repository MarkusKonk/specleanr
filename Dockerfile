FROM rocker/r-ver:4.3.0

# Install remotes (needed to install specific versions of packages)
RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com/')"


# Install system dependencies for terra and clean up afterward
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    make \
    g++ \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies:
COPY /.binder/install.R /src/install.R
RUN Rscript /src/install.R

# Install those packages that did not get installed via install.R
# TODO Fix this!
RUN R -e "install.packages(c('s2', 'units', 'sf'))"

# Copy the scripts to be called by the OGC processes:
COPY src /src
WORKDIR /src

# If the specleanr was updated, install it here again by
# uncommenting this before building.
# Once the version is fixed, it will be handled by install.R.
#RUN R -e 'remotes::install_github("AnthonyBasooma/specleanr")'


# Add an entrypoint that can deal with CLI arguments that contain spaces:
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

