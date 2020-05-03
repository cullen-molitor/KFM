# Install R version 3.6.3
FROM rocker/shiny-verse:3.6.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  unixodbc-dev \
  libgdal-dev \
  libudunits2-dev \
  && install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    devtools \
    formatR \
    remotes \
    selectr \
    caTools \
    BiocManager \
  && rm -rf /tmp/downloaded_packages 

# Install R packages that are required
 # RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggridges', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('splitstackshape', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('measurements', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('rsconnect', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggnewscale', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ncdf4', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rnoaa', repos='http://cran.rstudio.com/')"

# Copy configuration files into the Docker image
COPY /www /srv/shiny-server/www
COPY about.md /srv/shiny-server/
COPY bands_MPA.csv /srv/shiny-server/
COPY bands_RAW.csv /srv/shiny-server/
COPY bands_Species.csv /srv/shiny-server/
COPY bands_Summary.csv /srv/shiny-server/
 # COPY Buoy_46053.csv /srv/shiny-server/
 # COPY Buoy_46054.csv /srv/shiny-server/
 # COPY Buoy_46218.csv /srv/shiny-server/
 # COPY Buoy_46251.csv /srv/shiny-server/
COPY Buoy_Stations.csv /srv/shiny-server/
COPY California_Marine_Protected_Areas.csv /srv/shiny-server/
COPY California_Marine_Protected_Areas.cpg /srv/shiny-server/
COPY California_Marine_Protected_Areas.dbf /srv/shiny-server/
COPY California_Marine_Protected_Areas.prj /srv/shiny-server/
COPY California_Marine_Protected_Areas.shp /srv/shiny-server/
COPY California_Marine_Protected_Areas.shx /srv/shiny-server/
COPY California_Marine_Protected_Areas.xml /srv/shiny-server/
COPY cinms_py.dbf /srv/shiny-server/
COPY cinms_py.html /srv/shiny-server/
COPY cinms_py.kmz /srv/shiny-server/
COPY cinms_py.prj /srv/shiny-server/
COPY cinms_py.sbn /srv/shiny-server/
COPY cinms_py.sbx /srv/shiny-server/
COPY cinms_py.shp /srv/shiny-server/
COPY cinms_py.shp.xml /srv/shiny-server/
COPY cinms_py.shx /srv/shiny-server/
COPY cinms_py.xml /srv/shiny-server/
COPY core_MPA.csv /srv/shiny-server/
COPY core_Species.csv /srv/shiny-server/
COPY core_Summary.csv /srv/shiny-server/
COPY fiveM_MPA.csv /srv/shiny-server/
COPY fiveM_Raw.csv /srv/shiny-server/
COPY fiveM_Species.csv /srv/shiny-server/
COPY fiveM_Summary.csv /srv/shiny-server/
COPY FSF_MPA.csv /srv/shiny-server/
COPY FSF_MPA_Raw.csv /srv/shiny-server/
COPY FSF_Raw.csv /srv/shiny-server/
COPY FSF_Summary.csv /srv/shiny-server/
COPY global.R /srv/shiny-server/
COPY KFM_2.Rproj /srv/shiny-server/
COPY KFM_2.Rproj /srv/shiny-server/
 # COPY KFM_Transects_line.cpg /srv/shiny-server/
 # COPY KFM_Transects_line.dbf /srv/shiny-server/
 # COPY KFM_Transects_line.prj /srv/shiny-server/
 # COPY KFM_Transects_line.shp /srv/shiny-server/
 # COPY KFM_Transects_line.shx /srv/shiny-server/
COPY KFM_Transects_SmoothLine5.cpg /srv/shiny-server/
COPY KFM_Transects_SmoothLine5.dbf /srv/shiny-server/
COPY KFM_Transects_SmoothLine5.prj /srv/shiny-server/
COPY KFM_Transects_SmoothLine5.shp /srv/shiny-server/
COPY KFM_Transects_SmoothLine5.shx /srv/shiny-server/
COPY NHSF_MPA.csv /srv/shiny-server/
COPY NHSF_MPA_Raw.csv /srv/shiny-server/
COPY NHSF_Raw.csv /srv/shiny-server/
COPY NHSF_Summary.csv /srv/shiny-server/
COPY nino34.csv /srv/shiny-server/
COPY nps_boundary.dbf /srv/shiny-server/
COPY nps_boundary.prj /srv/shiny-server/
COPY nps_boundary.shp /srv/shiny-server/
COPY nps_boundary.shx /srv/shiny-server/
COPY nps_boundary.xml /srv/shiny-server/
COPY oneM_MPA.csv /srv/shiny-server/
COPY oneM_RAW.csv /srv/shiny-server/
COPY oneM_Species.csv /srv/shiny-server/
COPY oneM_Summary.csv /srv/shiny-server/
COPY PDO_NOAA.csv /srv/shiny-server/
COPY PDO_UW.csv /srv/shiny-server/
COPY Protocols.csv /srv/shiny-server/
COPY RDFC_Raw.csv /srv/shiny-server/
COPY RDFC_Summary.csv /srv/shiny-server/
 # COPY README.md /srv/shiny-server/
COPY rpcs_MPA.csv /srv/shiny-server/
COPY rpcs_Species.csv /srv/shiny-server/
COPY rpcs_Summary.csv /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY SIO_Salt_Weekly.csv /srv/shiny-server/
COPY SIO_Temp_Weekly.csv /srv/shiny-server/
COPY Site_info.csv /srv/shiny-server/
COPY Site_info2.csv /srv/shiny-server/
COPY Site_info3.csv /srv/shiny-server/
COPY Species_Fish.csv /srv/shiny-server/
COPY SpeciesComplete.csv /srv/shiny-server/
COPY SpeciesName.csv /srv/shiny-server/
COPY Temp_weekly_summary.csv /srv/shiny-server/
COPY Temp_weekly_summary_byIsl.csv /srv/shiny-server/
COPY ui.R /srv/shiny-server/
COPY VisitDates.csv /srv/shiny-server/

 # COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# select port
 # EXPOSE 3838
ENV PORT 8080

 # Copy further configuration files into the Docker image
 # COPY shiny-server.sh /usr/bin/shiny-server.sh

# run app
CMD ["/usr/bin/shiny-server.sh"]