FROM davidedge/lipd_webapps:lipdBase

LABEL maintainer="Dave Edge <david.edge@nau.edu>"

# basic shiny functionality
RUN R -q -e "install.packages('shiny')"

# install dependencies of the map
RUN R -q -e "install.packages('rnaturalearth')"

#Install map data dep
RUN R -q -e "install.packages('rnaturalearthdata')"

#install usethis to load data
RUN R -q -e "install.packages('usethis')"

#install loading bar package
RUN R -q -e "install.packages('shinycssloaders')"

#install data table package
RUN R -q -e "install.packages('DT')"

# copy the app to the image
RUN mkdir /root/euler
COPY euler /root/euler

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/root/euler')"]
