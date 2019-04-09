FROM usgswma/rserve:latest

ENV GSPLOT_VERSION_DEFAULT=0.8.1

COPY inst/extdata/installPackages.R /tmp/install/installPackages.R

# Get GSPlot dependency installs
RUN mkdir -p /tmp/install/gsplot_description_dir && \
  wget -O /tmp/install/gsplot_description_dir/DESCRIPTION https://raw.githubusercontent.com/USGS-R/gsplot/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}/DESCRIPTION

# Install GSplot
RUN mkdir ${RSERVE_HOME}/R_libs && \
  mkdir ${RSERVE_HOME}/work && \
  cd /tmp/install/gsplot_description_dir && \
  Rscript /tmp/install/installPackages.R && \
  Rscript -e "library(devtools);install_url('https://github.com/USGS-R/gsplot/archive/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}.zip', dependencies = F)"

# Install Repgen Dependencies
COPY DESCRIPTION /tmp/install/repgen/DESCRIPTION
RUN cd /tmp/install/repgen && Rscript /tmp/install/installPackages.R

# Copy Repgen Sources
COPY inst /tmp/install/repgen/inst
COPY man /tmp/install/repgen/man
COPY R /tmp/install/repgen/R
COPY tests /tmp/install/repgen/test
COPY .Rbuildignore /tmp/install/repgen/.Rbuildignore
COPY LICENSE /tmp/install/repgen/LICENSE
COPY NAMESPACE /tmp/install/repgen/NAMESPACE

# Build and install Repgen Package
RUN cd /tmp/install/ && R CMD build repgen && \
  mv /tmp/install/repgen_*.tar.gz /tmp/install/repgen.tar.gz && \
  Rscript -e "install.packages(\"/tmp/install/repgen.tar.gz\", repos = NULL, type=\"source\")" && \
  rm -rf /tmp/install
