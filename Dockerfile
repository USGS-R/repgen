FROM usgswma/rserve:latest

ENV GSPLOT_VERSION_DEFAULT=0.8.1

COPY inst/extdata/installPackages.R /tmp/install/installPackages.R

RUN mkdir -p /tmp/install/gsplot_description_dir
RUN mkdir -p /tmp/install/gsplot_source_dir

# Get GSPlot Sources
RUN if getent ahosts "sslhelp.doi.net" > /dev/null 2>&1; then \
		wget 'http://sslhelp.doi.net/docs/DOIRootCA2.cer' && \
    wget --ca-certificate=DOIRootCA2.cer -O /tmp/install/gsplot_description_dir/DESCRIPTION https://raw.githubusercontent.com/USGS-R/gsplot/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}/DESCRIPTION && \
    wget --ca-certificate=DOIRootCA2.cer -O /tmp/install/gsplot_source_dir/gsplot.zip https://github.com/USGS-R/gsplot/archive/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}.zip ; \
	else \
    wget -O /tmp/install/gsplot_description_dir/DESCRIPTION https://raw.githubusercontent.com/USGS-R/gsplot/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}/DESCRIPTION && \
    wget -O /tmp/install/gsplot_source_dir/gsplot.zip https://github.com/USGS-R/gsplot/archive/v${GSPLOT_VERSION:-$GSPLOT_VERSION_DEFAULT}.zip ; \
	fi ;
  
# Install GSPlot Dependencies
RUN mkdir ${RSERVE_HOME}/R_libs && \
  mkdir ${RSERVE_HOME}/work && \
  cd /tmp/install/gsplot_description_dir && \
  Rscript /tmp/install/installPackages.R

# Install Repgen Dependencies
COPY DESCRIPTION /tmp/install/repgen/DESCRIPTION
RUN cd /tmp/install/repgen && Rscript /tmp/install/installPackages.R

# Build and install GSPlot Package
RUN cd /tmp/install/gsplot_source_dir && \
  unzip gsplot.zip && \
  mv ./gsplot-0.8.1/ ./gsplot/ && \
  R CMD build gsplot && \
  mv /tmp/install/gsplot_source_dir/gsplot_*.tar.gz /tmp/install/gsplot.tar.gz && \
  Rscript -e "install.packages(\"/tmp/install/gsplot.tar.gz\", repos = NULL, type=\"source\")" && \
  ls /opt/rserve/R_libs | if [ $(grep -c gsplot) -eq 0 ]; then echo "GSPlot build failed" && exit 1; fi

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
  ls /opt/rserve/R_libs | if [ $(grep -c repgen) -eq 0 ]; then echo "Repgen build failed" && exit 1; fi && \
  rm -rf /tmp/install
