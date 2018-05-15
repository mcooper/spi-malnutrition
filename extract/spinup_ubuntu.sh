#Ubuntu Xenial 16.04

#Install GDAL 2.1
sudo add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
sudo apt update 
sudo apt install -y gdal-bin python-gdal python3-gdal
sudo apt-get -y install libgdal1-dev libproj-dev

#Install R
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get -y install r-base r-base-dev

#Install stan https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux
sudo apt-get -y install libssl-dev
sudo apt-get -y install libcurl4-gnutls-dev
sudo R
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined", file = M, sep = "\n", append = TRUE)
cat("\nCXXFLAGS+=-flto -Wno-unused-local-typedefs",  file = M, sep = "\n", append = TRUE)
cat(readLines(M), sep = "\n")
M <- file.path(file.path(Sys.getenv("HOME"), ".R"), "Makevars");
file.exists(M) #Should be TRUE
#You might be missing some package, like RCurl, rsconnect, or shinystan
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE) #This takes awhile
q()

#Install RStudio Server
sudo apt-get install -y gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.1.447-amd64.deb
sudo gdebi rstudio-server-1.1.447-amd64.deb
