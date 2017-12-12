#Ubuntu Xenial 16.04

#Install GDAL 2.1
sudo add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
sudo apt update 
sudo apt install gdal-bin python-gdal python3-gdal
sudo apt-get install libgdal1-dev libproj-dev

#Install R
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get install r-base r-base-dev
