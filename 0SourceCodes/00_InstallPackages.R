############ Installation code to run before to use the 01_MainCode.R
#It installs the used packages and record where are stored the source codes
## last update: July 19 2023

# install packages
install.packages("lubridate") #To add date on plot
install.packages("stringr") #To replace string
install.packages("svDialogs") #Package for popup dialog windows
install.packages("ggplot2") #Plots
install.packages("gridBase") #Plots
install.packages("gridExtra") #Plots
install.packages("sets")#Plots
install.packages("pbapply")#For Hyrisk
install.packages("reliaR")#For Hyrisk
install.packages("date")#For Hyrisk
install.packages("evir")#For Hyrisk
install.packages("chron")#for Hyrisk
install.packages("triangle")#For Hyrisk
install.packages("rgenoud")#For Hyrisk

# Then download the HYRISK package and its dependencies here: 
browseURL("https://cran.r-project.org/src/contrib/Archive/kerdiest/kerdiest_1.2.tar.gz")
browseURL("https://cran.r-project.org/src/contrib/Archive/reliaR/reliaR_0.01.tar.gz")
browseURL("https://cran.r-project.org/src/contrib/Archive/HYRISK/HYRISK_1.2.tar.gz")
browseURL("https://cran.r-project.org/src/contrib/Archive/grid/grid_0.7-4.tar.gz")

library(svDialogs) #Package for popup dialog windows

#Selecting the archives containing the packages that are not available on CRAN 
dlg_message(message="Please select the archive we just downloaded to get the kerdiest package (file kerdiest_1.2.tar.gz)"
            , type = c("ok"));DownloadedPackage<-dlg_open(default = "	kerdiest_1.2.tar.gz",
                                                          title = "Please select the archive we just downloaded to get the kerdiest package (file kerdiest_1.2.tar.gz)")$res
install.packages(DownloadedPackage, repos = NULL, type = "source")#Package uncertainty propagation


dlg_message(message="Please select the archive we just downloaded to get the reliaR package (file reliaR_0.01.tar.gz)"
            , type = c("ok"));DownloadedPackage<-dlg_open(default = "reliaR_0.01.tar.gz",
                                                          title = "Please select the archive we just downloaded to get the kerdiest package (file reliaR_0.01.tar.gz)")$res
install.packages(DownloadedPackage, repos = NULL, type = "source")#Package uncertainty propagation

dlg_message(message="Please select the archive we just downloaded to get the Hyrisk package (file HYRISK_1.2.tar.gz)"
            , type = c("ok"));DownloadedPackage<-dlg_open(default = "HYRISK_1.2.tar.gz",
                                                          title = "Show me where was downloaded the Hyrisk package (file HYRISK_1.2.tar.gz)")$res

install.packages(DownloadedPackage, repos = NULL, type = "source")#Package uncertainty propagation

dlg_message(message="Please select the archive we just downloaded to get the kerdiest package (file grid_0.7-4.tar.gz)"
            , type = c("ok"));DownloadedPackage<-dlg_open(default = "grid_0.7-4.tar.gz",
                                                          title = "Please select the archive we just downloaded to get the grid package (file grid_0.7-4.tar.gz)")$res
install.packages(DownloadedPackage, repos = NULL, type = "source")#Package uncertainty propagation
