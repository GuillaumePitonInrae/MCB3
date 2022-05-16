############ Installation code to run before to use the 01_MainCode.R
#It installs the used packages and record where are stored the source codes

# install packages
install.packages("lubridate") #To add date on plot
install.packages("stringr") #To replace string
install.packages("svDialogs") #Package for popup dialog windows
install.packages("ggplot2") #Plots

# Then download the HYRISK package here: 
browseURL("https://cran.r-project.org/src/contrib/Archive/HYRISK/HYRISK_1.2.tar.gz")

library(svDialogs) #Package for popup dialog windows
#Selecting the repository where the HYRISK package HYRISK_1.2.tar.gz is stored
dlg_message(message="Show me where was downloaded the Hyrisk package (file HYRISK_1.2.tar.gz)"
            , type = c("ok"));DownloadedPackage<-dlg_open(default = "HYRISK_1.2.tar.gz",
                                                          title = "Show me where was downloaded the Hyrisk package (file HYRISK_1.2.tar.gz)")$res

install.packages(DownloadedPackage, repos = NULL, type = "source")#Package uncertainty propagation

