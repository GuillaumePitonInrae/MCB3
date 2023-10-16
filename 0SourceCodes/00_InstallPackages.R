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
install.packages("https://cran.r-project.org/src/contrib/Archive/kerdiest/kerdiest_1.2.tar.gz")
install.packages("https://cran.r-project.org/src/contrib/Archive/reliaR/reliaR_0.01.tar.gz")
install.packages("https://cran.r-project.org/src/contrib/Archive/HYRISK/HYRISK_1.2.tar.gz")
