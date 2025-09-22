#' ---
#' title: "Install Packages"
#' output: html_document
#' date: "2024-03-22"
#' Author: G. Piton, C. Misset, H. Shirra
#' ---
#' This is an annotated version of the 00_InstallPackages.R  script, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions during a debris flow event. 
#' This script installs packages necessary to run the simulation, it must be run only once as an installation of the necessary packages.
#' 
#' #Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "00_InstallPackages.Rmd", documentation = 2) #comment this line prior to running generated R script.

#' 
# ----eval=FALSE---------------------------------------------------------------
############ Installation code to run before to use the 00_MainCode.R
#It installs the used packages
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
install.packages("dplyr") #for data manipulation
install.packages("jsonlite")#for headless mode, read all params from JSON


# Then download the HYRISK package and its dependencies here:
install.packages("https://cran.r-project.org/src/contrib/Archive/kerdiest/kerdiest_1.2.tar.gz")
install.packages("https://cran.r-project.org/src/contrib/Archive/reliaR/reliaR_0.01.tar.gz")
install.packages("https://cran.r-project.org/src/contrib/Archive/HYRISK/HYRISK_1.2.tar.gz")

