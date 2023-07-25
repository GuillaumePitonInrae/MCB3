### Create structure definition
#V0 July 2023 - G. Piton & C. Misset

#Clean environment
rm(list=ls())

# Load package
library(dplyr) #for data manipulation
library(readr) #for data loading

#### function to load input data for structure in a given folder ####
import_structure<-function(InputDataRepository){
  
  #### initial check of the data ####
  # number of files in the repository
  nb_files<- length(list.files(InputDataRepository))
  
  # check if the strcuture is a bridge OR a barrier 
  if(nb_files>2){print("Warning: barrier or bridge data only ")}
  
  # check non missing files
  if(nb_files==0){print("Warning: structure data missing ")}
  
  #### import bridge or barrier data #### 
  # list of structure data
  nom_files<-list.files(InputDataRepository)
  
  # import bridge data and convert it into storage / elevation curve
  if(length(nom_files[nom_files=="bridge.txt"])==1){
    
    # read bridge data
    print("reading bridge data")
    bridge<-read.csv(paste0(InputDataRepository,"/bridge.txt"),sep="")
    
    # Load the barrier definition (openings)
    Opening<-read.csv(paste0(InputDataRepository,"/Opening.txt"),header = T)
    
    #### convert bridge data into storage / elevation curve ####
    # get min / max elevation values from bridge data
    Max.base.level<-max(Opening$Base.Level)
    Max.deck.level<-max(subset(Opening,Opening$Type=="slot")$Param)
    
    # interpolate the deposition slope
    seq_slope<-seq(0,bridge$slope*0.95,length.out=10)
    
    # interpolate the downstream altitude
    seq_Z<-seq(min(Opening$Base.Level),(max(Max.base.level,Max.deck.level)+5),0.5)
    
    # create an storage / elevation Curves data frame
    elev_storage<-data.frame(matrix(ncol = length(seq_slope),
                                    nrow = length(seq_Z),
                                    data = NA))
    elev_storage<-data.frame(seq_Z,elev_storage)
    
    # put the names in good format
    name_elev_storage<-rep(NA,length(seq_slope))
    for (i in 1:length(seq_slope)){name_elev_storage[i]<-paste0("S",round(seq_slope[i],3))}
    names(elev_storage)<-c("Z",name_elev_storage)
    
    # compute the storage / elevation curve
    for (i in 1:nrow(elev_storage)){
      for (j in 2:(ncol(elev_storage))){
        elev_storage[i,j]<-bridge$width*0.5*(elev_storage$Z[i]-min(Opening$Base.Level))^2/(tan(bridge$slope)-tan(seq_slope[j-1]))
        }
    }
    
    # creating a list with storage / elevation curve and opening data
    structure<-list(StorageElevation=elev_storage,Opening=Opening)
    
  }else{
    
  # Load storage / elevation curve if no bridge data
    print("reading barrier data")
    
    # Load the barrier storage / elevation curve
    StorageElevation<-read.csv(paste0(InputDataRepository,"/ElevationStorageCurves.txt"),sep="")
 
    # Load the barrier definition (openings)
    Opening<-read.csv(paste0(InputDataRepository,"/Opening.txt"),header = T)
    
    # creating a list with barrier data
    structure<-list(StorageElevation=StorageElevation,Opening=Opening)
    }
    
  # result of the function : a list with storage / elevation curve and openings data
  return(structure)
  
}

#### function to load all the structures data for the model and create a list with opening and storage / elevation data ####
Structure_definition<-function(InputDataRepository2){
  
  # list of folders, 1 folder for each structure
  names_files<-list.files(InputDataRepository2)
  
  # create a list to save the structure data : a list of two data frame for each structure (opening and storage / elevation data)
  list_save<-as.list(rep(NA,length(names_files)))
  
  # put names with good format for the list to save
  names(list_save)<-names_files
  for (i in 1:length(names_files))
  {
    list_save[[i]]<-import_structure(paste0(InputDataRepository2,"/",names_files[i]))
    }

  # return the list with all structure data
  return(list_save) 
  
}

 







