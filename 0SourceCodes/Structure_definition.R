#' ---
#' title: "Structure Definition"
#' output: html_document
#' date: "2024-03-22"
#' Author: H. Shirra, G. Piton, C. Misset
#' ---
#' 
#' This is an annotated version of the Structure_definition.R  script, originally developed by G. Piton and C. Misset as part of a larger script to stochastically simulate jamming through a series of constrictions in a debris flow event. It creates a function to load the structure data from text files for each constriction. The contents of these text files defines the geometry of the structures, and are created by user input. This function can be called upon by the 00_MainCode.R.
#' 
#' ### Summary of Script
#' Prior to importing structure data, this function checks the data:
#' 
#'   - To define the number of files within the data repository.
#'   - To define the nature of the structure (bridge or barrier).
#'   - To see if any files are missing.
#' A warning message is displayed if there are too many files in the repository, or if any structure data is missing.
#' 
#' The structure data is imported from the text files for each constriction, including the names of all the structures, and the geometry of the openings. The geometry for each opening is associated with the structure, and the structures are sorted into "bridges" or "barriers".
#' The storage-elevation curve is computed for the bridges using the opening geometry, and the channel slope. The maximum deck height and maximum channel elevation are extracted and defined. The storage-elevation curve for barriers is uploaded by the user as part of the input data. 
#' 
#' All geometric data is combined into one data frame.
#' 
#' ### Script
## ----comment= "#", echo=FALSE-------------------------------------------------
#Transfer the .Rmd file to R automatically when it is knit. 
#knitr::purl(input = "Structure_definition.Rmd", documentation = 2) #Comment this line prior to running generated R script.

#' 
## -----------------------------------------------------------------------------
### Create structure definition
#V0 July 2023 - G. Piton & C. Misset

#Function to load the bridge or barrier data from text files
import_structure<-function(InputDataRepository,StructureList,Structure_Ind,StructureName)
{
  
  #### Function to load input data for structure in a given folder ####
  #### Initial check of the data ####
  # Number of files in the repository
  nb_files<- length(list.files(paste0(InputDataRepository,"/",StructureName)))
  
  # Check if the structure is a bridge or a barrier 
  if(nb_files>2){print(paste0("Warning: two many files in repository",StructureName,
                              ", please provide data for barrier or bridge only."))}
  # Check non missing files
  if(nb_files==0){print(paste("Warning: structure data missing in repository of",StructureName))}
  
  #### Import bridge or barrier data #### 
  # List of structure data
  nom_files<-list.files(paste0(InputDataRepository,"/",StructureName))
  
  # Load the barrier definition (openings)
  Opening<-read.csv(paste0(InputDataRepository,"/",StructureName,"/Opening.txt"),header = T)
  
  # Import bridge data and convert it into storage-elevation curve
  if(length(nom_files[nom_files=="bridge.txt"])==1)
  {
    
    # Read bridge data
    print("reading bridge data")
    
    bridge<-read.csv(paste0(InputDataRepository,"/",StructureName,"/bridge.txt"),sep="",header = T)
    
    # Add values of name, type,storage / elevation curve and opening data

      StructureList$Name[Structure_Ind]<-StructureName
      StructureList$Type[Structure_Ind]<-"bridge"
      StructureList$StorageElevation[Structure_Ind]<-list(data.frame(Z=NA,S0.00=NA))
      StructureList$Openings[Structure_Ind]<- list(Opening)
      StructureList$width[Structure_Ind] <- bridge$width
      StructureList$slope[Structure_Ind] <- bridge$slope
      
    
  }else{
    
    # Load storage / elevation curve if no bridge data
    print("reading barrier data")
    
    # Load the barrier storage / elevation curve
    StorageElevation<-read.csv(paste0(InputDataRepository,"/",StructureName,"/ElevationStorageCurves.txt"),sep="")
    

      # Add values of name, type,storage / elevation curve and opening data
      
      StructureList$Name[Structure_Ind]<-StructureName
      StructureList$Type[Structure_Ind]<-"barrier"
      StructureList$StorageElevation[Structure_Ind]<-list(StorageElevation)
      StructureList$Openings[Structure_Ind]<- list(Opening)
      
  }
  
  
  
  # Result of the function : a list with storage / elevation curve and openings data
  return(StructureList)
  
}


## Function computing the storage elevation data from the bridge information
define_bridgeStorageElevation<-function(Opening,width,slope)
{
  #### convert bridge data into storage / elevation curve ####
  # get min / max elevation values from bridge data
  BaseLevel_max<-max(Opening$BaseLevel)
  if(is.null(subset(Opening,Opening$Type=="slot")$Param))
  { DeckLevel_max <- BaseLevel_max+99
  }else{
    DeckLevel_max <- max(subset(Opening,Opening$Type=="slot")$Param)
  }
  
  # Interpolate the deposition slope on 10 values of slopes
  seq_slope<-seq(0,slope*0.95,length.out=10)
  
  # Interpolate the downstream altitude, we do not extrapolate arbitrarily over 5 m above the max deck level
  seq_Z<-seq(min(Opening$BaseLevel),(max(BaseLevel_max,DeckLevel_max)+5),length.out=50)
  
  # Create an storage - elevation curve data frame
  elev_storage<-data.frame(matrix(ncol = length(seq_slope),
                                  nrow = length(seq_Z),
                                  data = NA))
  elev_storage<-data.frame(seq_Z,elev_storage)
  
  # Format the names
  name_elev_storage<-rep(NA,length(seq_slope))
  for (i in 1:length(seq_slope)){name_elev_storage[i]<-paste0("S",round(seq_slope[i],3))}
  names(elev_storage)<-c("Z",name_elev_storage)
  
  # Compute the storage - elevation curve
  for (i in 1:nrow(elev_storage)){
    for (j in 2:(ncol(elev_storage))){
      elev_storage[i,j]<-width*0.5*(elev_storage$Z[i]-min(Opening$BaseLevel))^2/(slope-seq_slope[j-1])
    }
  }
  return(elev_storage)
}


#### Function to load all the structures data for the model and create a list with opening and storage - elevation data ####
structure_definition<-function(InputDataRepository){
  
  #Load the structure list and organisation----
  Structure_organisation<-read.csv(paste0(InputDataRepository,"/StructureList.txt"),sep="\t")
  
  # Load initial conditions
  #InitialConditions<-read.csv(paste0(InputDataRepository,"/InitialConditions.txt"),sep="\t")
  # 
  # Reorganize the table
  Structure_organisation<-data.frame(
    Name = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")],
    InitialCondition = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+1],
    Transfer = Structure_organisation$Value[which(Structure_organisation$Variable=="Structure")+2])
  
  # List the repositories available
  names_repository<-list.dirs(InputDataRepository,full.names = FALSE)
  # Remove the parent repository
  names_repository<-names_repository[2:length(names_repository)]
  N_Structure<-length(Structure_organisation$Name)
  #list_save<-as.list(rep(NA,length(names_repository)))
  
  #names(list_save)<-names_repository
  # Initialize the structure list
  StructureList<-list(Name=rep(NA,N_Structure)
                   ,Type=rep(NA,N_Structure)
                   ,Rank=rep(0,N_Structure)
                   ,TransferDownstream=rep(NA,N_Structure)
                   ,InitialConditions=list(rep(NA,N_Structure))
                   ,StorageElevation=list(rep(NA,N_Structure))
                   ,Opening=list(rep(NA,N_Structure))
                   ,width=rep(NA,N_Structure)
                   ,slope=rep(NA,N_Structure))
  
  
  for (Structure_Ind in 1:N_Structure)
  {
    StructureList<-import_structure(InputDataRepository,StructureList
                                    ,Structure_Ind,
                                    names_repository[match(Structure_organisation$Name[Structure_Ind],names_repository)])
  }
  
  
  return(StructureList) 
  
}

# TEST<-import_structure(InputDataRepository,"pont 1")


#' 
#' 
#' 
