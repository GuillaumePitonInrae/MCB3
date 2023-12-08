### Create structure definition
#V0 July 2023 - G. Piton & C. Misset

#Function to load the bridge or barrier data from text files
import_structure<-function(InputDataRepository,StructureList,Structure_Ind,StructureName)
{
  
  #### function to load input data for structure in a given folder ####
  #### initial check of the data ####
  # number of files in the repository
  nb_files<- length(list.files(paste0(InputDataRepository,"/",StructureName)))
  
  # check if the structure is a bridge OR a barrier 
  if(nb_files>2){print(paste0("Warning: two many files in repository",StructureName,
                              ", please provide data for barrier or bridge only."))}
  # check non missing files
  if(nb_files==0){print(paste("Warning: structure data missing in repository of",StructureName))}
  
  #### import bridge or barrier data #### 
  # list of structure data
  nom_files<-list.files(paste0(InputDataRepository,"/",StructureName))
  
  # Load the barrier definition (openings)
  Opening<-read.csv(paste0(InputDataRepository,"/",StructureName,"/Opening.txt"),header = T)
  
  # import bridge data and convert it into storage / elevation curve
  if(length(nom_files[nom_files=="bridge.txt"])==1)
  {
    
    # read bridge data
    print("reading bridge data")
    
    bridge<-read.csv(paste0(InputDataRepository,"/",StructureName,"/bridge.txt"),sep="",header = T)
    
    # add values of name, type,storage / elevation curve and opening data

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
    

      # add values of name, type,storage / elevation curve and opening data
      
      StructureList$Name[Structure_Ind]<-StructureName
      StructureList$Type[Structure_Ind]<-"barrier"
      StructureList$StorageElevation[Structure_Ind]<-list(StorageElevation)
      StructureList$Openings[Structure_Ind]<- list(Opening)
      
  }
  
  
  
  # result of the function : a list with storage / elevation curve and openings data
  return(StructureList)
  
}


##Function computing the storage elevation data from the bridge information
define_bridgeStorageElevation<-function(Opening,width,slope)
{
  #### convert bridge data into storage / elevation curve ####
  # get min / max elevation values from bridge data
  BaseLevel_max<-max(Opening$BaseLevel)
  DeckLevel_max<-max(subset(Opening,Opening$Type=="slot")$Param)
  
  # interpolate the deposition slope on 10 values of slopes
  seq_slope<-seq(0,slope*0.95,length.out=10)
  
  # interpolate the downstream altitude, we do not extrapolate arbitrarily over 5 m above the max deck level
  seq_Z<-seq(min(Opening$BaseLevel),(max(BaseLevel_max,DeckLevel_max)+5),length.out=50)
  
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
      elev_storage[i,j]<-width*0.5*(elev_storage$Z[i]-min(Opening$BaseLevel))^2/(tan(slope)-tan(seq_slope[j-1]))
    }
  }
  return(elev_storage)
}


#### function to load all the structures data for the model and create a list with opening and storage / elevation data ####
structure_definition<-function(InputDataRepository){
  #list the repository available
  names_repository<-list.dirs(InputDataRepository,full.names = FALSE)
  #remove the parent repository
  names_repository<-names_repository[2:length(names_repository)]
  N_Structure<-length(names_repository)
  # list_save<-as.list(rep(NA,length(names_repository)))
  
  # names(list_save)<-names_repository
  #Initialize the structure list
  StructureList<-list(Name=rep(NA,N_Structure)
                   ,Type=rep(NA,N_Structure)
                   ,Rank=rep(NA,N_Structure)
                   ,TransferDownstream=rep(NA,N_Structure)
                   ,InitialConditions=list(rep(NA,N_Structure))
                   ,StorageElevation=list(rep(NA,N_Structure))
                   ,Opening=list(rep(NA,N_Structure))
                   ,width=rep(NA,N_Structure)
                   ,slope=rep(NA,N_Structure))
  
  
  for (Structure_Ind in 1:N_Structure)
  {
    StructureList<-import_structure(InputDataRepository,StructureList
                                    ,Structure_Ind,names_repository[Structure_Ind])
  }
  
  
  return(StructureList) 
  
}

# TEST<-import_structure(InputDataRepository,"pont 1")




