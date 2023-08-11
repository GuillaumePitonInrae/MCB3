### Create structure definition and times series
#V0 July 2023 - G. Piton & C. Misset




####### a enlever - pour test
# InputDataRepository<-"D:/Private/05_PROJETS/2023_DFbuffering/4Simu/DFbuffering/1Data"

# InputDataRepository2<-"D:/Private/05_PROJETS/2023_DFbuffering/4Simu/DFbuffering/1Data"

####### a enlever - pour test


import_structure<-function(InputDataRepository,StructureName){
  #### initial check of the data ####
  # number of files in the repository
  nb_files<- length(list.files(paste0(InputDataRepository,"/",StructureName)))
  
  # check strcuture is a bridge OR a lateral structure 
  if(nb_files>2){print("Warning: barrier or bridge data only ")}
  
  # check non missing files
  if(nb_files==0){print(paste("Warning: structure data missing in repository of",StructureName))}
  
  # list of structure data
  nom_files<-list.files(paste0(InputDataRepository,"/",StructureName))
  
  # Load the barrier definition (openings)
  Opening<-read.csv(paste0(InputDataRepository,"/",StructureName,"/Opening.txt"),header = T)
  
  # import bridge data in convert into storage elevation curve
  if(length(nom_files[nom_files=="bridge.txt"])==1){
    
    # read bridge data
    print("reading bridge data")
    bridge<-read.csv(paste0(InputDataRepository,"/",StructureName,"/bridge.txt"),header = T)
    
    #### convert bridge data into elevation/storage curve ####
    # get min / max elevation values from bridge data
    Max.base.level<-max(Opening$Base.Level)
    Max.deck.level<- Opening %>% 
      filter(Type == "slot") %>%
      summarize(max(Param)) %>%
      pull()
    
    # interpolate the deposition slope on 10 values of slopes
    seq_slope<-seq(0,bridge$slope*0.95,length.out=10)
    
    # interpolate the downstream altitude
    seq_Z<-seq(min(Opening$Base.Level),(max(Max.base.level,Max.deck.level)+5),0.5)
    
    # create an Elevation Storage Curves data frame
    elev_storage<-data.frame(matrix(ncol = length(seq_slope),
                                    nrow = length(seq_Z),
                                    data = NA))
    elev_storage<-data.frame(seq_Z,elev_storage)
    
    # put the names in good format
    name_elev_storage<-rep(NA,length(seq_slope))
    
    for (i in 1:length(seq_slope)){
      name_elev_storage[i]<-paste0("S",round(seq_slope[i],3))
    }
    
    names(elev_storage)<-c("Z",name_elev_storage)
    
    # compute the elevation storage curve
    for (i in 1:nrow(elev_storage)){
      for (j in 2:(ncol(elev_storage))){
        elev_storage[i,j]<-bridge$width*0.5*(elev_storage$Z[i]-min(Opening$Base.Level))^2/(tan(bridge$slope)-tan(seq_slope[j-1]))
        }
    }
  
    StorageElevation<-elev_storage
    
  }else{
    

  # Load storage - elevation curve if no bridge data
    print("reading barrier data")
    
    # Load the barrier storage / elevation curve
    StorageElevation<-read.csv(paste0(InputDataRepository,"/",StructureName,"/ElevationStorageCurves.txt"),sep="")
    }
    
  # creating a list with barrier data
  structure<-list(StorageElevation=StorageElevation,Opening=Opening)
  
  return(structure)
  
}




structure_definition<-function(InputDataRepository2){
  #list the repository available
  names_repository<-list.dirs(InputDataRepository2,full.names = FALSE)
  #remove the parent repository
  names_repository<-names_repository[2:length(names_repository)]
  
  list_save<-as.list(rep(NA,length(names_repository)))
  
  names(list_save)<-names_repository
  
  for (i in 1:length(names_repository))
  {
    list_save[[i]]<-import_structure(InputDataRepository2,names_repository[i])
  }
  
  
 return(list_save) 
  
}


# save<-structure_definition(InputDataRepository2)



  

 







