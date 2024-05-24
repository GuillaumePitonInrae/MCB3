
##Function to simulate passage of debris flow through a series of cascading structures. 
Cascade_of_structure_functionning<-function(input)
{
  #Create input timesseries accordingly
  Qin<-Create_inlet_timeseries(input,Boulders)
  
  #Computation at each structure
  for(Structure_Ind in 1:sum(!is.na(Structures$Rank)))
  {
    #If not the first structure, then compute first the transferred inlet discharge and boulder
    if(Structure_Ind > 1)
    { 
      #record input 
      Qo_all_upstream<-Qo#_all
      #Check type of transfert condition
      TransferType <- Structures$TransferDownstream[[which(Structures$Rank==Structure_Ind-1)]]
      if(substr(TransferType,1,6) != "Mixing")
      {
        Vmixing<-1#Default value to define a value but which is unused.
      }else{
        Vmixing <- as.numeric(substr(TransferType
                                     ,8
                                     ,nchar(TransferType)))
      }
      #update the input accordingly
      Qo_all_upstream<-Qo_all_upstream[Qo_all_upstream$StructureRank == (Structure_Ind-1),]
      Qin<-Transfer_Between_Structure(Qo = Qo_all_upstream 
                                      ,TransferType = TransferType
                                      ,Vmixing = Vmixing)
    }
    
    #Compute the actual structure functionning
    Qo<-Structure_functionning(ModelVersion=ModelVersion
                               ,StructureName=Structures$Name[[which(Structures$Rank==Structure_Ind)]]
                               ,input=input,Qin=Qin
                               ,Opening=as.data.frame(Structures$Openings[[which(Structures$Rank==Structure_Ind)]])
                               ,StorageElevation=as.data.frame(Structures$StorageElevation[[which(Structures$Rank==Structure_Ind)]])
    )
    
    Result<-Synthetic_Structure_results(Qo, Structures$Openings[[which(Structures$Rank==Structure_Ind)]])
    Result$StructureRank<-Structure_Ind
    
    #record the run ID (general variable) and structure name
    # Qo$Run<-paste0("Run #",Run_Ind)
    Qo$StructureRank<-Structure_Ind
    
    #Record the run results
    # if(Run_Ind == 1){
    #   Result_all<-Result
    #   Qo_all<-Qo
    # }else{
    #   load(paste0("Result_Evt-",EventName,"_Structure_",Structures$Name[[which(Structures$Rank==Structure_Ind)]],".RData")
    #        ,envir=globalenv())
    #   Result_all<-rbind(Result_all,Result)
    #   Qo_all<-rbind(Qo_all,Qo)
    # }
    # save(Result,Qo,file=paste0("Result_Evt-",EventName,"_Structure_",Structures$Name[[which(Structures$Rank==Structure_Ind)]],".RData"))
    
    # Save a data frame with the main results of all runs as a .Rdata file
    File.Name<-paste0("Result_Evt-",EventName,"-_Structure_@-",Structures$Name[[which(Structures$Rank==Structure_Ind)]],"-ComputedOn",lubridate::now(),".Rdata")
    File.Name<-str_replace_all(File.Name,":","-")
    # File.Name<-str_replace_all(File.Name," ","_")
    save(Result,Qo,file=File.Name)
    
  } #end of the for loop on structures
  
  #print message
  load(file = "RunInd.Rdata")
  if(Perform_error_propagation){Run_Ind<-Run_Ind+0.5}else{Run_Ind<-Run_Ind+1}
  save(Run_Ind,N_runs,file = "RunInd.Rdata")
  print(paste0("PROGRESS[",Run_Ind,"/",N_runs,"]"))
  # if(!OnlyNormalRun){print(paste0("PROGRESS[one run computed]"))}
  #Return max outlet discharge at last structure
  return(max(Qo$Qo))
}
