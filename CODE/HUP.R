# Household Universe of Products (HUP)
# Function: Extracts information from the pophouse generator output to determine 
#           which PUCs a sampled household has the potential to use.

###########################
#### Beginning of HUP #####
###########################

HUP <- function(household_index, phf, ent, file_csv=NULL){
    
    ####################################################################
    # Inputs:
    #       household_index: index number of a given household
    #       phf: population and housing generator output file
    #       ent: ever never table
    # Outputs:
    #       huplist: a list of candidate PUCs
    ###################################################################
    
    # create a list of PUCs to be removed
    removelist <- c()

    # replace NA by 999. Otherwise it will cause problems in selection
    ent[is.na(ent)] <- 999 

    # remove excluded PUCs
    removelist <- c(removelist, ent[ent$NO2017==1, "PUCID_productype"])
    ent <- ent[ent$NO2017!=1, ]
    
    # remove HUP non-related PUCs
    removelist <- c(removelist, ent[ent$HUP==0, "PUCID_productype"])
    ent <- ent[ent$HUP==1, ]
    
    
    # read in housing characteristics of the hth household
    hc <- phf[household_index, ] 

    # read in universe of PUCs
    upuclist <- unique(ent$PUCID_productype) 

    # if statement for cars:
    if(hc$cars==0){ 
      removelist<-c(removelist, ent[ent$cars==0,]$PUCID_productype)  # removelist contains the shedsIDs, or PUCs, that must be removed when cars are not present (#rows in ent where cars==0 (no cars owned))
    } # end of if-else statement for cars
    
    #if statement for septic:
    if(hc$sewdis!=1){
      removelist<-c(removelist, ent[ent$sewdis==0,]$PUCID_productype) #removelist contains the shedsIDs, or PUCs, that must be removed when the house has no septic tank present (#rows in ent where sewdis!=1 (no septic tank))
    }  #end of if statement for septic
    
    #if statement for dishwasher:
    if(hc$dishwash==0){
     removelist<-c(removelist, ent[ent$dishwash==0,]$PUCID_productype) # removelist now contains the shedsIDs, or PUCs, that must be removed when no dishwasher is present (#rows in ent where dishwash==0 (no dishwasher))
    }
    
    #if statement for oven:
    if(hc$stoven==0) {
      removelist<-c(removelist, ent[ent$stoven==0,]$PUCID_productype) # removelist now  contains the shedsIDs, or PUCs, that must be removed when no oven is present (#rows in ent where stoven==0 (no oven))
    } 
    
    #if statement for printers:
    if(hc$pcprint==0){
      removelist<-c(removelist, ent[ent$pcprint==0,] $PUCID_productype) # removelist now  contains the shedsIDs, or PUCs, that must be removed when no printers are present (#rows in ent where pcprint==0 (no printers))
    } 
    
    #if statement for pools:
    if(hc$swim==0){
      removelist<-c(removelist, ent[ent$swim==0,]$PUCID_productype) # removelist now  contains the shedsIDs, or PUCs, that must be removed when no pool or hot tub is present (#rows in ent where swim==0 (no pool or hot tub))
    } 
    
    #if statement for yard:
    if(hc$lot<200){
      removelist<-c(removelist, ent[ent$yard==0,]$PUCID_productype) # removelist now contains the shedsIDs, or PUCs, that must be removed when no yard is present (#rows in ent where yard==0 (no yard))
    } 
    
    #if statement for renting:
    if(hc$kownrent>1){
      removelist<-c(removelist, ent[ent$kownrent==0,]$PUCID_productype) # removelist now  contains the shedsIDs, or PUCs, that must be removed when a property is not owned (#rows in ent where kownrent==0 (do not own))
    } 
    
   #concatenate the list of all PUCs that must be removed to accurately characaterize household
    removelist<-unique(removelist)
    removelist<-removelist[removelist!=""]
    
    #subtract those PUCs that must be removed from the full universe of household-relevant PUCs
    upuclist<- as.character(upuclist)
    huplist <- setdiff(upuclist, removelist)
    # print(length(huplist))

    # if an output file name is supplied, will save as an output file
    if(!is.null(file_csv)){
        write.csv(huplist, file_csv, row.names = FALSE)
    }

    #return the final list of household-relevant PUCs that are specific to a certain household
    return(huplist)
 
}

######################
#### End of  HUP #####
######################

