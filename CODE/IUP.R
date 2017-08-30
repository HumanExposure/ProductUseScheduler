# Individual Universe of Products (IUP)
# Function: Extract individual-level information, per household, from the 
#           pophouse generator to determine which PUCs an individual has 
#           the potential to use.

###########################
#### Beginning of IUP #####
###########################
IUP <- function(housePUCs, ic, ent, hpf, file_csv=NULL){
    
    ######################################################################
    # Inputs:
    #   housePUCs: a list of PUCs can be used by a household, output of HUP
    #   ic: a row input from HCP_output
    #   ent: ever never table
    #   hpf: product categories file; full list of possible PUCs
    # Outputs:
    #   iup_temp: a dataframe of PUCs at individual level
    ######################################################################

    #be sure to CLEAN LIST BEFORE RE-RUNNING 
    removelist <- c()

    # replace NA by 999. Otherwise it will cause problems in selection
    ent[is.na(ent)] <- 999 

    # add the no 2017 items to removelist
    removelist <- c(removelist, ent$sheds_id[ent$NO2017==1])

    # remove excluded PUCs
    ent <- ent[ent$NO2017!=1, ]

    # remove HUP non-related PUCs
    ent <- ent[ent$IUP!=0, ]

    #if statement for gender (Male/Female)
    if(ic$person_gender=="M"){ 
        removelist <- c(removelist, ent[ent$M==0,]$sheds_id) 
    } else {
        removelist <- c(removelist, ent[ent$F==0,]$sheds_id) # removelist contains the shedsIDs, or PUCs, that must be removed based on gender
    }

    # if else statements for
    # W = white
    # B = black
    # A = Asian
    # N = Native American
    # P = Pacific Islander
    # O = mixed/other
    if(ic$person_ethnicity=="W"){ 
        removelist <- c(removelist,ent[ent$W==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as white (W==0 in int)
    } else if (ic$person_ethnicity=="B"){ 
        removelist <- c(removelist,ent[ent$B==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as black (B==0 in int)     
    } else if(ic$person_ethnicity=="A"){  
        removelist <- c(removelist,ent[ent$A==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as Asian (N==0 in int)
    } else if(ic$person_ethnicity=="N"){ 
        removelist <- c(removelist,ent[ent$N==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as Native American (N==0 in int)
    } else if (ic$person_ethnicity=="P"){ 
        removelist <- c(removelist,ent[ent$P==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as Pacific Islander (P==0 in int)
    } else if(ic$person_ethnicity=="O"){ 
        removelist <- c(removelist,ent[ent$O==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual does not identify as mixed race/other (O==0 in int)
    }

    # if satement for age range 0_to_5_yrs, 6_to_12_yrs, 13_to_15_yrs, 16_to_18_yrs, 19_to_49_yrs, 50_plus
    if(ic$person_age>=0 && ic$person_age<=5){ 
        removelist <- c(removelist,ent[ent$age0_5==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is between 0 and 5 years old
    } else if(ic$person_age>=6 && ic$person_age<=12){ 
        removelist <- c(removelist,ent[ent$age6_12==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is between 6 and 12 years old
    } else if(ic$person_age>=13 && ic$person_age<=15){ 
        removelist <- c(removelist,ent[ent$age13_15==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is between 13 and 15 years old
    } else if(ic$person_age>=16 && ic$person_age<=18){ 
        removelist <- c(removelist,ent[ent$age13_15==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is between 16 and 18 years old
    } else if(ic$person_age>=19 && ic$person_age<=49){ 
        removelist <- c(removelist,ent[ent$age19_49==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is between 19 and 49 years old
    } else if(ic$person_age>=50){ 
        removelist <- c(removelist,ent[ent$age50plus==0,]$sheds_id)  # removelist contains the shedsIDs, or PUCs, that must be removed when individual is 50 years old or older
    }

    #concatenate the list of all individual PUCs that must be removed to accurately characaterize the individual's universe
    removelist<-unique(removelist)
    removelist<-removelist[removelist!=""]

    # #subtract those PUCs that must be removed from the full universe of individual-relevant PUCs
    iuplist <- setdiff(housePUCs, removelist)

    # prepare result df
    iup_temp <- data.frame("sheds_id"=iuplist,
                           "household_index"=ic$household_index,
                           "person_index"=ic$person_index,
                           "person_gender"=ic$person_gender,
                           "person_age"=ic$person_age,
                           "house_size"=ic$house_size)
    iup_temp$sheds_id <- as.character(iup_temp$sheds_id)

    hpf_sub <- select(hpf, SHEDSID, product_type, Personal_or_Communal)
    iup_temp <- left_join(iup_temp, hpf_sub, by=c("sheds_id"="SHEDSID"))
    
    # if an output file name is supplied, will save as an output file
    if(!is.null(file_csv)){
        write.csv(iup_temp, file_csv, row.names = FALSE)
    }

    #return the final list of individual-relevant PUCs that are specific to a certain individual
    return(iup_temp)  
}

#####################
#### End of IUP #####
#####################

