# Insert Activities (INACT)
# Function: Expand individual diaries by inserting activities into idle times slots

#############################
#### Beginning of INACT #####
#############################
INACT <- function(rdiary, IIToutput){ 
    ############################################################################
    # Inputs:
    #       rdiary: a dataframe of a person's diary
    #       IIToutput: a dataframe output of from IIT with how PUCs are assigned
    # Outputs:
    #       rd: a dataframe of updated diary
    ############################################################################

    slots <- IIToutput$fslot
    # double the durations list by replicating each element in place
    durations <- rep(IIToutput$fdur_t, 1, each=2) 
    dur_theory_combine <- rep(IIToutput$fdur_t, 1, each=2)
    flag <- rep(IIToutput$flag, 1, each=2)
    periodicity <- rep(IIToutput$periodicity, 1, each=2)
    new_cluster_PUC <- rep(as.character(IIToutput$new_cluster_PUC), 1, each=2)
    new_cluster_name <- rep(as.character(IIToutput$new_cluster_name), 1, each=2)
    fni_list <- rep(as.character(IIToutput$fni_list), 1, each=2)
    freq_list <- rep(as.character(IIToutput$freq_list), 1, each=2)
    agg_fold_list <- rep(as.character(IIToutput$agg_fold_list), 1, each=2)
    freq_float <- rep(as.character(IIToutput$freq_float), 1, each=2)

    # individual's raw diary
    rd <- rdiary 
    # introduces duration variable in minutes  
    rd$Duration.min <- rdiary$Duration.hr*60
    
    # if there are no slots, return original diary
    if (slots[1]==""){
        return(rd)
    } else { 
        rd1 <- rd[slots,]     # diary with identified slots only
        rd2 <- rd[-slots,]    # diary without identified slots
        rd1 <- rd1[rep(1:nrow(rd1), 1, each=2),] # double rd1 in order
        
        # loop over expanded rd1 in increments of 2
        for(i in seq(1, nrow(rd1), 2)){ 
            # assigned idle time 
            rd1$Activity.Code[i] <- -2
            # assigned activity duration
            rd1$Duration.hr[i] <- durations[i]  
            # assigned activity duration
            rd1$Duration.min[i] <- durations[i]*60 
            rd1$product_type[i] <- IIToutput$product_type[1]
            rd1$sheds_id[i] <- IIToutput$sheds_id[1]
            rd1$Personal_or_Communal[i] <- IIToutput$Personal_or_Communal[1]
            rd1$Clusters[i] <- IIToutput$Clusters[1]
            rd1$Indoor_outdoor[i] <- IIToutput$Indoor_outdoor[1]
            rd1$periodicity[i] <- periodicity[i]
            rd1$dur_theory_combine[i] <- dur_theory_combine[i]
            rd1$flag[i] <- flag[i]
            rd1$new_cluster_PUC[i] <- new_cluster_PUC[i]
            rd1$new_cluster_name[i] <- new_cluster_name[i]
            rd1$fni_list[i] <- fni_list[i]
            rd1$freq_list[i] <- freq_list[i]
            rd1$agg_fold_list[i] <- agg_fold_list[i]
            rd1$freq_float[i] <- freq_float[i]

            # update remaining idle time block 
            # adjust remaining idle time duration
            rd1$Duration.hr[i+1] <- max(rd1$Duration.hr[i+1] - durations[i], 0) 
            # adjust remaining idle time duration
            rd1$Duration.min[i+1] <- max(rd1$Duration.min[i+1] - durations[i]*60, 0) 
            # adjust start time of remaining idle time slot
            rd1$Start.Time.hr.using.military.time[i+1] <- rd1$Start.Time.hr.using.military.time[i] + durations[i] 
        


        }    # end of for loop

        # join four elements together to create composite diary
        rd <- bind_rows(rd1, rd2) 
        # sort diary along minutes
        rd <- rd[order(rd$Minutes),]
        # remove unwanted interim diaries
        rm(rd1, rd2)  
        
        # delete interim variables
        rd$rind <- NULL 
        # delete interim variables
        rd$Duration.min <- NULL 
        # delete interim variables
        rd$Minutes <- NULL 
        rd <- filter(rd, Duration.hr>0)
        return(rd) # return diary
    } # end of else statement
    
} # end function inact

########################
#### End of INACT ######
########################
