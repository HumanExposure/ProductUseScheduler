# Identify Idle Time (IIT)
# Function: for a specific PUC/cluster activity, identifies idle time slots in 
#           the individual diary to accommodate the activity over the full 
#           period of the diary.
#           First time slot chosen at random in the first period of the diary
#           Subsequent time slots chosen at the required periodicity
#           Consider seasonality


###########################
#### Beginning of dpro ####
###########################
# Function: process raw diary and return global processed diaries

dpro <- function(rdiary, sdf, PUC, region) { 
    #################################################################
    # Inputs:
    #       rdiary: a dataframe of a person's full diary
    #       sdf: PUC's seasonality info (amongst other descriptors)
    #       PUC: a PUC's sheds_idZ
    #       region: region of a household
    # Outputs:
    #       rdiary: a dataframe of processed raw diary
    #       idiary: a dataframe of idle diary
    #################################################################
    rdiary$rind <- seq.int(nrow(rdiary))            # introduce row index
    rdiary$Duration.min <- rdiary$Duration.hr*60    # introduces duration variable in minutes  
    rdiary$Minutes <- rdiary$Day.of.the.year*24*60+rdiary$Start.Time.hr.using.military.time*60
    idiary <- rdiary[rdiary$Day.of.the.year!=0,]    # keep only days not equal to 0 in idle diary
    idiary <- idiary[idiary$Activity.Code==-1,]     # keep only idle slots in idle diary 

    if (nrow(idiary) >0){
        # count of idle time slots per day
        idc<-aggregate(idiary$Activity.Code~idiary$Day.of.the.year, idiary, FUN=NROW) 
        names(idc)<-c('Day.of.the.year', 'N.Idle') # adjust names of idc
        idiary<-merge(idiary, idc, by=c('Day.of.the.year'), all.x=TRUE) # merge idiary with idc

        # if statement to filter PUCs based on seasonality
        if (!is.na(PUC) && PUC %in% sdf$sheds_id && !is.na(region)) { 
            sdays <- c() # initialize seasonal days list

            # region-specific construction of seasonal days list
            if (region==1|region==2|region==4) { 
                # warm weather condition
                if (sdf$WH[sdf$sheds_id==PUC]==1)   {sdays=union(sdays, 92:182)} 
                # cool weather condition
                if (sdf$Cool[sdf$sheds_id==PUC]==1) {sdays=union(sdays, c(1:91, 183:273))} 
                # cold weather condition
                if (sdf$Cold[sdf$sheds_id==PUC]==1) {sdays=union(sdays, 274:364)} 
            } # end of region 1, 2 and 4 condition

            # region-specific construction of seasonal days list
            if (region==3){
                # warm weather condition
                if (sdf$WH[sdf$sheds_id==PUC]==1) {sdays=union(sdays, 1:273)}
                # cool weather condition
                if (sdf$Cool[sdf$sheds_id==PUC]==1) {sdays=union(sdays, 274:364)} 
            } # end of region 3 condition

            # keep only those days in the idle diary that are appropriate to the season/s the PUC may be used in
            idiary<-idiary[idiary$Day.of.the.year %in% sdays,]  
        } # end of PUC seasonality condition
    } else{
        idiary <- rdiary[0,]
    }

    return (list("rdiary"=rdiary, "idiary"=idiary))
  } # end of dpro function

#####################
#### End of dpro ####
#####################


############################
#### Beginning of fslot ####
############################
# Function: to find the first slot; depends on: periodicity (per) specified in 
#           minutes; duration (dur) specified in hours

fslot <- function(idiary, per, dur) { 
    #################################################################
    # Inputs:
    #       idiary: a dataframe of idle diary
    #       per: periodicity of a PUC (happen every other x minutes)
    #       dur: duration of a PUC in hr
    # Outputs:
    #       fslot_return: a dataframe of first slot
    #################################################################

    # flag: indicator of duration status
    # 1-- aggregated and truncated
    # 2-- aggregated and no truncated
    # 3-- no aggregated but truncated
    # 4-- no aggregated no truncated

    fslot <- c()
    fdur_t <- c()
    fday <- c()
    flag <- c()
    fni_list <- c()
    freq_list <- c()
    agg_fold_list <- c()
    freq_float <- c()

    freq_f <- 24*60/per  #  frequency per day

    # generate a random number to account
    prob_freq_ceiling  <- freq_f%%1
    prob_freq_floor <- 1 - prob_freq_ceiling
    freq <- sample(c(floor(freq_f), ceiling(freq_f)), size=1, 
                   prob=c(prob_freq_floor, prob_freq_ceiling), 
                   replace = TRUE)


    # keep only those days in the individual idle diary occurring in the first period
    id <- idiary[idiary$Minutes<=per,] 

    # keep only those idle time slots greater than duration
    id <- id[id$Duration.hr>dur,] 

    # temporary adjustable periodicity for while loop below 
    mper <- per 

    # if we could not find a proper time slot in the initial attempt,
    # while loop addresses situations when time slots of adequate 
    # duration are not available
    while (nrow(id)==0 & mper<=max(idiary$Minutes)){ 
        # try adding one hour to periodicity
        mper <- mper+24*60 
        id <- idiary[idiary$Minutes<=mper,]
        id <- id[(id$Duration.hr-dur)>=0.01,] 
    } #end of while

    # if time slots are available then...
    if (nrow(id)>0) { 
        rnd <- sample(1:nrow(id), 1) # pick one time slot at random
        # assign individual diary's original row index for that slot to variable sl.
        sl <- id$rind[rnd]  
        # number of idle times on the chosen day of first slot 
        fni <- id$N.Idle[rnd] 
        # record duration of that idle slot
        idle_hr <- id$Duration.hr[rnd]

        # keep the selected day only, row number should equal fni
        id_sub <- idiary[which(idiary$Day.of.the.year==id$Day.of.the.year[rnd] & idiary$Duration.hr>0),]

        ###############################################################
        # adjusted duration  
        # if freq is greater than available time slots in a day, 
        # need to aggregate PUC activities to the available slots.
        # first calculate this aggregate multiplier,  agg_fold
        # we want agg_fold to an integer to avoid fraction of duration
        # thus, need to either round up or round down agg_fold
        ###############################################################
        agg_fold <- freq/fni

        # generate a random number to account
        prob_agg_fold_ceiling <- agg_fold%%1
        prob_agg_fold_floor <- 1 - prob_agg_fold_ceiling
        # cat ("      fslot_agg_fold=", agg_fold, " freq=", freq, " fni=", fni, " per=", per,"\n")

        # executes only if the daily frequency of the activity 
        # exceeds number idle time slots on that day
        if (fni < freq){ 
            for (j in 1:fni) {
                # sample a aggregation value
                agg_fold_1 <- sample(c(floor(agg_fold), ceiling(agg_fold)), size=1, 
                             prob=c(prob_agg_fold_floor, prob_agg_fold_ceiling), 
                             replace = TRUE)
                fdur <- dur*agg_fold_1

                if (id_sub$Duration.hr[j] < fdur){
                    # 1 -- aggregated and truncated
                    # do nothing now.
                    while(agg_fold_1 > 0 & fdur > id_sub$Duration.hr[j]){
                        agg_fold_1 <- agg_fold_1 -1
                        fdur <- dur*agg_fold_1
                    }
                    # if we find a time slot by reducing agg_fold_1
                    if (agg_fold_1 > 0){
                        flag <- c(flag, 11)
                        fslot <- c(fslot, id_sub$rind[j])
                        fdur_t <- c(fdur_t, fdur)
                        fday <- c(fday, id_sub$Day.of.the.year[j])
                        fni_list <- c(fni_list, fni)
                        freq_list <- c(freq_list, freq)
                        agg_fold_list <- c(agg_fold_list, agg_fold_1)
                        freq_float <- c(freq_float, freq_f)
                    } else { #do nothing
                    }
                } 
                else{
                    # 2-- aggregated and no truncated
                    flag <- c(flag, 21)
                    fslot <- c(fslot, id_sub$rind[j])
                    fdur_t <- c(fdur_t, fdur)
                    fday <- c(fday, id_sub$Day.of.the.year[j])
                    fni_list <- c(fni_list, fni)
                    freq_list <- c(freq_list, freq)
                    agg_fold_list <- c(agg_fold_list, agg_fold_1)
                    freq_float <- c(freq_float, freq_f)
                }
            } # end of for loop
        } else if (fni >= freq) { # covers case where daily frequency less than fni
            fdur <- dur
            id_sub1 <- arrange(id_sub, desc(Duration.hr))[1:max(freq,1),] %>% arrange(rind)

            for (j1 in 1:max(freq,1)) { # loop over each idle time slot
                idle_hr <- id_sub1$Duration.hr[j1]
                if (idle_hr < fdur){
                    # 3-- no aggregated but truncated
                    # do nothing
                } else{
                    # 4-- no aggregated no truncated
                    flag <- c(flag, 41)
                    fdur_t <- c(fdur_t, fdur)
                    fslot <- c(fslot, id_sub1$rind[j1])
                    fday <- c(fday, id_sub1$Day.of.the.year[j1])
                    fni_list <- c(fni_list, fni)
                    freq_list <- c(freq_list, max(freq,1))
                    agg_fold_list <- c(agg_fold_list, 1)
                    freq_float <- c(freq_float, freq_f)
                }
            }
        } # else cover activities fni >= freq
    } # end of if statement if timeslots are available 
    # cat("     freq=", freq, "fni=", fni, ", fdur=", fdur, "\n")

    fslot_return <- data.frame("fslot"=fslot, 
                               "fdur_t"=fdur_t, 
                               "flag"=flag, 
                               "fday"=fday, 
                               "fni_list"=fni_list,
                               "freq_list"=freq_list, 
                               "agg_fold_list"=agg_fold_list,
                               "freq_float"=freq_float
                               )
    return(fslot_return)    # return row index for first slot and duration
} # end of fslot function
#####################
#### End of fslot ###
#####################


###############################
#### Beginning of allslots ####
###############################
# Function: finds successive time slots for activity. 
#           depends on: periodicity (min), duration (hr) and first slot.

allslots <- function(idiary, per, dur, fsl, ICP_output_pc=NULL, 
                     ICP_output_cc=NULL, is_cluster=FALSE, Clusters_temp=""){ 
    #################################################################
    # Inputs:
    #       idiary: a dataframe of idle diary
    #       per: periodicity of a PUC in minutes
    #       dur: duration of a PUC in hr
    #       fsl: a dataframe output from fslot()
    #       ICP_output_pc:
    #       ICP_output_cc:
    #       is_cluster:
    #       Clusters_temp:
    # Outputs:
    #       allslots_return: a dataframe of assigned slots
    #################################################################

    fslot <- c(fsl$fslot)
    fdur_t <- c(fsl$fdur_t)
    fday <- c(fsl$fday)
    flag <- c(fsl$flag)
    new_cluster_PUC <- c(fsl$new_cluster_PUC)
    new_cluster_name <- c(fsl$new_cluster_name)
    fni_list <- c(fsl$fni_list)
    freq_list <- c(fsl$freq_list)
    agg_fold_list <- c(fsl$agg_fold_list)
    freq_float <- c(fsl$freq_float)

    freq_f <- 24*60/per  #  frequency per day

    # generate a random number to account
    prob_freq_ceiling <- freq_f%%1
    prob_freq_floor <- 1 - prob_freq_ceiling
    freq <- sample(c(floor(freq_f), ceiling(freq_f)), size=1, 
                   prob=c(prob_freq_floor, prob_freq_ceiling), 
                   replace = TRUE)

    # if first slot is not missing then...
    if (nrow(fsl)>0) { 
        # row index for first minute that activity occurs 
        fm <- idiary$Minutes[idiary$rind==tail(fsl$fslot,1)]   
        # next activity should occur after nm in col Minute
        nm <- fm+per  # next minute is first minute plus periodicity

        if (per>1187 & per<1582) {
          fm_sub <- idiary[idiary$rind==tail(fsl$fslot,1),] # diary for row of day of first slot
          fs_day <- fm_sub$Day.of.the.year # first slot day of year

          nm_t <- idiary[min(which(idiary$Day.of.the.year > fs_day)), 'Minutes'] # idle time diary for next closest day
          if(!is.na(nm_t)){ 
            nm <- nm_t
          } 
        }

        # while loop as long as nm does not exceed last minute in year
        while (nm <= max(idiary$Minutes)){ 
            # if it is a cluster, we need to sample its composition to create variability
            if (is_cluster==TRUE){
                CSF_out <- CSF(ICP_output_pc, ICP_output_cc, Clusters_temp)
                # cat ("      dur=", CSF_out$dur/60, "dur_old=", dur, "freq=", 24*60*365/per, " new_cluster_PUC=", CSF_out$new_cluster_PUC, "\n")
                dur <- CSF_out$dur/60
                new_cluster_PUC_t <- paste(as.character(CSF_out$new_cluster_PUC), collapse=" ")
                new_cluster_name_t <- CSF_out$new_cluster_name
            } else{
                new_cluster_PUC_t <- NA
                new_cluster_name_t <- NA
            }

            # from individual idle diary, keep 1) minutes including and after nm 
            # and 2) slots whose duration exceeds duration of activity
            id <- idiary[which(idiary$Minutes>=nm & idiary$Duration.hr-dur>=0.01), ] 

            # if time slots are available then...
            if (nrow(id) > 0) { 
                # order time slots by row index. not sure if needed or would automatically be ordered.
                order(id$rind) 
                actday <- id$Day.of.the.year[1]
                id1 <- id[which(id$Day.of.the.year==actday),]
                fni <- nrow(id1) # number of idle times on the chosen day of first slot

                ################################################################
                # adjusted duration  
                # if freq is greater than available time slots in a day, 
                # need to aggregate PUC activities to the available slots.
                # first calculate this aggregate multiplier,  agg_fold
                # we want agg_fold to an integer to avoid fraction of duration
                # thus, need to either round up or round down agg_fold
                ################################################################
                agg_fold <- freq/fni
                # generate a random number to account
                prob_agg_fold_ceiling <- agg_fold%%1
                prob_agg_fold_floor <- 1 - prob_agg_fold_ceiling
                # cat ("      allslots_agg_fold=", agg_fold, " freq=", freq, " fni=", fni, "\n")

                # executes if the number of idle time slots on the day 
                # is less than the daily frequency
                # aggregation needs to happen
                if (fni < freq) { 
                    # minute corresponding to ending idle time that day 
                    # (to prevent multiple assignments to same day)
                    lm <- id1$Minutes[nrow(id1)] + id1$Duration.min[nrow(id1)] 
                    lm_day <- id1$Day.of.the.year[1] # day of the year on which last minute of activity occurs
                    nm <- lm + per 
                    # set next minute at minute corresponding to final idle 
                    # slot on that day plus periodicity

                    if (per>1187 & per<1582) {
                      nm_t <- idiary[min(which(idiary$Day.of.the.year > lm_day)), 'Minutes'] # idle time diary for next closest day
                       # first idle time minutes on next day
                      if(!is.na(nm_t)){ 
                        nm <- nm_t
                      } 
                    } 

                    # loop over each idle time slot in that day
                    for (j in 1:fni) {  
                        # sample a aggregation value
                        agg_fold_1 <- sample(c(floor(agg_fold), ceiling(agg_fold)), size=1, 
                                             prob=c(prob_agg_fold_floor, prob_agg_fold_ceiling), 
                                             replace = TRUE)
                        mdur <- dur*agg_fold_1      # aggregate duration

                        # if mdur is longer than available time,
                        # will reduce agg_fold_1 by 1 at a time
                        if (mdur > id1$Duration.hr[j]){
                            while(agg_fold_1 > 0 & mdur > id1$Duration.hr[j]){
                                agg_fold_1 <- agg_fold_1-1
                                mdur <- dur*agg_fold_1
                            }
                            # if we find a time slot by reducing agg_fold_1
                            if (agg_fold_1 > 0){
                                flag <- c(flag, 12)
                                fslot <- c(fslot, id1$rind[j])
                                new_cluster_PUC <- c(new_cluster_PUC, new_cluster_PUC_t)
                                new_cluster_name <- c(new_cluster_name, new_cluster_name_t)
                                fdur_t <- c(fdur_t, mdur)
                                fday <- c(fday, id1$Day.of.the.year[j])
                                fni_list <- c(fni_list, fni)
                                freq_list <- c(freq_list, freq)
                                agg_fold_list <- c(agg_fold_list, agg_fold_1)
                                freq_float <- c(freq_float, freq_f)
                            } 
                        } else if (mdur<=id1$Duration.hr[j]){
                            # 2-- aggregated and no truncated
                            flag <- c(flag, 22)
                            fslot <- c(fslot, id1$rind[j])
                            new_cluster_PUC <- c(new_cluster_PUC, new_cluster_PUC_t)
                            new_cluster_name <- c(new_cluster_name, new_cluster_name_t)
                            fdur_t <- c(fdur_t, mdur)
                            fday <- c(fday, id1$Day.of.the.year[j])
                            fni_list <- c(fni_list, fni)
                            freq_list <- c(freq_list, freq)
                            agg_fold_list <- c(agg_fold_list, agg_fold_1)
                            freq_float <- c(freq_float, freq_f)
                        }
                    } # end of for loop
                } else if (fni >= freq) { # covers case where daily frequency less than fni
                    mdur <- dur
                    # select n rows with largest Duration.hr, where n=max(freq,1)
                    id2 <- arrange(id1, desc(Duration.hr))[1:max(freq,1),] %>% arrange(rind)
                    lm <- tail(id2$Minutes,1) + tail(id2$Duration.min,1) # minute corresponding to ending idle time that day (to prevent multiple assignments to same day)
                    lm_day<-id2$Day.of.the.year[1] # day of the year on which last minute of activity occurs
                    nm <- lm+per  # next minute is first minute plus periodicity

                    if (per>1187 & per<1582) {
                      nm_t <- idiary[min(which(idiary$Day.of.the.year > lm_day)),'Minutes'] # idle time diary for next closest day
                      if(!is.na(nm_t)){ 
                        nm <- nm_t
                      } 
                    } 

                    for (j1 in 1:max(freq, 1)) { # loop over each idle time slot
                        idle_hr <- id2$Duration.hr[j1]
                        # cat("j1=", j1, "idle_hr=", idle_hr, "mdur=", mdur, "\n")
                        if (idle_hr < mdur){
                            # 3-- no aggregated but truncated
                            # do nothing
                        } else{
                            # 4-- no aggregated no truncated
                            flag <- c(flag, 42)
                            fdur_t <- c(fdur_t, mdur)
                            fslot <- c(fslot, id2$rind[j1])
                            new_cluster_PUC <- c(new_cluster_PUC, new_cluster_PUC_t)
                            new_cluster_name <- c(new_cluster_name, new_cluster_name_t)
                            fday <- c(fday, id2$Day.of.the.year[j1])
                            fni_list <- c(fni_list, fni)
                            freq_list <- c(freq_list, max(freq, 1))
                            agg_fold_list <- c(agg_fold_list, 1)
                            freq_float <- c(freq_float, freq_f)
                        }
                    }
                    # cat("Scenario 3  ", id$rind[1], " ", id$Day.of.the.year[1], "\n ")
                } # end fni >= freq
            } # end of if statement for when timeslots are available
            else {
                nm <- nm + per

                if (per>1187 & per<1582) {
                  lm_sub<-idiary[idiary$Minutes>=nm,] # diary for last minute of assigned slot
                  lm_day<-lm_sub$Day.of.the.year[1] # first slot day of year
                  nm_t <- idiary[min(which(idiary$Day.of.the.year > lm_day & idiary$Duration.hr-dur>=0.01)),'Minutes'] # idle time diary for next closest day
                  if(!is.na(nm_t)){ 
                    nm <- nm_t
                  } 
                } 
                
                # cat ("    time slot not available, nm=", nm - per, "\n")
            } # else statement for when timeslots are not available
        } # end of while 
    } else {
        fslot <- c(fslot, NA)
        fdur_t <- c(fdur_t, NA)
        flag <- c(flag, NA)
        fday <- c(fday, NA)
        new_cluster_PUC <- c(new_cluster_PUC, NA)
        new_cluster_name <- c(new_cluster_name, NA)
        fni_list <- c(fni_list, NA)
        freq_list <- c(freq_list, NA)
        agg_fold_list <- c(agg_fold_list, NA)
        freq_float <- c(freq_float, NA)
    } # else when slots are not available 
    
    # fslot -- index of assigned timeslot
    # fdur_t -- duration in theory after aggregation (if applicable)
    # flag -- indicators of how a PUC is assigned
    # fday -- day of assigned timeslot
    # new_cluster_PUC -- PUCs from randomly selected cluster
    # new_cluster_name -- cluster name

    allslots_return <- data.frame("fslot"=fslot, 
                                  "fdur_t"=fdur_t, 
                                  "flag"=flag, 
                                  "fday"=fday, 
                                  "new_cluster_PUC"=new_cluster_PUC,
                                  "new_cluster_name"=new_cluster_name,
                                  "fni_list"=fni_list,
                                  "freq_list"=freq_list,
                                  "agg_fold_list"=agg_fold_list,
                                  "freq_float"=freq_float
                                  )

    return (allslots_return) 
} # end of all slots function
########################
#### End of allslots ###
########################

##########################
#### Beginning of IIT ####
##########################
# Function: identify idle time slots and assigned PUCs to them
#           depends on raw diary (rdiary), 
#           periodicity (days) and duration of activity (hours)

IIT <- function(idiary, per, dur, ICP_output_pc, ICP_output_cc, cluster_name=""){ 
    #################################################################
    # Inputs:
    #       idiary: a dataframe of idle diary
    #       per: periodicity of a PUC in minutes
    #       dur: duration of a PUC in hr
    #       cluster_name: name of a cluster
    #       ICP_output_pc: PUC_clusters 
    #       ICP_output_cc: collapsed_clusters
    # Outputs:
    #       allslots_output: a dataframe of assigned slots
    #################################################################
    # default return is an empty array -- failed to assign
    # it will be overwritten under conditions
    allslots_output <- data.frame()
    if (nrow(idiary)>0){
        # if it is a cluster
        if (cluster_name !=""){
            # identify the first timeslot
            CSF_out_fslot <- CSF(ICP_output_pc, ICP_output_cc, cluster_name)
            fslot_output <- fslot(idiary, per, CSF_out_fslot$dur/60)

            # if this cluster can be assigned
            if (nrow(fslot_output) > 0){
                fslot_output$new_cluster_PUC <- paste(as.character(CSF_out_fslot$new_cluster_PUC), collapse=" ")
                fslot_output$new_cluster_name <- CSF_out_fslot$new_cluster_name
                # call allslots function, with a call to fslot function as a parameter
                allslots_output <- allslots(idiary, per, dur, fslot_output, ICP_output_pc, ICP_output_cc, is_cluster=TRUE, Clusters_temp=cluster_name) 
            }
        } else{
            # identify the first timeslot
            fslot_output <- fslot(idiary, per, dur)
            # if this cluster can be assigned
            if (nrow(fslot_output) > 0){
                fslot_output$new_cluster_PUC <- NA
                fslot_output$new_cluster_name <- NA
                # call allslots function, with a call to fslot function as a parameter
                allslots_output <- allslots(idiary, per, dur, fslot_output) 
            }
        }
    }
    return(allslots_output) # return output of allslots function
  
} # end of iit function

###################
#### End of IIT ###
###################









