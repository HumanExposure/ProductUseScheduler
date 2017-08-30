# Clustered Activity Disaggregator (CAD)
# Function: Disaggregate activities that occur in a cluster 

###########################
#### Beginning of CAD #####
###########################
CAD <- function(rdiary, cluster_df){  
    ###########################################################
    # Inputs:
    #       rdiary: a dataframe of a person's diary
    #       cluster_df: a dataframe cluster PUCs
    # Outputs:
    #       rdiary_final: a dataframe of processed diary
    ###########################################################
    
    # extract PUC clusters to be expanded
    rdiary_cl <- rdiary[which(rdiary$Activity.Code==-2 & is.na(rdiary$sheds_id)),]

    # assigned PUC but not clusters
    rdiary_non_cl1 <- rdiary[which(rdiary$Activity.Code==-2 & !is.na(rdiary$sheds_id)),]

    # none idle time block
    rdiary_non_cl2 <- rdiary[which(rdiary$Activity.Code!=-2),]

    # if cluster PUCs has been assigned to a dirary
    if (nrow(rdiary_cl)>0){
        rdiary_cl_expand <- data.frame()
        for (kk in 1:nrow(rdiary_cl)){
            # target_cluster
            rdiary_cl_expand_t <- data.frame()
            cluster_temp <- rdiary_cl[kk,]

            # extract sheds_id from a randomly sampled cluster
            cluster_comp <- strsplit(cluster_temp$new_cluster_PUC, " ")[[1]]
            # if this slot is truncated, this ratio is <1
            duration_ratio <- 1

            # cluster composition
            cluster_temp1 <- cluster_df[cluster_df$sheds_id %in% cluster_comp,]

            for (q in 1:nrow(cluster_temp1)){
                cluster_temp2 <- cluster_temp
                if (q==1){
                    cluster_temp2$Start.Time.hr.using.military.time <- cluster_temp$Start.Time.hr.using.military.time
                } else{
                    cluster_temp2$Start.Time.hr.using.military.time <- rdiary_cl_expand_t[q-1, "Start.Time.hr.using.military.time"]+cluster_temp1[q-1, "new_aso"]*duration_ratio/60
                }

                cluster_temp2$Duration.hr <- cluster_temp1[q, "new_aso"]*duration_ratio/60
                cluster_temp2$Duration.min <- cluster_temp1[q, "new_aso"]*duration_ratio
                cluster_temp2$sheds_id <- cluster_temp1[q, "sheds_id"]
                cluster_temp2$product_type <- cluster_temp1[q, "product_type"]
                rdiary_cl_expand_t <- bind_rows(rdiary_cl_expand_t, cluster_temp2)
                }
            rdiary_cl_expand <- bind_rows(rdiary_cl_expand, rdiary_cl_expand_t)
            }

        rdiary_cl_expand <- mutate(rdiary_cl_expand, ending_time = Start.Time.hr.using.military.time+Duration.hr)
        rdiary_final <- bind_rows(rdiary_non_cl1, rdiary_non_cl2, rdiary_cl_expand)
    } else{
        rdiary_final <- rdiary
    }
    
    rdiary_final <- arrange(rdiary_final, Day.of.the.year, Start.Time.hr.using.military.time)
  
    return(rdiary_final) # function returns the expanded disaggregated individual diary
  
} # end of function

######################
#### End of CAD ######
######################
