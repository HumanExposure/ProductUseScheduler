# Clustered Activity Aggregator (CAA)
# Function: Aggregate activities that occur in a cluster and compute 
#           cluster frequencies and durations

###########################
#### Beginning of CAA #####
###########################
CAA <- function(ilist){ 
    
    #########################################################################
    # Inputs:
    #       ilist: a dataframe corresponding to an individual's list of PUCs 
    #        (with data on duration, freq, etc)
    # Outputs:
    #       PUC_clusters: a dataframe with PUCs that are part of clusters
    #       collapsed_clusters: a dataframe with non-cluster PUCs and collapsed cluster PUCs
    #       indi_puc_new_freq: a dataframe of both cluster and non-cluster PUCs except that 
    #            frequencies of PUCs from the same cluster are set to the same
    #########################################################################

    # db1 contains those PUCs that are not in clusters
    db1 <- ilist[ilist$Clusters==""|is.na(ilist$Clusters),] 

    # db2 contains those PUCs that are part of clusters
    db2 <- ilist[!(ilist$Clusters==""|is.na(ilist$Clusters)),] 

    # temporally replace a mixture of indoor/outdoor PUCs to indoor
    unique_painting_io <- unique(unlist(db2[(db2$Clusters=="Painting"), "Indoor_outdoor"]))
    if (length(unique_painting_io) > 1){
        db2[(db2$Clusters=="Painting"), "Indoor_outdoor"]<- "I"
    }

    # maxdur collapses the clusters to extract the maximum duration
    sumdur <- aggregate(new_aso ~ Clusters, data=db2, FUN=sum) 

    # maxfreq collapses the clusters to extract the max frequency
    maxfreq <- aggregate(new_freq ~ Clusters, data=db2, FUN=max) 

    # personal or communal
    PoC <- aggregate(Personal_or_Communal ~ Clusters, data=db2, FUN=unique) 

    # Indoor_outdoor
    IoO <- aggregate(Indoor_outdoor ~ Clusters, data=db2, FUN=unique) 

        
    # db3 contains the merge of sumdur and maxfreq
    db3 <- Reduce(function(...) merge(..., by='Clusters', all.x=TRUE), 
                  list(sumdur, maxfreq, PoC, IoO))

    # find those names in db2 that are not in db3
    names_to_na <- names(db2)[!(names(db2) %in% names(db3))] 

    # add columns equal to NA for columns which are not suitable for a cluster
    db3[names_to_na] <- NA 
    db3$household_index <- db1$household_index[1]
    db3$person_index <- db1$person_index[1]
    db3$person_gender <- db1$person_gender[1]
    db3$person_age <- db1$person_age[1]
    db3$house_size <- db1$house_size[1]

    # join db1 (non-cluster PUCs) with db3 (collapsed clusters)
    db4 <- rbind(db1, db3) 

    # based on db2 but using cluster freq
    db5 <- left_join(select(db2, -c(new_freq)), select(db3, c(Clusters, new_freq)), by="Clusters")

    # used to check how PUCs are assigned
    db6 <- rbind(db1, db5)

    # output processed dataframe as well as original list of cluster pucs for later expansion in CAD
    res <- list("collapsed_clusters"=db4, "PUC_clusters"=db2, "indi_puc_new_freq"=db6)

    return(res) 
}

#####################
#### End of CAA #####
#####################






