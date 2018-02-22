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
    #       db2: a dataframe with PUCs that are part of clusters
    #       db4: a dataframe with non-cluster PUCs and collapsed cluster PUCs
    #       db6: a dataframe of both cluster and non-cluster PUCs except that 
    #            frequencies of PUCs from the same cluster are set to the same
    #########################################################################

    # db1 contains those PUCs that are not in clusters
    db1 <- ilist[ilist$Clusters==""|is.na(ilist$Clusters),] 

    # db2 contains those PUCs that are part of clusters
    db2 <- ilist[!(ilist$Clusters==""|is.na(ilist$Clusters)),] 

    # maxdur collapses the clusters to extract the maximum duration
    sumdur <- aggregate(use_aso ~ Clusters, data=db2, FUN=sum) 

    # maxfreq collapses the clusters to extract the maximum frequency
    maxfreq <- aggregate(use_freq ~ Clusters, data=db2, FUN=max) 

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
    db3gender <- db1$gender[1]
    db3$age_person <- db1$age_person[1]
    
    # join db1 (non-cluster PUCs) with db3 (collapsed clusters)
    db4 <- rbind(db1, db3) 

    # based on db2 but using cluster freq
    db5 <- left_join(select(db2, -c(use_freq)), select(db3, c(Clusters, use_freq)), by="Clusters")

    # used to check how PUCs are assigned
    db6 <- rbind(db1, db5) 

    # output processed dataframe as well as original list of cluster pucs for later expansion in CAD
    res <- list("collapsed_clusters"=db4, "PUC_clusters"=db2, "indi_puc_new_freq"=db6)

    return(res) 
}

#####################
#### End of CAA #####
#####################






