# Cluster Sampling Function (CSF)
# Function:  
# (i) draw a random set of PUCs from the cluster assuming that the most frequent PUC always occurs and 
# (ii) each of the remaining PUCs in the cluster will be probabilistically sampled in the ratio of its frequency to the most frequent PUC frequency. So if the most frequent PUC item 1 has a frequency f1 then item 2 will be sampled with probability = f2/f1
# (iii) compute cluster duration as sum of durations of chosen PUCs. 
# CSF will return: 
# (1) cluster duration 
# (2) cluster composition list (chosen PUC IDs appended together; and 
# (3) cluster duration list (duration of chosen PUCs appended together)


###########################
#### Beginning of CSF #####
###########################
CSF <- function(ICP_output_pc, ICP_output_cc, cluster_name){  
    
    ##########################################
    # all PUCs for a cluster before being sampled
    Cluster_bsampled_pc <- ICP_output_pc[ICP_output_pc$Clusters==cluster_name,]

    max_freq_PUC <- Cluster_bsampled_pc[which(Cluster_bsampled_pc$new_freq==max(Cluster_bsampled_pc$new_freq)),]
    non_freq_PUC <- Cluster_bsampled_pc[which(Cluster_bsampled_pc$new_freq!=max(Cluster_bsampled_pc$new_freq)),]

    # if not all of the component PUCs have the same freq
    if (nrow(non_freq_PUC)>0){
        # add a frequency ratio between non-max freq VS max freq
        non_freq_PUC$freq_ratio <- non_freq_PUC$new_freq/max(max_freq_PUC$new_freq)

        # generate a random number
        non_freq_PUC$rand <- runif(nrow(non_freq_PUC))
        max_freq_PUC$freq_ratio <- 1
        max_freq_PUC$rand <- 0
        
        # randomly pick PUCs by comparing rand against freq_ratio
        Cluster_sampled_pc <- non_freq_PUC[non_freq_PUC$rand<=non_freq_PUC$freq_ratio, ]
    } else{
        Cluster_sampled_pc <- data.frame()
    }

    # cluster after sampling, update pc
    Cluster_afsampled_pc <- bind_rows(max_freq_PUC, Cluster_sampled_pc)

    # new cluster name, duration and compositions
    new_cluster_name <- paste0(Cluster_afsampled_pc$Clusters[1], "--", 
                               paste(sample(letters, 8, replace=TRUE), collapse=""))

    Cluster_afsampled_pc$new_cluster_name <- new_cluster_name

    new_aso <- sum(Cluster_afsampled_pc$new_aso)
    new_cluster_PUC <- Cluster_afsampled_pc$sheds_id

    res <- list("dur"=new_aso,
                "new_cluster_PUC"=new_cluster_PUC,
                "new_cluster_name"=new_cluster_name)

    return(res)
}
#####################
#### End of CSF #####
#####################