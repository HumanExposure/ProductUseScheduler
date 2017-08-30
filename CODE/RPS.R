# Refined Product Selector Utility Function (RSP)
# Function: Randomly select only one refined PUC per semi-refined PUC given a 
#           dataframe that includes a column named "SHEDSID" 

###########################
#### Beginning of RPS #####
###########################
RPS <- function (df) { 
    ############################################################################
    # Inputs:
    #       df: a dataframe with a column named "sheds_id"
    # Output:   
    #       df: a dataframe with selected PUCs
    ############################################################################
    
    # save column names of the original input dataframe
    cnames <- names(df) 

    # split the shedsid variable at the period "." and make a dataframe
    spuc <- do.call(rbind, strsplit(df$sheds_id, '\\.')) 

    # assign column names to spuc dataframe
    colnames(spuc) <- c("p1", "p2", "p3", "p4") 

    # merge spuc alongside original df
    df <- cbind(df, spuc) 
    df$p0 <- paste(df$p1, df$p2, df$p3, sep=".") 

    # introduce random variable
    df$rand <- runif(nrow(df)) 

    # order in descending order of random variable
    df <- df[order(-df$rand), ] 

    # keep only one observation of third level puc (p3). this effectively 
    # means a randomly assigned 4th level puc will be chosen.
    df <- df[!duplicated(df$p0), ] 

    # keep only columns of the original input dataframe 
    df <- df[cnames] 

    # go back to the original order
    df <- df[order(row.names(df)), ]  

    return (df)
}

#####################
#### End of RPS #####
#####################
