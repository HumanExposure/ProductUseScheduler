# Agent based model setup  (ABS)
# Function: Load and preprocess inputs and required libraries

###########################
#### Beginning of ABS #####
###########################
ABS <- function(wd=NULL){

    library(dplyr)
    # disable warning message
    options(warn=-1)

    if(!is.null(wd)){
        setwd(wd)
    }

    # load all modules
    R_file_pool <- c("AAD.R", "CAA.R","CAD.R","CPG.R","CSF.R","HCP.R","HUP.R",
                     "ICP.R","INACT.R","IIT.R","ISP.R","IUP.R","RID.R","RPS.R",
                     "UPG.R")
    
    for (each_R_file in R_file_pool){
        tryCatch(source(paste0("./CODE/", each_R_file)),
             error=function(e) {stop(paste0(each_R_file, " is not available"))}
                )
    }

    # population and housing generator output file
    tryCatch(
        phf <- read.csv(file="./INPUTS/pophouse_10k.csv", header=TRUE, sep=",", stringsAsFactors = FALSE), 
         error=function(e) {stop("pophouse.csv is not available")}
    )


    # ever never table
    tryCatch(
        ent <- read.csv(file="./INPUTS/FullENT.csv", header=TRUE, sep=",", stringsAsFactors = FALSE),
         error=function(e) {stop("FullENT.csv is not available")}
    )

    # Seasonality_PUC: based on ent
    Seasonality_PUC <- ent[,c("sheds_id", "sheds_product_category", "Seasonality", "WH", "Cool", "Cold")]
    Seasonality_PUC <- Seasonality_PUC[which(Seasonality_PUC$Seasonality==1), ]

    # product categories file; full list of possible PUCs
    tryCatch(
        hpf <- read.csv(file="./INPUTS/ABMCategorizations.csv", header=TRUE, sep=",", stringsAsFactors = FALSE),
        error=function(e) {stop("ABMCategorizations.csv is not available")}
    )


    hpf_sub <- select(hpf,  SHEDSID,
                            general_category,
                            product_type,
                            refined_product_type,
                            Personal_or_Communal,
                            Clusters,
                            location=Use_locale,
                            Indoor_outdoor=InOut,
                            Appliances=Appliances,
                            Impacted.by=Size_depend
                    )

    # load inputs sheds_sheds_variables
    tryCatch(
        sheds_var_raw <- read.csv("./INPUTS/PUC_use_data.csv", header=TRUE, stringsAsFactors=FALSE),
        error=function(e) {stop("PUC_use_data.csv is not available")}
    )

    # load diary RDS file
    tryCatch(
        activity_diary_pool <- readRDS("./INPUTS/activity_diary_pool.rds"),
        error=function(e) {stop("activity_diary_pool.rds is not available")}
    )
    activity_diary_pool <- filter(activity_diary_pool, Day.of.the.year>0)

    ################################################
    # Call Household Characteristics Processor (HCP)
    ################################################
    HCP_output <- HCP(phf)
    # households_list <- unique(HCP_output$household_index)


    return_variables <- list("phf"=phf,
                             "ent"=ent,
                             "hpf"=hpf_sub,
                             "sheds_var_raw"=sheds_var_raw,
                             "activity_diary"=activity_diary_pool,
                             "seasonality_PUC"=Seasonality_PUC,
                             "HCP_output"=HCP_output
                            )
    return(return_variables)
}

#####################
#### End of ABS #####
#####################

