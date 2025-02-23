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
    R_file_pool <- c("HCP.R", "HUP.R", "UPG.R", "CPS.R", "IUP.R", "ISP.R", 
                     "CAA.R", "RID.R", "INACT.R", "IIT.R", "AAD.R", "CAD.R", "CSF.R")

    for (each_R_file in R_file_pool){
        tryCatch(source(paste0("./CODE/", each_R_file)),
             error=function(e) {stop(paste0(each_R_file, " is not available"))}
                )
    }

    # population and housing generator output file
    tryCatch(
        phf <- read.csv(file="./INPUTS/pophouse.csv", header=TRUE, sep=",", stringsAsFactors = FALSE), 
         error=function(e) {stop("pophouse.csv is not available")}
    )


    # ever never table
    tryCatch(
        ent <- read.csv(file="./INPUTS/FullENT.csv", header=TRUE, sep=",", stringsAsFactors = FALSE),
         error=function(e) {stop("FullENT.csv is not available")}
    )
    # rename ent columns from pretty input names to code names
    names(ent)<-c("PUCID_productype","PUCID_PT_description","NO2017","HUP","IUP","Residential","Demographic","Seasonality","M","F","W","B","A","N","P","O","age0_5","age6_12","age13_15","age16_18","age19_49","age50plus","kownrent","sewdis","dishwash","stoven","cars","pcprint","swim","yard","Hot_Warm","Cool","Cold","personal_communal") 

    # Seasonality_PUC: based on ent
    Seasonality_PUC <- ent[,c("PUCID_productype", "Seasonality", "Hot_Warm", "Cool", "Cold")]
    Seasonality_PUC <- Seasonality_PUC[which(Seasonality_PUC$Seasonality==1), ]

    # load inputs sheds_sheds_variables
    tryCatch(
        sheds_var_raw <- read.csv("./INPUTS/PUC_use_data.csv", header=TRUE, stringsAsFactors=FALSE),
        error=function(e) {stop("PUC_use_data.csv is not available")}
    )

    product_refine_tbl <- filter(sheds_var_raw, refined==1) %>% select(PUCID_productype, PUCID_refined) %>% 
                          left_join(select(ent, PUCID_productype, PUCID_PT_description), by="PUCID_productype")

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


    return_variables <- list("phf"=phf,
                             "ent"=ent,
                             "sheds_var_raw"=sheds_var_raw,
                             "product_refine_tbl"=product_refine_tbl,
                             "activity_diary"=activity_diary_pool,
                             "seasonality_PUC"=Seasonality_PUC,
                             "HCP_output"=HCP_output
                            )
    return(return_variables)
}

#####################
#### End of ABS #####
#####################

