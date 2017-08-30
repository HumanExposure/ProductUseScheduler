# Individual Sample of Products (ISP)
# Function: Use probabilistic sampling to (i) determine individual basket of 
#           products and (ii) frequency, duration, and mass of product 

###########################
#### Beginning of ISP #####
###########################
ISP <- function(sheds_var_raw, IUPoutput){
    
    ############################################################################
    # Inputs:
    #       sheds_var_raw: a dataframe converted from sheds_sheds_variables CSV
    #       IUPoutput: output from IUP module
    # Output:   
    #       ISP_output: a dataframe of PUCs at individual level with use profile
    #                   (i.e., use mass, duration, etc.)
    ############################################################################

    UPG_output_all <- data.frame()

    #  loop by each PUC 
    for (zz in 1:dim(IUPoutput)[1]){
        # extract sheds_id
        sheds_id_temp <- IUPoutput[zz, "sheds_id"]
        # extract house_size
        house_size_temp <- IUPoutput[zz, "house_size"]
        # extract person_gender
        person_gender_temp <- IUPoutput[zz, "person_gender"]
        # extract person_age
        person_age_temp <- IUPoutput[zz, "person_age"]

        # extract Personal_or_Communal
        Personal_or_Communal_temp <- IUPoutput[zz, "Personal_or_Communal"]

        # generate a single random number
        rnd <- runif(1) 

        # for each PUC, call UPG to get its use profile
        UPG_output <- UPG(sheds_id_temp, sheds_var_raw, house_size_temp, person_gender_temp, person_age_temp, Personal_or_Communal_temp, rnd)

        if (nrow(UPG_output)>0){
            UPG_output_all <- bind_rows(UPG_output_all, UPG_output)
        }
    }

    # convert characters to strings since some of them are factors for later merge
    UPG_output_all$sheds_id <- as.character(UPG_output_all$sheds_id)
    UPG_output_all$gender <- as.character(UPG_output_all$gender)
    IUPoutput$sheds_id <- as.character(IUPoutput$sheds_id)
    IUPoutput$person_gender <- as.character(IUPoutput$person_gender)

    # merge use profile to IUP output and return the joint data frame
    ISP_output <- left_join(IUPoutput, UPG_output_all, by=c("sheds_id"="sheds_id", "person_gender"="gender", "person_age"="age_person"))

    # sort data frame by household index and person index
    ISP_output <- arrange(ISP_output, household_index, person_index)

    # return rows without NA 
    ISP_output <- ISP_output[complete.cases(ISP_output),]
    return (ISP_output)
}
#####################
#### End of ISP #####
#####################

