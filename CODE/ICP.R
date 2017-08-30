# Individual Cumulative Products (ICP)
# Function: Expand ISP PUCs to include communal household activity PUCs 
#           based on other household member's usage. Adjusts individual 
#           frequencies for common activities based on adult family labor 
#           division rule

###########################
#### Beginning of ICP #####
###########################
ICP <- function(PUC_household, household_communal_uni, person_index_str){
    ############################################################################
    # Inputs:
    #       PUC_household: a dataframe of PUCs at household level
    #       household_communal_uni: a dataframe of communal PUCs within household
    #       person_index_str: a string, index of an individual with a household
    # Output:   
    #       ICP_person_temp: a dataframe with PUCs after adjusting 
    #                        usages for communal PUCs at indivudial level
    ############################################################################

    # subset based on person index
    ICP_person_temp <- filter(PUC_household, person_index==person_index_str)

    # get a person's age
    age_temp <- ICP_person_temp$person_age[1]
    gender_temp <- ICP_person_temp$person_gender[1]

    # If that person is >12 years, and it is not the only household member,
    # we need to add household communal PUCs to his/her PUC lists.
    # This means people in the same household will share
    # the same responsibilities on certain PUCs
    
    # Step 1: find not included communal PUCs (communal PUCs used by others)
    additional_communal_PUC <- household_communal_uni[!household_communal_uni$sheds_id %in% ICP_person_temp$sheds_id,]

    if (age_temp>12 && nrow(additional_communal_PUC)>0) {
        # remove unused columns and overwrite person characteristics.  
        # basically, we "borrow" communal PUCs' use profiles from others if not 
        # included previously
        additional_communal_PUC_sub <- as.data.frame(select(additional_communal_PUC, -c(c_use_aso, c_use_freq, n_male_use, n_female_use)))
        additional_communal_PUC_sub$person_index <- person_index_str
        additional_communal_PUC_sub$person_gender <- gender_temp
        additional_communal_PUC_sub$person_age <- age_temp

        # Step 2: add them to his/her PUC
        ICP_person_temp <- rbind(ICP_person_temp, additional_communal_PUC_sub)
    }

    ICP_person_temp$new_freq <- 0
    ICP_person_temp$new_aso <- 0
    total_person_temp_puc <- dim(ICP_person_temp)[1]

    # loop through a person's PUCs and calculate new freq and aso if it is a 
    # communal PUC and that person's age>=12
    for (zz in 1:total_person_temp_puc){
        
        ICP_person_temp_row_temp <- ICP_person_temp[zz,]
        # extract a user's gender, age
        # extract PUC id, and its property such as Personal/Communal, indoor/outdoor
        PUC_temp <- ICP_person_temp_row_temp$sheds_id
        P_C_temp <- ICP_person_temp_row_temp$Personal_or_Communal
        I_O_temp <- ICP_person_temp_row_temp$Indoor_outdoor
        
        if (P_C_temp == "Communal" && age_temp >12){
            household_communal_temp <- filter(household_communal_uni, sheds_id==PUC_temp)
            # we assume male will use 80% of outdoor communal PUCs and 
            # female will use 80% of indoor communal PUCs
            if (I_O_temp == "O" && gender_temp == "M"){
                new_freq <- 0.8 * household_communal_temp$c_use_freq/household_communal_temp$n_male_use
            } else if (I_O_temp == "O" && gender_temp == "F"){
                new_freq <- 0.2 * household_communal_temp$c_use_freq/household_communal_temp$n_female_use
            } else if (I_O_temp == "I" && gender_temp == "M"){
                new_freq <- 0.2 * household_communal_temp$c_use_freq/household_communal_temp$n_male_use
            } else if (I_O_temp == "I" && gender_temp == "F"){
                new_freq <- 0.8 * household_communal_temp$c_use_freq/household_communal_temp$n_female_use
            }
            new_aso <- household_communal_temp$c_use_aso
        } else{
            new_freq <- ICP_person_temp[zz,]$use_freq
            new_aso <- ICP_person_temp[zz,]$use_aso
        }
        ICP_person_temp[zz,]$new_freq <- new_freq
        ICP_person_temp[zz,]$new_aso <- new_aso
    }

    return(ICP_person_temp)
}

#####################
#### End of ICP #####
#####################




