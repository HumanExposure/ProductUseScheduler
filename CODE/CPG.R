# Communal PUC Generators (CPG)
# Function: Identify communal PUCs at the household level

###########################
#### Beginning of CPG #####
###########################
CPG <- function(PUC_household){
    
    #############################################################################
    # Inputs:
    #   PUC_household: a dataframe of PUCs used at household level
    # Outputs:
    #   household_communal_uni: a dataframe with communal PUCs at household level
    #   PUC_household_Step_3: a dataframe with both updated PUCs 
    #                         (both personal and commnual) at household level
    #############################################################################
    
    # Split household PUCs into personal and communal levels. note: we remove 
    # Communal PUC responsibilities for people age <13
    PUC_household_personal <- filter(PUC_household, Personal_or_Communal=="Personal")
    PUC_household_communal <- filter(PUC_household, Personal_or_Communal=="Communal", person_age>12)

    # Call Refined Product Selector Utility Function (RPS) module to 
    # select only one refinery PUC from the same product type for communal PUCs
    PUC_household_communal_uniq <- RPS(PUC_household_communal)

    # Select a few necessary columns from PUC_household_communal_uniq
    PUC_household_communal_uniq <- select(PUC_household_communal_uniq, sheds_id, 
                                          product_type, use_mass, use_freq, use_aso, 
                                          use_prev, Personal_or_Communal, Indoor_outdoor,
                                          Clusters)

    # rename variables by adding a prefix "new_"
    names(PUC_household_communal_uniq) <- paste0("new_", names(PUC_household_communal_uniq))

    # Merge the PUC_household_communal_uniq back to PUC_household_communal. 
    # Note: this merge/overwrite is only done for communal products. 
    # Step 1: in the newly merged dataframe, it has two use profiles:
    #         the first ones are created by sampling each person, 
    #         the second ones are created by picking a PUC's use profile for everything  
    #         else within the same product type.
    #         This means that if 4 people within the same household are using 4 PUCs 
    #         from the same product type, we will randomly pick one person's use habit 
    #         and ask the other 3 follows the "winner's" use profile. This is per Paul's
    #         suggestion "only one PUC from the same product type will be selected".

    PUC_household_Step_1 <- left_join(PUC_household_communal, PUC_household_communal_uniq, 
                                      by=c("product_type"="new_product_type"))

    # sort by product_type, and sheds_id
    PUC_household_Step_1 <- arrange(PUC_household_Step_1, product_type, sheds_id)

    # Step 2: Update the unselected PUCs since that person still uses a PUC.
    #         However, based on the rule "only PUCs from the same 
    #         refinery category will be selected"

    PUC_household_Step_2 <- select(PUC_household_Step_1, sheds_id = new_sheds_id,
                                                         household_index, person_index,
                                                         person_gender, person_age,
                                                         house_size, product_type,
                                                         use_mass = new_use_mass,
                                                         use_freq = new_use_freq,
                                                         use_aso = new_use_aso,
                                                         use_prev = new_use_prev,
                                                         Personal_or_Communal = new_Personal_or_Communal,
                                                         Clusters = new_Clusters,
                                                         Indoor_outdoor = new_Indoor_outdoor
                                  )

    # Step 3: Combine communal and Personal PUCs 
    PUC_household_personal_sub <- select(PUC_household_personal, sheds_id, household_index, 
                                         person_index, person_gender, person_age, house_size, 
                                         product_type, use_mass, use_freq, use_aso, use_prev,
                                         Personal_or_Communal, Clusters, Indoor_outdoor)

    PUC_household_Step_3 <- rbind(PUC_household_Step_2, PUC_household_personal_sub)

    # Select communal PUCs used by people whose ages are >12
    # this means that we assume people <= 12 will not carry responsibility of 
    # applying household PUCs. 
    household_communal <- filter(PUC_household_Step_2, Personal_or_Communal=="Communal", person_age>12)

    # Count number of males and females
    gender_pivot <- household_communal %>% distinct(person_index, person_gender)
    n_male_use <- nrow(gender_pivot[gender_pivot$person_gender=="M",])
    n_female_use <- nrow(gender_pivot[gender_pivot$person_gender=="F",])
    
    # Calculate communal PUCs' characters based on the following rules: 
    # among the same PUC, find
    # 1. longest duration as the household duration (mins)
    # 2. smallest freq as the household freq (1/year)
    household_communal <- group_by(household_communal, sheds_id)
    household_communal <- mutate(household_communal, c_use_aso = max(use_aso), 
                                                     c_use_freq = min(use_freq)
                                )

    # drop the group property to improve speed
    household_communal <- ungroup(household_communal)

    # in order to recalculate use freq, need to find the number of people using the communal PUCs by gender
    household_communal$n_male_use <- n_male_use
    household_communal$n_female_use <- n_female_use
    household_communal <- arrange(household_communal, sheds_id)

    # remove duplicated ones
    household_communal_uni <- household_communal[!duplicated(household_communal$sheds_id), ]

    return (list("household_communal_uni"=household_communal_uni, "PUC_household"=PUC_household_Step_3))
}

#####################
#### End of CPG #####
#####################