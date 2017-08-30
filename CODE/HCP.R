# Household Characteristics Processor (HCP)
# Function: Expand the population housing generator file to create one line per member

###########################
#### Beginning of HCP #####
###########################
HCP <- function(pophouse, file_csv=NULL){
    
    ###########################################################
    # Inputs:
    #       pophouse: a dataframe converted from pophouse CSV
    # Outputs: 
    #       pophouse_output: a dataframe where each line represents
    #       a household member with house and person features
    ###########################################################
    
    ## initialize a variable to hold outputs
    pophouse_output <- data.frame()

    # begin to process each household in pophouse
    for (k in 1:dim(pophouse)[1]){
        pophouse2_sub <- pophouse[k,]
        # main person is defined as the first in Col A of RPGen output
        main_person_gender <- strsplit(pophouse2_sub$gender, "")[[1]][1]
        main_person_age <- pophouse2_sub$age_years

        # extract region 
        # first need to add a leading zero if it is a 6 digit
        compid_t <- formatC(pophouse2_sub$compid, width=7, flag="0")
        # extract the first two digits
        state_t <- substring(compid_t, 1, 2)
        # Region 1 to 4 state codes
        r1states <- c("09","23","25","33","34","36","42","44","50")
        r2states <- c("17","18","19","20","26","27","29","31","38","39","46","55")
        r3states <- c("01","05","10","11","12","13","21","22","24","28","37","40","45","47","48","51","54")
        r4states <- c("02","04","06","08","15","16","30","32","35","41","49","53","56")

        region<-NA # default value if state is not in regions 1-4
      
        if (state_t %in% r1states) {region=1}
        if (state_t %in% r2states) {region=2}
        if (state_t %in% r3states) {region=3}
        if (state_t %in% r4states) {region=4}

        # extract house size (square footage, 99-99998)
        pophouse_unitsf <- pophouse2_sub$unitsf

        # extract yard size (square footage, 200 - 999,997), but has many -6
        pophouse_yard <- pophouse2_sub$lot

        # extract the number of bathrooms (0-10, -6=NA)
        pophouse_baths <- pophouse2_sub$baths

        # number of household people
        # pophouse_ppl <- nchar(strsplit(pophouse2_sub$genders, "[.]")[[1]][1])

        # extract genders and count number of people in a household
        household_genders_str <- strsplit(pophouse2_sub$genders, "[.]")[[1]][1]
        household_genders <- strsplit(household_genders_str, "")[[1]]
        pophouse_ppl <- length(household_genders)

        # age--(age in full years, rounded down, ange=0 to 96)
        # based on http://stackoverflow.com/a/11619681/1231509

        # extract ages
        household_ages_str <- strsplit(pophouse2_sub$ages, "[.]")[[1]][1]
        household_ages_str2 <- strsplit(household_ages_str, "")[[1]]
        # concatenate two digits to age
        household_ages <- paste0(household_ages_str2[c(TRUE, FALSE)], household_ages_str2[c(FALSE, TRUE)])
        household_ages <- as.numeric(household_ages)
        # create a dataframe to hold gender and age
        age_gender <- data.frame(age=household_ages, gender=household_genders)
        # need to convert gender from factor to characters
        age_gender[, "gender"] <- sapply(age_gender[, "gender"], as.character)

        main_person_index <- which(age_gender$age==main_person_age & age_gender$gender==main_person_gender)[1]
        age_gender$main_person <- 0
        age_gender$main_person[main_person_index] <- 1
        # list the main person to the top of the list
        age_gender_reoder <- age_gender[order(-age_gender$main_person),]

        # number of cars--number of cars (0-5, -6=NA)
        pophouse_cars <- pophouse2_sub$cars

        # dishwash--dishwasher used in home (0=no, 1=yes)
        pophouse_dishwash <- pophouse2_sub$dishwash

        # cwasher-- clothes washer used in home (0=no, 1=yes)
        pophouse_cwasher <- pophouse2_sub$cwasher

        # dryer-- clothes dryer used in home (0=no, 1=yes)
        pophouse_dryer <- pophouse2_sub$dryer

        # swim--swimming pool or hot tub
        # 0=none, 1=hot tub only, 2=pool only, 3=both
        pophouse_swim <- pophouse2_sub$swim

        # own or rent house (1=owned, 2=rented, 3 = stay without rent)
        pophouse_ownrent <- pophouse2_sub$kownrent

        # A person's ethnicity
        # W=White, B=Black, N=Native American, A=Asian
        # P=Pacific Islander, O=Other, M=Multiple  
        ethnicity <- pophouse2_sub$race

        # House type is back-calculated from Col "pool" using the following eq
        # The house types are 1= single family (attached or detached), 
        # 2 = multi-family apartment building, 
        # 3 = mobile home or other.
        # Calculate mod(pool,36), If this is in the range 0-11, 
        # then house_type=1; if 12-23 then house_type=2; or if 24-35 then house_type=3.
        pophouse_housetype_Raw <- pophouse2_sub$pool%%36   # remainder 

        if ((pophouse_housetype_Raw) >=0 && (pophouse_housetype_Raw) <=11){
            pophouse_housetype <- "Single family"
        } else if ((pophouse_housetype_Raw) >=12 && (pophouse_housetype_Raw) <=23){
            pophouse_housetype <- "Multi-family apartment building"
        } else if ((pophouse_housetype_Raw) >=24 && (pophouse_housetype_Raw) <=35){
            pophouse_housetype <- "Mobile home or other"
        }

        # return data frame
        pophouse_df_t <- data.frame("household_index" = k,
                                    "ownrent" = pophouse_ownrent,
                                    "region" = region,
                                    "n_ppl_household" = pophouse_ppl,
                                    "person_index" = NA,
                                    "person_age" = NA,
                                    "person_gender" = NA,
                                    "person_ethnicity" = ethnicity,
                                    "house_type" = pophouse_housetype,
                                    "house_size" = pophouse_unitsf,
                                    "yard_size" = pophouse_yard,
                                    "n_bath" = pophouse_baths,
                                    "n_car" = pophouse_cars,
                                    "w_dishwash" = pophouse_dishwash,
                                    "w_cwasher" = pophouse_cwasher,
                                    "w_dryer" = pophouse_dryer,
                                    "w_swim" = pophouse_swim,
                                    stringsAsFactors = FALSE
                                    )

        # replicate the row for each member of a household
        pophouse_df_t2 <- pophouse_df_t[rep(1, each=pophouse_ppl),]

        pophouse_df_t2$person_index[1:pophouse_ppl] <- 1:pophouse_ppl
        pophouse_df_t2$person_age[1:pophouse_ppl] <- age_gender_reoder$age
        pophouse_df_t2$person_gender[1:pophouse_ppl] <- age_gender_reoder$gender
        pophouse_output <- bind_rows(pophouse_output, pophouse_df_t2)
    }

    # if an output file name is supplied, will save as an output file
    if(!is.null(file_csv)){
        write.csv(pophouse_output, file_csv, row.names=FALSE)
    }

    return(pophouse_output)
}
#####################
#### End of HCP #####
#####################

# pophouse <- read.csv(file="D:/Dropbox/_ICF_project/WA 2-75/Agent-Based Models/Modular_Structure/INPUTS/pophouse.csv", header=TRUE, sep=",", stringsAsFactors = FALSE) 


### not use for now###
# dryruse-- frequency clothes dryer used
# (1=every time clothes washed, 
# 2=sometimes when clothes washed, 
# 3=rarely, 
# -2=NA)
# pophouse_dryruse <- pophouse2_sub$dryruse

# washload-- frequency clothes washer used 
# (1=1/week or less, 
# 2=2-4 per week, 
# 3=5-9 per week, 
# 4=10-15 per week, 
# 5= 16+ per week)
# pophouse_washload <- pophouse2_sub$washload
### not use for now###

