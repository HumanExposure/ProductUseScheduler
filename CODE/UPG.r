# Use Profile Generator (UPG)
# Function: Randomly sample activity/PUC use frequency, duration, 
#           prevalence and mass


##################################
#### Beginning of ari_random #####
##################################
# Function: Takes mean and standard deviation in arithmetic and 
#           converts them to log scales then generates a random value

ari_random <- function(mean_ari, sd_ari, var_name, pctl=0){
    ####################################################################
    # Inputs:
    #       mean_ari: arithmetic mean 
    #       sd_ari: arithmetic standard deviation
    #       var_name: name of random values (i.e., mass, frequency, duration, etc.)
    #       pctl: a percentile value set as the lower end threshold
    # Outputs:
    #       df_return: a generated random number
    ###################################################################
    # if varibility is not zero
    if (sd_ari!=0){
        mean_log <- log(mean_ari/(1+(sd_ari/mean_ari**2))**0.5)
        sd_log <- (log(1+(sd_ari/mean_ari**2)))**0.5
        
        # set a default return
        random_val <- 0
        qualified_val <- 0
        if (is.finite(mean_log)){
            while (qualified_val !=1) {
                random_val <- rlnorm(1, meanlog = mean_log, sdlog = sd_log)
                min_thres <- qlnorm(pctl, meanlog = mean_log, sdlog = sd_log)
                # if the random value is greather than 3 times arithmetic mean, it will be dropped
                if (var_name %in% c("ht") && random_val < 3*mean_ari && random_val >= max(1, min_thres)){
                    qualified_val <- 1
                } else if (var_name %in% c("mass", "frequency") && random_val < 3*mean_ari && random_val >= min_thres){
                    qualified_val <- 1
                } else {qualified_val <- 0}
            }
        }
    } else{
        random_val <- mean_ari
    }
    return(random_val)
}

#############################
#### End of ari_random ######
#############################


###########################
#### Beginning of UPG #####
###########################
# Function: Generate random values as well as select point estimates
#           for parameters associate to a product use. 

UPG <- function(sheds_id, sheds_var_raw, house_size, gender, age_person, Personal_or_Communal, prev_thres=0){
    ############################################################################
    # Inputs:
    #       sheds_id: sheds_id of a PUC
    #       sheds_var_raw: a dataframe converted from sheds_sheds_variables CSV
    #       house_size: household square footage
    #       gender: gender of a household member
    #       age_person: age of a household member
    #       Personal_or_Communal: property of a PUC
    #       prev_thres: a threshold value against use_prev_val for this PUC to be selected
    # Outputs:
    #       df_return: a dataframe of generated PUC use profile
    ###########################################################################

    ###################################################
    # prevalence (% of population uses a given product)
    ###################################################
    if (as.integer(age_person)<13){
        age_group <- "children"
    } else {
        age_group <- "adult"
    }
    # select the correct prevalence value
    if (age_group == "adult" && tolower(gender) == "f") {
        # sheds_prev_select <- sheds_var_raw[, c("source.id", "source.description", "Prev_F")]
        sheds_prev_select <- sheds_var_raw[, c("PUCID_refined", "New.Description", "Prev_F")]

    } else if (age_group == "adult" && tolower(gender) == "m") {
        sheds_prev_select <- sheds_var_raw[, c("PUCID_refined", "New.Description", "Prev_M")]

    } else if (age_group == "children") {
        sheds_prev_select <- sheds_var_raw[, c("PUCID_refined", "New.Description", "Prev_child")]

    }

    use_prev_select <- sheds_prev_select[which(sheds_prev_select$PUCID_refined==sheds_id),]
    use_prev_val <- use_prev_select[,3] 

    if (use_prev_val<prev_thres){
        df_return <- data.frame()
    } else{
        if (Personal_or_Communal=="Communal"){
            # based on house size select a percentile value as a threshold  
            if (house_size<=1200){
                pctl <- 0
            } else if (house_size>=2501){
                pctl <- 0.67
            } else{
                pctl <- 0.34
            }
        } else{
            pctl <- 0
        }
        
        sheds_var_raw <- ungroup(sheds_var_raw)
        product_type_select <- sheds_var_raw[which(sheds_var_raw$PUCID_refined==sheds_id),]

        ###################################################
        # mass (g)
        ###################################################
        use_mass_select_mean <- product_type_select$Mass
        use_mass_select_sd <- use_mass_select_mean*product_type_select$Mass_CV
        use_mass_val <- ari_random(use_mass_select_mean, use_mass_select_sd, "mass", pctl)

        ###################################################
        # frequency (1/year)
        ###################################################
        use_freq_select_mean <- product_type_select$Freq
        use_freq_select_sd <- use_freq_select_mean*product_type_select$Freq_CV
        use_freq_val <- ari_random(use_freq_select_mean, use_freq_select_sd, "frequency", pctl)

        #########################################################
        # handling time, activity time and associate time (mins)
        #########################################################
        use_ht_select_mean <- product_type_select$HT
        use_ht_select_sd <- use_ht_select_mean*product_type_select$HT_CV
        use_ht_val <- ari_random(use_ht_select_mean, use_ht_select_sd, "ht", pctl)
        use_act_val <- product_type_select$AT
        use_aso_val <- use_ht_val + use_act_val

        df_return <- data.frame("sheds_id" = as.character(sheds_id),
                                "use_mass" = use_mass_val, 
                                "use_freq" = use_freq_val, 
                                "use_ht"  = use_ht_val, 
                                "use_act"  = use_act_val, 
                                "use_aso"  = use_aso_val, 
                                "use_prev" = use_prev_val,
                                "gender" = gender,
                                "age_person" = age_person,
                                stringsAsFactors = FALSE)
    }

    return(df_return)      
}

######################
#### End of UPG ######
######################




