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
  
  # loop by each PUC 
  for (zz in 1:dim(IUPoutput)[1]){
    # extract sheds_id
    sheds_id_temp <- IUPoutput[zz, "PUCID_productype"]
    # extract house_size
    house_size_temp <- IUPoutput[zz, "house_size"]
    # extract person_gender
    person_gender_temp <- IUPoutput[zz, "person_gender"]
    # extract person_age
    person_age_temp <- IUPoutput[zz, "person_age"]
    # extract Personal_or_Communal
    personal_or_Communal_temp <- IUPoutput[zz, "personal_communal"]
    # extract Cluster
    cluster_temp <- IUPoutput[zz, "cluster"]

    if ((cluster_temp==0)|is.null(cluster_temp)|is.na(cluster_temp)){
      cluster_temp <- ""
    }
        
    # get a random number to decide which person will use this personal product type
    rnd <- runif(1) 
    
    if (as.integer(person_age_temp)<13){
      age_group <- "children"
    } else {
      age_group <- "adult"
    }
    
    gender<-person_gender_temp
    
    # select the correct prevalence value
    if (age_group == "adult" && tolower(gender) == "f") {
      # sheds_prev_select <- sheds_var_raw[, c("source.id", "source.description", "Prev_F")]
      sheds_prev_select <- sheds_var_raw[, c("PUCID_productype","PUCID_refined",  "refined","New.Description", "Prev_F")]
      
    } else if (age_group == "adult" && tolower(gender) == "m") {
      sheds_prev_select <- sheds_var_raw[, c("PUCID_productype","PUCID_refined", "refined", "New.Description", "Prev_M")]
      
    } else if (age_group == "children") {
      sheds_prev_select <- sheds_var_raw[, c("PUCID_productype","PUCID_refined", "refined","New.Description", "Prev_child")]
      
    }
    
    use_prev_select <- sheds_prev_select[which((sheds_prev_select$PUCID_productype==sheds_id_temp)&(sheds_var_raw$refined==0)),]
    use_prev_val <- use_prev_select[,5] 
 
    if (is.null(use_prev_val)|is.na(use_prev_val)|use_prev_val<rnd){# if the product type is NOT selected for the person return UPG output as empty dataframe
      UPG_output <-data.frame()
    }
    
    else { # if the product type is selected for the person compute its use profile by calling UPG
    
    # get all the refined PUCs
    sheds_var_raw_refined_t <- sheds_var_raw[(sheds_var_raw$PUCID_productype==sheds_id_temp) & (sheds_var_raw$refined==1), ]
    
    # randomly pick one
    sheds_id_temp_refined <- sheds_var_raw_refined_t[sample(nrow(sheds_var_raw_refined_t), 1), ]
    sheds_id_temp_refined <- sheds_id_temp_refined$PUCID_refined[1]
    
    # for each PUC, call UPG to get its use profile. Set rnd to 0 to force selection of the refined PUC because the selection is determined here in ISP and must not be left to UPG.
    UPG_output <- UPG(sheds_id_temp_refined, sheds_var_raw, house_size_temp, person_gender_temp, person_age_temp, personal_or_Communal_temp, 0)
    
    if (nrow(UPG_output)>0){
      # UPG_output$PUCID_productype<-PUCID_PT
      UPG_output$cluster <- cluster_temp
    }
      
    } # end else for when PT is selected
    
      UPG_output_all <- bind_rows(UPG_output_all, UPG_output)
    } # end for over all IUP items
  

  
  # convert characters to strings since some of them are factors for later merge
  UPG_output_all$sheds_id <- as.character(UPG_output_all$sheds_id)
  UPG_output_all$gender <- as.character(UPG_output_all$gender)
  UPG_output_all$cluster <- as.character(UPG_output_all$cluster)
  # 
  # # merge use profile to IUP output and return the joint data frame
  # ISP_output <- left_join(IUPoutput, UPG_output_all, by=c("PUCID_productype"="PUCID_productype", "person_gender"="gender", "person_age"="age_person"))
  # 
  # # sort data frame by household index and person index
  # ISP_output <- arrange(ISP_output, household_index, person_index)
  # 
  # # return rows without NA 
  # ISP_output <- ISP_output[complete.cases(ISP_output),]
  ISP_output<-UPG_output_all[complete.cases(UPG_output_all),]
  return (ISP_output)
}
#####################
#### End of ISP #####
#####################

