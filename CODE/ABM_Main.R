#############################################
# ABM is executed by calling ABM_Runner() 
##############################################


###################################
#### Beginning of ABM_Runner ######
###################################
ABM_Runner <- function(pth_str=NULL, household_number=3){
    ############################################################################
    # Inputs:
    #       pth_str: a working folder with the following three sub-folders:
    #                INPUTS, CODE, OUTPUTS
    #       household_number: a vector of house ID you are interested, this  
    #                         vector only contains one number now
    #       random_seed: number of a random seed
    ############################################################################

    pth_str <- "D:/Dropbox/_ICF_project/WA 2-75/Agent-Based Models/Modular_Structure/ProductUseScheduler/"
    household_number=1


    # check inputs
    if(is.null(pth_str)){
        stop("Please specify a working directory")
    }

    ##################
    # setup ABM inputs
    ##################
    setwd(pth_str)
    tryCatch(source("./CODE/ABS.R"),
         error=function(e) {stop("ABS.R is not available")}
            )
    ABSoutput <- ABS()
    phf <- ABSoutput$phf                            # population and housing generator output file
    ent <- ABSoutput$ent                            # ever never table
    sheds_var_raw <- ABSoutput$sheds_var_raw        # inputs sheds_sheds_variables both product and refined levels
    # sheds_var_product_raw <- ABSoutput$sheds_var_product_raw # inputs sheds_sheds_variables only product level
    activity_diary <- ABSoutput$activity_diary      # people activity diary
    seasonality_PUC <- ABSoutput$seasonality_PUC    # PUC seasonality

    ################################################
    # Household Characteristics Processor (HCP)
    ################################################
    HCP_output <-  ABSoutput$HCP_output

    ##########################
    ## for a given household 
    ##########################
    household_index_str <- household_number
    print (household_index_str)
    HCP_output_sub <- filter(HCP_output, household_index==household_index_str)

    ################################################
    # Call Household Universe of Products (HUP)
    ################################################
    # define HUP results for a household as 'HUPoutput_temp' and 
    # re-write as a dataframe
    HUPoutput_temp <- HUP(household_index_str, phf, ent)
    # list all the persons from a given household
    person_list <- unique(HCP_output_sub$person_index)


    CPS_out <- CPS(HUPoutput_temp, HCP_output_sub, sheds_var_raw)

    write.csv(CPS_out, "CPS_out1.csv")
    # # create a dataframe PUC_household to hold available PUCs at the 
    # # household level with its user's information (gender, age), PUC use 
    # # profile (mass, duration), as well as household information (size)
    # IUP_output <- data.frame()
    # ISP_output <- data.frame()
    # PUC_household <- data.frame()

    # ##########################################################
    # # loop through individuals in a household -- first pass
    # ##########################################################
    # for (person_index_str in 1:length(person_list)){
    #     # loop by person 
    #     ic <- filter(HCP_output_sub, person_index==person_index_str)

    #     ####################################################################
    #     # Call Individual Universe of Products (IUP) to get individual PUCs
    #     ####################################################################
    #     IUP_output_t <- IUP(HUPoutput_temp, ic, ent, hpf)
    #     IUP_output <- rbind(IUP_output, IUP_output_t)

    #     #################################################################
    #     # Call Individual Sample of Products (ISP) to get use profiles
    #     #################################################################
    #     ISP_output_t <- ISP(sheds_var_raw, IUP_output_t)
    #     ISP_output_t$sheds_id <- as.character(ISP_output_t$sheds_id)
    #     ISP_output <- rbind(ISP_output, ISP_output_t)

    #     ####################################################################
    #     # Call Refined Product Selector Utility Function (RPS) module to 
    #     # select only one refinery PUC from the same product type for each 
    #     # person. For instance, Person A CAN NOT use color pen or color 
    #     # pencil. BUT Person A can use color pen, while Person B can use 
    #     # color pencil.
    #     ####################################################################
    #     ISP_output_t_uniq <- RPS(ISP_output_t)
    #     PUC_person_temp <- left_join(ISP_output_t_uniq, hpf, 
    #                                  by=c("sheds_id"="SHEDSID", 
    #                                       "product_type"="product_type", 
    #                                       "Personal_or_Communal"="Personal_or_Communal")
    #                                 )
    #     PUC_household <- rbind(PUC_household, PUC_person_temp)
    # }

    # #############################################################
    # # loop through individuals in a household -- second pass
    # #############################################################
    # # begin to revise a person's communal PUC use frequency based 
    # # on above calculated info.
    # ICP_household_output <- data.frame()
    # IIT_household_output <- data.frame()
    # rdiary_household_output <- data.frame()
    # ICP_output_t_cc_output <- data.frame()
    
    # ####################################################################
    # # Call Communal PUC Generators (CPG)
    # ####################################################################
    # CPG_output <- CPG(PUC_household)
    # household_communal_uni <- CPG_output$household_communal_uni
    # PUC_household_CPG <- CPG_output$PUC_household

    # for (kk in 1:length(person_list)){
    #     cat ("Person ", kk, "\n")
    #     ####################################################################
    #     # Call Individual Cumulative Products (ICP)
    #     ####################################################################
    #     ICP_output_t <- ICP(PUC_household_CPG, household_communal_uni, kk)
    #     ICP_household_output <- bind_rows(ICP_household_output, ICP_output_t)

    #     ####################################################################
    #     # Call Clustered Activity Aggregator (CAA)
    #     ####################################################################
    #     CAAoutput <- CAA(ICP_output_t)
    #     # sort based on adjusted frequency
    #     ICP_output_t_cc <- arrange(CAAoutput$collapsed_clusters, desc(new_freq))
    #     ICP_output_t_pc <- arrange(CAAoutput$PUC_clusters, Clusters, sheds_id)
    #     ICP_output_t_cc_output <- bind_rows(ICP_output_t_cc_output, ICP_output_t_cc)

    #     # extract features from household members which will be insert to final output
    #     person_gender_temp <- ICP_output_t_cc[1, "person_gender"]
    #     person_age_temp <- ICP_output_t_cc[1, "person_age"]
    #     household_index_temp <- ICP_output_t_cc[1, "household_index"]
    #     person_index_temp <- ICP_output_t_cc[1, "person_index"]

    #     ####################################################################
    #     # Call Read Individual Diary (RID) and select a person's diary
    #     ####################################################################
    #     RIDoutput <- RID(activity_diary, person_age_temp)

    #     # note: if one wants to add columns to the output file need to change 3 places
    #     # 1. create column in RIDoutput and filled with NA
    #     # 2. to supply the same col names in IIT block below
    #     # 3. update INACT module
    #     RIDoutput$product_type <- NA
    #     RIDoutput$sheds_id <- NA
    #     RIDoutput$Personal_or_Communal <- NA
    #     RIDoutput$Clusters <- NA
    #     RIDoutput$Indoor_outdoor <- NA
    #     RIDoutput$periodicity <- NA
    #     RIDoutput$dur_actural <- NA            # actual duration of PUC used
    #     RIDoutput$dur_theory_combine <- NA
    #     RIDoutput$new_cluster_PUC <- NA
    #     RIDoutput$new_cluster_name <- NA
    #     RIDoutput$fni_list <- NA
    #     RIDoutput$freq_list <- NA
    #     RIDoutput$agg_fold_list <- NA
    #     RIDoutput$freq_float <- NA
    #     RIDoutput$flag <- NA

    #     # Begin to assign PUCs to diary
    #     for (jj in 1:nrow(ICP_output_t_cc)){
    #         # new_freq unit is times per year, new_aso unit is minutes
    #         # periodicity in minutes
    #         per <- 364.0/ICP_output_t_cc[jj, "new_freq"]*24*60 
    #         # duration in hr
    #         dur <- ICP_output_t_cc[jj, "new_aso"]/60 
    #         # name of a cluster (if available)
    #         Clusters_temp <- ICP_output_t_cc[jj, "Clusters"]
    #         if (Clusters_temp == ""){
    #             product_type_temp <- ICP_output_t_cc[jj, "product_type"]
    #         } else{
    #             product_type_temp <- ICP_output_t_cc[jj, "Clusters"]
    #         }
    #         Indoor_outdoor_temp <- ICP_output_t_cc[jj, "Indoor_outdoor"]
    #         sheds_id_temp <- ICP_output_t_cc[jj, "sheds_id"]
    #         Personal_or_Communal_temp <- ICP_output_t_cc[jj, "Personal_or_Communal"]

    #         # call the diary processing function each time before IIT and INACT can be invoked
    #         # add a few columns here such as index of person from household 
    #         # household index, Personal_or_Communal
    #         if (jj==1){
    #             dprooutput <- dpro(RIDoutput, seasonality_PUC, sheds_id_temp, HCP_output_sub$region[1]) 
    #             rdiary <- dprooutput$rdiary
    #         } else{
    #              # call the diary processing function each time before iit and inact can be invoked
    #             dprooutput <- dpro(rdiary, seasonality_PUC, sheds_id_temp, HCP_output_sub$region[1])
    #             rdiary <- dprooutput$rdiary
    #         }

    #         idiary <- dprooutput$idiary

    #         ###################################
    #         # Call Identify Idle Time (IIT) 
    #         ###################################
    #         IIToutput <- IIT(idiary, per, dur, ICP_output_t_pc, ICP_output_t_cc, Clusters_temp)
            
    #         if (nrow(IIToutput)>0){
    #             IIToutput$sheds_id <- sheds_id_temp
    #             IIToutput$product_type <- product_type_temp
    #             IIToutput$Personal_or_Communal <- Personal_or_Communal_temp
    #             IIToutput$Clusters <- Clusters_temp
    #             IIToutput$Indoor_outdoor <- Indoor_outdoor_temp
    #             IIToutput$periodicity <- per
    #             IIT_household_output <- rbind(IIT_household_output, IIToutput)

    #             ###################################
    #             # Call Insert Activities (INACT) 
    #             ###################################
    #             rdiary <- INACT(rdiary, IIToutput)
    #         }
    #     } # end of assigning PUCs to diary loop

    #     # Clean up rdiary after assigning all PUCs
    #     dprooutput <- dpro(rdiary, seasonality_PUC, sheds_id_temp, HCP_output_sub$region[1]) 
    #     rdiary <- dprooutput$rdiary
    #     rdiary <- mutate(rdiary, ending_time = Start.Time.hr.using.military.time+Duration.hr)
    #     rdiary <- filter(rdiary, Day.of.the.year>0)
    #     rdiary <- select(rdiary, -Minutes, -rind)
        
    #     ###############################################
    #     # Call Aggregated Activity Disaggregator (AAD) 
    #     ###############################################
    #     rdiary <- AAD(rdiary)

    #     ###############################################
    #     # Call Clustered Activity Disaggregator (CAD) 
    #     ###############################################
    #     rdiary_final <- CAD(rdiary, ICP_output_t_pc)
    #     rdiary_final$person_index <- person_index_temp
    #     rdiary_final$person_gender <- person_gender_temp
    #     rdiary_final$person_age <- person_age_temp
    #     rdiary_final$household_index <- household_index_str
    #     rdiary_household_output <- bind_rows(rdiary_household_output, rdiary_final)
    # } # end of second pass loop

    # ########################################
    # # add ht, act to output
    # ########################################
    # household_output <- left_join(rdiary_household_output, 
    #                               select(PUC_household, sheds_id, person_index, 
    #                                      use_ht, use_act, use_mass, 
    #                                      Appliances, Impacted.by), 
    #                               by=c("sheds_id"="sheds_id", 
    #                                    "person_index"="person_index")
    #                               )
    
    # ############################################################
    # # standardizer ht, act and mass for communal products 
    # ############################################################
    # # split the full results into 3 parts, rows with communal PUCs,
    # # rows with personal, and others
    # Communal_row <- filter(household_output, Personal_or_Communal=="Communal")
    # Personal_row <- filter(household_output, Personal_or_Communal=="Personal")
    # Other_row <- filter(household_output, is.na(Personal_or_Communal))
    
    # # select PUCs whose use_ht is non-empty (non-clusters)
    # non_empty_dur <- filter(household_output, Activity.Code==-2, !is.na(use_ht))
    # # find max value for ht, act and mass
    # non_empty_dur2 <- group_by(non_empty_dur, sheds_id) %>% 
    #                            mutate(max_ht=max(use_ht), 
    #                                   max_act=max(use_act),
    #                                   max_mass=max(use_mass)
    #                                   ) %>% 
    #                            select(sheds_id, use_ht=max_ht, 
    #                                             use_act=max_act, 
    #                                             use_mass=max_mass
    #                                   )
    # # remove duplicated, keeping one for each communal products
    # non_empty_dur2 <- non_empty_dur2[!duplicated(non_empty_dur2$sheds_id), ]

    # # overwrite old ht, iet oet and mass by standardizer values
    # Communal_row_fill <- left_join(select(Communal_row, -c(use_ht, use_act, use_mass)), 
    #                                non_empty_dur2, by=c("sheds_id"))

    # # piece everything back
    # household_output_udpate <- rbind(Communal_row_fill, Personal_row, Other_row)
    # household_output_udpate <- left_join(select(household_output_udpate, -Indoor_outdoor),
    #                                      select(hpf, SHEDSID, Indoor_outdoor), 
    #                                      by=c("sheds_id"="SHEDSID"))

    # # select variables to be sent to the output
    # household_output_udpate3 <- select(household_output_udpate, 
    #                                    household_index, 
    #                                    Person_household_index=person_index,
    #                                    Person_diary_ID=Person.ID,
    #                                    Diary_category=Diary.category,
    #                                    Person_gender=person_gender, 
    #                                    Person_age=person_age,
    #                                    Day_of_the_year=Day.of.the.year,
    #                                    Start_Time_hr_using_military_time=Start.Time.hr.using.military.time,
    #                                    End_Time_hr_using_military_time=ending_time,
    #                                    Duration_hr=Duration.hr,
    #                                    Duration_min=Duration.min,
    #                                    Activity_code=Activity.Code,
    #                                    Sheds_id=sheds_id,
    #                                    Product_type=product_type,
    #                                    Personal_or_Communal,
    #                                    Clusters,
    #                                    Indoor_outdoor,
    #                                    Appliances=Appliances,
    #                                    Impacted_by=Impacted.by,
    #                                    Handling_time=use_ht,
    #                                    Activity_time=use_act,
    #                                    Mass=use_mass
    #                                    ) 

    # # sort results
    # household_output_udpate3 <- arrange(household_output_udpate3, Person_household_index, 
    #                                     Day_of_the_year, Start_Time_hr_using_military_time)
    
    # # remove NA
    # household_output_udpate3[is.na(household_output_udpate3)]<- ""

    # # painting cluster could be either outdoor or indoor. So combine strings here to avoid writing issues
    # household_output_udpate3$Indoor_outdoor <- as.character(household_output_udpate3$Indoor_outdoor)
    
    # # label the Primary person of a household
    # household_output_udpate3$Primary_person <- 0
    # household_output_udpate3[household_output_udpate3$Person_household_index==1, "Primary_person"] <- 1

    # # export results into a CSV
    # write.csv(household_output_udpate3, paste0("./OUTPUTS/", "Household_", household_index_str, ".csv"), row.names = FALSE)

    # # clean up memory
    # gc()
}
######################
#### End of ABM_Runner
######################


