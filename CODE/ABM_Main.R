#############################################
# ABM is executed by calling ABM_Runner()
##############################################

rm(list=ls())

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
  
  # check inputs
  if(is.null(pth_str)){
    stop("Please specify a working directory")
  }
  
  ##################
  # setup ABM inputs
  ##################
  
  
  ##########################
  ## for a given household
  ##########################
  rdiary_household_output<- data.frame()
  household_index_str <- household_number
  print (household_index_str)
  HCP_output_sub <- HCP_output[HCP_output$household_index==household_index_str,]
  # list all the persons from a given household
  person_list <- unique(HCP_output_sub$person_index)
  
  ################################################
  # Call Household Universe of Products (HUP)
  ################################################
  # define HUP results for a household as 'HUP_output' and
  # re-write as a dataframe (only communal)
  HUP_output <- HUP(household_index_str, phf, ent)
  
  ################################################
  # Call Communal PUC Selector (CPS)
  ################################################
  CPS_out <- CPS(HUP_output, HCP_output_sub, sheds_var_raw)
  # write.csv(CPS_out, paste0("./OUTPUTS/CPS_out_", household_index_str, ".csv"))
  
  # create a dataframe PUC_household to hold available PUCs at the
  # household level with its user's information (gender, age), PUC use
  # profile (mass, duration), as well as household information (size)
  # IUP_output_full <- data.frame()
  # RPT_output_full <- data.frame()
  
  ##########################################################
  # loop through individuals in a household -- first pass
  ##########################################################
  for (person_index_str in 1:length(person_list)){
    # loop by person
    # person_index_str=2
    ic <- HCP_output_sub[HCP_output_sub$person_index==person_index_str, ]
    person_index_temp <- person_index_str
    person_gender_temp <- ic$person_gender
    person_age_temp <- ic$person_age
    
    ####################################################################
    # Call Individual Universe of Products (IUP) to get individual PUCs
    ####################################################################
    IUP_output <- IUP(ic, ent, sheds_var_raw)
    # IUP_output_full <- rbind(IUP_output_full, IUP_output)
    
    ### temport fix to only keep personal PUCs
    # IUP_output <- IUP_output[IUP_output$personal_communal=="Personal",]
    ###
    
    #################################################################
    # Call Individual Sample of Products (ISP) to get use profiles
    #################################################################
    ISP_output <- ISP(sheds_var_raw, IUP_output)
    ISP_output$sheds_id <- as.character(ISP_output$sheds_id)
    
    
    CPS_out_ind <- filter(CPS_out, person_index==person_index_str)
    CPS_out_ind$person_index <- NULL
    
    df_rpt <- rbind(CPS_out_ind, ISP_output)
    colnames(df_rpt)[which(names(df_rpt) == "cluster")] <- "Clusters"
    # RPT_output_full <- rbind(RPT_output_full, df_rpt)
    
    ####################################################################
    # Call Clustered Activity Aggregator (CAA)
    ####################################################################
    CAA_output <- CAA(df_rpt)
    df_aggcl <- CAA_output$collapsed_clusters
    df_aggcl <- df_aggcl[order(df_aggcl$use_freq, decreasing = TRUE),]
    df_puccl <- CAA_output$PUC_clusters
    df_puccl <- df_puccl[order(df_puccl$use_freq, decreasing = TRUE),]
    
    ####################################################################
    # Call Read Individual Diary (RID) and select a person's diary
    ####################################################################
    RIDoutput <- RID(activity_diary, person_age_temp)
    
    # note: if one wants to add columns to the output file need to change 3 places
    # 1. create column in RIDoutput and filled with NA
    # 2. to supply the same col names in IIT block below
    # 3. update INACT module
    RIDoutput$product_type <- NA
    RIDoutput$sheds_id <- NA
    RIDoutput$Personal_or_Communal <- NA
    RIDoutput$Clusters <- NA
    RIDoutput$Indoor_outdoor <- NA
    RIDoutput$periodicity <- NA
    RIDoutput$dur_actural <- NA            # actual duration of PUC used
    RIDoutput$dur_theory_combine <- NA
    RIDoutput$new_cluster_PUC <- NA
    RIDoutput$new_cluster_name <- NA
    RIDoutput$fni_list <- NA
    RIDoutput$freq_list <- NA
    RIDoutput$agg_fold_list <- NA
    RIDoutput$freq_float <- NA
    RIDoutput$flag <- NA
    
    # Begin to assign PUCs to diary
    for (jj in 1:nrow(df_aggcl)){
      # use_freq unit is times per year, use_aso unit is minutes
      # periodicity in minutes
      per <- (364.0/df_aggcl[jj, "use_freq"])*24*60
      # duration in hr
      dur <- df_aggcl[jj, "use_aso"]/60
      # name of a cluster (if available)
      Clusters_temp <- df_aggcl[jj, "Clusters"]
      if (Clusters_temp == ""){
        product_type_temp <- df_aggcl[jj, "sheds_id"]
      } else{
        product_type_temp <- df_aggcl[jj, "Clusters"]
      }
      Indoor_outdoor_temp <- df_aggcl[jj, "Indoor_outdoor"]
      sheds_id_temp <- df_aggcl[jj, "sheds_id"]
      Personal_or_Communal_temp <- df_aggcl[jj, "Personal_or_Communal"]
      # call the diary processing function each time before IIT and INACT can be invoked
      # add a few columns here such as index of person from household
      # household index, Personal_or_Communal
      if (jj==1){
        dprooutput <- dpro(RIDoutput, sheds_var_raw, sheds_id_temp, HCP_output_sub$region[1])
        rdiary <- dprooutput$rdiary
      } else{
        # call the diary processing function each time before iit and inact can be invoked
        dprooutput <- dpro(rdiary, sheds_var_raw, sheds_id_temp, HCP_output_sub$region[1])
        rdiary <- dprooutput$rdiary
      }
      
      idiary <- dprooutput$idiary
      
      ###################################
      # Call Identify Idle Time (IIT)
      ###################################
      IIToutput <- IIT(idiary, per, dur, df_puccl, df_aggcl, Clusters_temp)
      
      if (nrow(IIToutput)>0){
        IIToutput$sheds_id <- sheds_id_temp
        IIToutput$product_type <- product_type_temp
        IIToutput$Personal_or_Communal <- Personal_or_Communal_temp
        IIToutput$Clusters <- Clusters_temp
        IIToutput$Indoor_outdoor <- Indoor_outdoor_temp
        IIToutput$periodicity <- per
        # IIT_household_output <- rbind(IIT_household_output, IIToutput)
        
        ###################################
        # Call Insert Activities (INACT)
        ###################################
        rdiary <- INACT(rdiary, IIToutput)
      }
    } # end of assigning PUCs to diary loop
    
    # Clean up rdiary after assigning all PUCs
    dprooutput <- dpro(rdiary, sheds_var_raw, sheds_id_temp, HCP_output_sub$region[1])
    rdiary <- dprooutput$rdiary
    rdiary <- mutate(rdiary, ending_time = Start.Time.hr.using.military.time+Duration.hr)
    rdiary <- filter(rdiary, Day.of.the.year>0)
    rdiary <- select(rdiary, -Minutes, -rind)
    
    ###############################################
    # Call Aggregated Activity Disaggregator (AAD)
    ###############################################
    rdiary <- AAD(rdiary)
    
    ###############################################
    # Call Clustered Activity Disaggregator (CAD)
    ###############################################
    rdiary_final <- CAD(rdiary, df_puccl)
    rdiary_final$person_index <- person_index_temp
    rdiary_final$person_gender <- person_gender_temp
    rdiary_final$person_age <- person_age_temp
    rdiary_final$household_index <- household_index_str
    # create table of relevant refined PUCs with fields like use_mass, use_ht
    df_rpt_temp <- merge(select(df_rpt, sheds_id, use_ht, use_act, use_mass),
                         product_refine_tbl, all.x=T, by.x="sheds_id", by.y="PUCID_refined")
    # add fields like use_mass to idnvidual diary
    rdiary_final <- merge(rdiary_final, df_rpt_temp, all.x=T, by="sheds_id")
    # ### hack to fix Painting cluster indoor outdoor differential status
    # 
    # a=rdiary_final[which(rdiary_final$Clusters=="Painting"),] # partition painting cluster rows if any
    # b=rdiary_final[(rdiary_final$Clusters!="Painting"),] # all other rows
    # 
    # if (nrow(a)>0) {
    #   c<-sheds_var_raw[,c("PUCID_refined","indoor_outdoor")]
    #   c<-c[c$PUCID_refined!="#NA", ]
    #   d<-merge(a,c,by.a="sheds_id",by.c="PUCID_refined",all.a=T)
    #   d$Indoor_outdoor<-d$indoor_outdoor # replace with original PUCIDs for Painting
    #   e<-d[,!(names(d) %in% c("PUCID_refined","indoor_outdoor"))] # drop new vars
    #   rdiary_final<-bind_rows(b,e) # bind the non painting cluster rows with the painting cluster rows -- not sorted. done later anyway.
    # }
    # 
    # # end of hack
    
    rdiary_household_output <- bind_rows(rdiary_household_output, rdiary_final)
  } # end of second pass loop
  
  
  household_output <- rdiary_household_output %>% select( #Person.ID,
    Day.of.the.year,
    Start.Time.hr.using.military.time,
    End.Time.hr.using.military.time=ending_time,
    Duration.hr,
    Duration.min,
    Activity.Code,
    Diary.category,
    PUCID.productype=PUCID_productype,
    sheds.id.refined=sheds_id,
    PUCID.PT.description=PUCID_PT_description,
    Personal.or.Communal=Personal_or_Communal,
    Clusters,
    Indoor.outdoor=Indoor_outdoor,
    periodicity,
    dur.actural=dur_actural,
    dur.theory.combine=dur_theory_combine,
    new.cluster.PUC=new_cluster_PUC,
    new.cluster.name=new_cluster_name,
    fni.list=fni_list,
    freq.list=freq_list,
    agg.fold.list=agg_fold_list,
    freq.float=freq_float,
    flag,
    household.index=household_index,
    person.index=person_index,
    person.gender=person_gender,
    person.age=person_age,
    use.ht=use_ht,
    use.act=use_act,
    use.mass=use_mass
  ) %>%
    arrange(person.index, Day.of.the.year, Start.Time.hr.using.military.time)
  household_output<-household_output[!is.na(household_output$Day.of.the.year),] # part of hack
  
  household_output$Primary.person <- 0
  household_output[household_output$person.index=="1", "Primary.person"] <- 1
  household_output$freq.float <- as.numeric(household_output$freq.float)
  household_output <- household_output %>% mutate_at(.vars = c("Duration.hr", "periodicity", "dur.actural",
                                                               "dur.theory.combine", "freq.float", "Duration.min",
                                                               "use.ht", "use.act", "use.mass"),
                                                     .funs = funs(round(.,3)))
  opcols <- c("Day.of.the.year","Start.Time.hr.using.military.time","End.Time.hr.using.military.time","Duration.hr","Duration.min","Activity.Code","Diary.category","PUCID.productype","sheds.id.refined","PUCID.PT.description","Personal.or.Communal","Clusters","Indoor.outdoor","household.index","person.index","person.gender","person.age","use.ht","use.act","use.mass","Primary.person")
  household_output<-household_output[,opcols]
  
  # export results into a CSV
  write.csv(household_output, paste0("./OUTPUTS/", "Household_", household_index_str, ".csv"), row.names = FALSE)
  
  # post-processing
  # source("./CODE/post_analysis.R")
  # post_process(household_number, household_output)
  
  # clean up memory
  gc()
  an.error.occured<<-FALSE
}


######################
#### End of ABM_Runner
######################




# declare working folder
set.seed(1234)
pth_str <- "C:/Users/13963SS/Documents/ABM/2019/ProductUseScheduler-master_PP3/"

setwd(pth_str)
tryCatch(source("./CODE/ABS.R"),
         error=function(e) {stop("ABS.R is not available")}
)
ABSoutput <- ABS()
phf <- ABSoutput$phf                            # population and housing generator output file
ent <- ABSoutput$ent                            # ever never table
sheds_var_raw <- ABSoutput$sheds_var_raw        # inputs sheds_sheds_variables both product and refined levels
product_refine_tbl <- ABSoutput$product_refine_tbl  # a table with product and refined product links
activity_diary <- ABSoutput$activity_diary      # people activity diary
seasonality_PUC <- ABSoutput$seasonality_PUC    # PUC seasonality

################################################
# Household Characteristics Processor (HCP)
################################################
HCP_output <- ABSoutput$HCP_output

# directly specify household numbers

##### TWO RUN OPTIONS ARE AVAILABLE BELOW. COMMENT OUT THE VERSION NOT BEING USED.


### NON PARALLEL VERSION  ###############
#
# house_list <- 1:1000
# 
# for(i in house_list){
#   tryCatch({ABM_Runner(pth_str,i)}
#            , error = function(e) {an.error.occured <<- TRUE})
#      while(an.error.occured==TRUE){
#      tryCatch({ABM_Runner(pth_str,i)}
#             , error = function(e) {an.error.occured <<- TRUE})
#   } # end while
# 
#   
# } # end for
###############PARALLEL VERSION ########
#
house_list <- 1:1000

library(foreach)
library(doParallel)

# register number of cores (total #CPU-1)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)


out <- foreach(i = 1:length(house_list),.packages='dplyr') %dopar% {
  res <- tryCatch({ABM_Runner(pth_str,i)}
                  , error = function(e) {an.error.occured <<- TRUE})
  while(an.error.occured==TRUE){
    tryCatch({ABM_Runner(pth_str,i)}
             , error = function(e) {an.error.occured <<- TRUE})
    
  } # end while   
  
} # end of do-par loop


on.exit(stopCluster(cl))

