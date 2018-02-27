    post_process <- function(household_idx, household_output){
    library(openxlsx)
    file_name <- paste0("./OUTPUTS/", "Summary_PUC_Household_", household_idx, ".xlsx")

    ####################################################################
    # analysis of PUC profile 
    ####################################################################
    # finally assigned PUCs 
    rdiary_household_assign_all <- household_output %>% filter(Activity.Code == -2) %>%
                               group_by(person.index, sheds.id.refined) 

    summary_household_assign_all <- summarise(rdiary_household_assign_all, 
                                             n_assigned = length(sheds.id.refined),
                                             dur_actural_mean = mean(as.numeric(Duration.hr)),
                                             dur_actural_min = min(as.numeric(Duration.hr)),
                                             dur_actural_max = max(as.numeric(Duration.hr)),
                                             mass_mean = mean(as.numeric(use.mass))
                                            )

    summary_household_assign_all <- arrange(summary_household_assign_all, person.index, sheds.id.refined)



    ####################################################################
    # analysis of PUC profile (clusters)
    ####################################################################
    # finally assigned clsuter PUCs 
    rdiary_household_assign_c <- household_output %>% filter(Activity.Code == -2, Clusters !="") %>%
                                 group_by(person.index, Clusters, Day.of.the.year) %>% 
                                 mutate(Duration.hr_c = sum(Duration.hr)) %>% ungroup() %>% 
                                 group_by(person.index, Clusters)

    summary_household_assign_c0 <- summarise(rdiary_household_assign_c, 
                                            dur_actural_mean = mean(as.numeric(Duration.hr_c)),
                                            dur_actural_min = min(as.numeric(Duration.hr_c)),
                                            dur_actural_max = max(as.numeric(Duration.hr_c))
                                            )



    summary_household_assign_c0 <- arrange(summary_household_assign_c0, person.index)


    # save results to an excel
    wb <- createWorkbook()
    addWorksheet(wb = wb, sheetName = "All PUCs", gridLines = TRUE)
    writeDataTable(wb = wb, sheet = "All PUCs", x = as.data.frame(summary_household_assign_all))
    addWorksheet(wb = wb, sheetName = "Cluster", gridLines = TRUE)
    writeDataTable(wb = wb, sheet = "Cluster", x = as.data.frame(summary_household_assign_c0))
    saveWorkbook(wb, file_name, overwrite = TRUE)


}

