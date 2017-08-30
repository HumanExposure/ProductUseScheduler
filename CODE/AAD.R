# Aggregated Activity Disaggregator (AAD)
# Function: Disaggregate aggregated activities after IIT

###########################
#### Beginning of AAD #####
###########################
AAD <- function(rdiary){  

    ############################################################################
    # Inputs:
    #       rdiary: a dataframe of a person's diary
    # Outputs:
    #       rdiary: an updated diary
    ############################################################################

	# convert agg_fold_list to numeric
	rdiary$agg_fold_list <- as.numeric(rdiary$agg_fold_list)
	
	# diary contains aggregated rows
	rdiary_agg <- rdiary[rdiary$flag %in% c(11, 12, 21, 22),]
	# diary contains non-aggregated rows
	rdiary_nagg <- rdiary[!rdiary$flag %in% c(11, 12, 21, 22),]

	# number of aggregated rows
	n_agg_row <- nrow(rdiary_agg)
	# output of a disaggregated dataframe
	rdiary_dagg <- data.frame()

	# if diary contains aggregated row
	if ((n_agg_row) >0){
		for (jj in 1:n_agg_row){
			# times a activity has been aggregated
			agg_fold_t <- rdiary_agg[jj, "agg_fold_list"]
			
			# original aggregated row
			row_agg_ori_temp <- rdiary_agg[jj,]

			# replicate this row based on the times an activity is aggregated
			row_dagg_temp <- row_agg_ori_temp[rep(seq_len(nrow(row_agg_ori_temp)), each=agg_fold_t),]

			# divide the following columns based on times a activity has been aggregated
			row_dagg_temp$Duration.hr <- row_dagg_temp$Duration.hr/agg_fold_t
			row_dagg_temp$dur_theory_combine <- row_dagg_temp$dur_theory_combine/agg_fold_t
			row_dagg_temp$dur_actural <- row_dagg_temp$dur_actural/agg_fold_t
			row_dagg_temp$Duration.min <- row_dagg_temp$Duration.hr*60

			for (qq in (1:agg_fold_t)){
				if (qq==1){
					Start.Time.hr.temp <- row_dagg_temp[qq, "Start.Time.hr.using.military.time"]
					ending_time.temp <- Start.Time.hr.temp + row_dagg_temp[qq, "Duration.hr"]
					row_dagg_temp[qq, "ending_time"] <- ending_time.temp
					if (row_dagg_temp[qq, "flag"]==11){
						row_dagg_temp[qq, "flag"] <- 114
					} else if (row_dagg_temp[qq, "flag"]==12){
						row_dagg_temp[qq, "flag"] <- 124
					} else {
						row_dagg_temp[qq, "flag"] <- 204
					}
					row_dagg_temp[qq, "agg_fold_list"] <- 1
				}
				else{
					Start.Time.hr.temp <- ending_time.temp
					row_dagg_temp[qq, "Start.Time.hr.using.military.time"] <- Start.Time.hr.temp
					ending_time.temp <- Start.Time.hr.temp + row_dagg_temp[qq, "Duration.hr"]
					row_dagg_temp[qq, "ending_time"] <- ending_time.temp
					if (row_dagg_temp[qq, "flag"]==11){
						row_dagg_temp[qq, "flag"] <- 114
					} else if (row_dagg_temp[qq, "flag"]==12){
						row_dagg_temp[qq, "flag"] <- 124
					} else {
						row_dagg_temp[qq, "flag"] <- 204
					}

					row_dagg_temp[qq, "agg_fold_list"] <- 1
				}
			}
			rdiary_dagg <- bind_rows(rdiary_dagg, row_dagg_temp)
		}
		rdiary <- bind_rows(rdiary_dagg, rdiary_nagg)
		rdiary <- arrange(rdiary, Day.of.the.year, Start.Time.hr.using.military.time)
	}

	return(rdiary)
}  # end of function

######################
#### End of AAD ######
######################