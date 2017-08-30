##################################
# handle diary files
##################################

#######################################################################################################################	
# This block is only used after modification of diary files. 
# Since the original diary files are too large, we covnert it to an R object to speedup

library(dplyr)
# setup working folder
setwd("C:/Users/33855/Dropbox/_ICF_project/WA 2-75/Agent-Based Models/Modular_Structure/INPUTS/diary_new_04_26_2017")

add_id_diary <- function(p_diary){
	# calculate ID of each person
	# find index of Day=1
	day_1_index <- which(p_diary$day==1)

	kk_pre_index<-1
	new_id_index <- c(kk_pre_index)
	# a collection of index where day==1
	new_id <- c(day_1_index[kk_pre_index])

	for (kk_index in 2:length(day_1_index)){
		kk_pre_val <- day_1_index[kk_pre_index]
		kk_val <- day_1_index[kk_index]

		if (kk_val-kk_pre_val >1){
			new_id_index <- c(new_id_index, kk_index)
			new_id <- c(new_id, kk_val)
			kk_pre_index <- kk_index
		} else{
			kk_pre_index <- kk_index
		}
	}

	p_diary$id <- 0
	# a collection of index where day==365
	new_id_last <- c(new_id-1, nrow(p_diary))
	new_id_last <- new_id_last[new_id_last>0]
	# assign id
	for (qq in 1:length(new_id)){
		p_diary[new_id[qq]:new_id_last[qq], "id"] <- qq
	}

	return(p_diary)
}


# load CSV diary files and add one indicator to reflect its source
adult_non_work_raw <- read.csv("icf_adult_non_work.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="")
adult_non_work_raw$cat <- "adult_non_work"
# add ID to diary
adult_non_work_raw_2 <- add_id_diary(adult_non_work_raw)

adult_work_raw <- read.csv("icf_adult_work.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="")
adult_work_raw$cat <- "adult_work"
adult_work_raw_2 <- add_id_diary(adult_work_raw)

child_school_raw <- read.csv("icf_child_school.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="")
child_school_raw$cat <- "child_school"
child_school_raw_2 <- add_id_diary(child_school_raw)

child_young_raw <- read.csv("icf_child_young.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="")
child_young_raw$cat <- "child_young"
child_young_raw_2 <- add_id_diary(child_young_raw)

# combine diary files into one variable
activity_diary_raw <- rbind(adult_non_work_raw_2, adult_work_raw_2, child_school_raw_2, child_young_raw_2)

# pick useful columns
activity_diary_pool <- activity_diary_raw[c("id", "day", "start", "dt", "act", "cat")]
names(activity_diary_pool) <- c("Person.ID", "Day.of.the.year", "Start.Time.hr.using.military.time", "Duration.hr", "Activity.Code", "Diary.category")

# Day 1 corresponds to Sunday. Therefore, the first full day is Monday morning  (Day 2).
activity_diary_pool$Day.of.the.year <- activity_diary_pool$Day.of.the.year-1

# save to an RDS file to speedup
saveRDS(activity_diary_pool, "activity_diary_pool.rds")
#######################################################################################################################