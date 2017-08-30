# Read Individual Diary (RID)
# Function: Pull an appropriate diary from the csv set EPA provided

###########################
#### Beginning of RID #####
###########################
RID <- function(activity_diary_pool, person_age, diary_type_str=NULL, person_index_str=NULL){

    ############################################################################
    # Inputs:
    #       activity_diary: a pool of activity diary files including 
    #                       adult_non_work, adult_work, and child_school
    #       person_age: age of a household member
    #       diary_type_str (optional): used if wants to select a specific diary type
    #       person_index_str (optional): used if wants to select a specific ID from 
    #                                activity diary ID
    # Outputs:
    #       P_activity_diary: a dataframe of selected diary
    ############################################################################
    
    # Find the proper pool of diaries based on age. 
    # If this houshold member is >18, choose adult group
    # If age is between 6 and 18, choose child_school
    # Otherwise using child_young.

    if (is.null(diary_type_str)){
        if (person_age>18){
            # for now randomly pick one
            diary_type<- sample(c("adult_non_work", "adult_work"), 1)
            activity_diary <- filter(activity_diary_pool, Diary.category==diary_type)
        } else if (person_age>=6 && person_age<=18){
            activity_diary <- filter(activity_diary_pool, Diary.category=="child_school")
        } else {
            activity_diary <- filter(activity_diary_pool, Diary.category=="child_young")
        }
    } else{
            activity_diary <- filter(activity_diary_pool, Diary.category==diary_type_str)
    }


    # if a person index is specified, use it for all households (debug mode), otherwise
    # just randomly select a person
    if (is.null(person_index_str)){
        random_person_index <- sample(1:length(unique(activity_diary$Person.ID)), 1)
    } else{
        random_person_index <- person_index_str
    }
    
    # Select a person's diary between Day 1 to Day 364
    P_activity_diary <- filter(activity_diary, Person.ID==random_person_index)

    # calculate ending time
    P_activity_diary <- mutate(P_activity_diary, End.Time.hr.using.military.time=Start.Time.hr.using.military.time+Duration.hr)
    P_activity_diary_OT <- filter(P_activity_diary, End.Time.hr.using.military.time>24.00)
    P_activity_diary_nonOT <- filter(P_activity_diary, End.Time.hr.using.military.time<=24.00)

    # code to break a cross-day slot into 2. 
    P_activity_diary_OT_add_pool <- data.frame()
    for (kk in 1:nrow(P_activity_diary_OT)){
        # old dur day
        old_day <- P_activity_diary_OT[kk, "Day.of.the.year"]
        # find full duration
        old_dur_full <- P_activity_diary_OT[kk, "Duration.hr"]
        # duration 1 which makes the ending time to 23.99 on previous day
        old_dur_1 <- 24.00 - P_activity_diary_OT[kk, "Start.Time.hr.using.military.time"]
        # duration 2 which are the residual duration assigned to next day
        old_dur_2 <- old_dur_full - old_dur_1

        # update previous day to make sure the PUC ends at 23.99
        P_activity_diary_OT[kk, "Duration.hr"] <- old_dur_1
        P_activity_diary_OT[kk, "End.Time.hr.using.military.time"] <- 24.00

        # extract the timeslot to be splitted
        P_activity_diary_OT_add <- P_activity_diary_OT[kk, ]
        P_activity_diary_OT_add$Start.Time.hr.using.military.time <- 0.00
        P_activity_diary_OT_add$Day.of.the.year <- old_day + 1
        P_activity_diary_OT_add$Duration.hr <- old_dur_2
        P_activity_diary_OT_add$End.Time.hr.using.military.time <- old_dur_2
        P_activity_diary_OT_add_pool <-bind_rows(P_activity_diary_OT_add_pool, P_activity_diary_OT_add)
    }   

    P_activity_diary <-bind_rows(P_activity_diary_nonOT, P_activity_diary_OT, P_activity_diary_OT_add_pool)
    P_activity_diary <- arrange(P_activity_diary, Day.of.the.year, Start.Time.hr.using.military.time)
    P_activity_diary <- select(P_activity_diary, -End.Time.hr.using.military.time)

    return (P_activity_diary)
}

#####################
#### End of CAA #####
#####################


