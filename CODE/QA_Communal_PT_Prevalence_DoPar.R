# to estimate observed prevalence for personal product types only (not refined, not household)

# need to change variables to reflect product type variable in new diary file, plus changes to input output

#variables to hold files


# BUILD PUC LIST (OR DATAFRAME) AS FOLLOWS

# read sheds_var_raw file and filter on (1) communal and (2) refined=0
# keep umbrella product type codes and prev_hh columns (theoretical prevalence)

#data frames to hold data: 
pth_str <- "D:/Dropbox/_ICF_project/WA 2-75/Agent-Based Models/Modular_Structure/ProductUseScheduler/"
setwd(pth_str)

sheds_var_raw<-read.csv("./INPUTS/PUC_use_data_NEW_TH2.csv")

upt_comm<-sheds_var_raw[sheds_var_raw$refined==0&sheds_var_raw$personal_communal=="Communal",]

upt_comm_list<-as.list(upt_comm$PUCID_productype)

upt_comm_list<-as.vector(unlist(upt_comm_list))

op<-data.frame("PUC_Product_Type"=character(), "Observed_Prevalence_ProductType_Communal"=numeric(), stringsAsFactors = F)

com_puc_prev<-function(a) { # function to process each PUC. argument a is an index in the list of communal PUCs
  
  apuc <- upt_comm_list[a]#this PUC
  
  print(apuc)
  
  pt_hh_count<-0.0
  
  hd_list<-list.files(path="./OUTPUTS/", pattern="^Household_")
  
  for (hn in hd_list){
    
    hdiary<-read.csv(paste("./OUTPUTS/","/",hn,sep=""))
    
    print(hn)
    
    print(apuc%in%hdiary$PUCID.productype)
    
    if (apuc %in% hdiary$PUCID.productype){
      
      pt_hh_count<-pt_hh_count+1
      
    } # end of if statement
    
    
  } # end of loop over households
  
  prev_puc_hh <-pt_hh_count /length(hd_list) # change to number of households or create and use nhousehold variable
  
  opt<-data.frame("PUC_Product_Type"=apuc,"Observed_Prevalence_ProductType_Communal"=prev_puc_hh)
  
  return (opt)
  

} # end of function 


library(doParallel)  
no_cores <- detectCores() - 3  
cl <- makeCluster(no_cores)  
registerDoParallel(cl)  

op<-foreach(a=1:length(upt_comm_list),.combine=rbind) %dopar% com_puc_prev(a)

opdf<-merge(op,upt_comm[,c("PUCID_productype","Prev_hh")],by.x="PUC_Product_Type",by.y="PUCID_productype",all.x=T)

ent<-read.csv("./INPUTS/FullENT_NEW.csv")

opdf<-merge(opdf,ent[,c("PUCID_productype","PUCID_PT_description")],by.x="PUC_Product_Type",by.y="PUCID_productype",all.x=T)

write.csv(opdf,"./Communal_Prevalance.csv", row.names=F)

on.exit(stopCluster(cl))