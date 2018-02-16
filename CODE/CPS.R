# Communal PUC Selector (CPS)
# Function: 

###########################
#### Beginning of CPS #####
###########################

sample_communal_PUC_step2 <- function(sheds_var_raw, sheds_var_product_selected, HCP_output_sub){
	##########################################################
	# for a product, decide which household member will use it 
	# and generate a use profile
	##########################################################
	### Step 1: for the selected communal product, pick one refined 
	### (assume each same chance for any refined product to be picked)
	
	# find product PUC 
	product_t <- sheds_var_product_selected$PUCID_productype[1]
	
	# get all the refined PUCs
	sheds_var_raw_refined_t <- sheds_var_raw[(sheds_var_raw$PUCID_productype==product_t) & 
	                                         (sheds_var_raw$refined==1), ]
	
	# randomly pick one
	sheds_var_raw_sub <- sheds_var_raw_refined_t[sample(nrow(sheds_var_raw_refined_t), 1), ]

	### Step 2: for all household members, pick one to use this item

	# select threshold to select a male
	refined_PUC_prev_M <- sheds_var_raw_sub$Prev_M[1]

	# get a random number to decide which person will use this communal PUC
	rnd <- runif(1) 

	# figure out the number of adults
	HCP_output_adult <- HCP_output_sub[HCP_output_sub$person_age>=13, ]
	
	# number of adults (assume this value is always>0)
	n_adult <- dim(HCP_output_adult)[1]
	
	# number of unique genders
	n_gender <- length(unique(HCP_output_adult$person_gender))

	# if same gender, randomly pick one
	if (n_gender==1){
		picked_person_idx <- HCP_output_adult[sample(nrow(HCP_output_adult), 1), ]$person_index
	} else{
		# if male prevalence is greater than the random number, we pick a male, else pick a female
		if (refined_PUC_prev_M >= rnd){
			HCP_output_adult_male <- HCP_output_adult[HCP_output_adult$person_gender=="M", ]
			n_adult_male <- dim(HCP_output_adult_male)[1]

			# if there are more than 1 male adults, randomly select one, 
			# otherwise (no male, I do not think else will be executed), pick a female
			if (n_adult_male>0){
				picked_person_idx <- HCP_output_adult_male[sample(nrow(HCP_output_adult_male), 1), ]$person_index
			} else{
				picked_person_idx <- HCP_output_adult[sample(nrow(HCP_output_adult), 1), ]$person_index
			}
		} else{
			HCP_output_adult_female <- HCP_output_adult[HCP_output_adult$person_gender=="F", ]
			n_adult_female <- dim(HCP_output_adult_female)[1]

			# if there are more than 1 female adults, randomly select one, otherwise, pick a male
			if (n_adult_female>0){
				picked_person_idx <- HCP_output_adult_female[sample(nrow(HCP_output_adult_female), 1), ]$person_index
			} else{
				picked_person_idx <- HCP_output_adult[sample(nrow(HCP_output_adult), 1), ]$person_index
			}
		}
	}

	### Step 3: generate use profile
	house_size_temp <- HCP_output_adult$house_size[1]
	select_adult <- HCP_output_adult[HCP_output_adult$person_index==picked_person_idx,]
	person_age_temp <- select_adult$person_age[1]
	person_gender_temp <- select_adult$person_gender[1]
	sheds_id_temp <- sheds_var_raw_sub$PUCID_refined[1]

	# call UPG to generate profile, since we know this refined PUC is picked, thus threshold for prevalence is set to 0
	PUC_use_profile <- UPG(sheds_id_temp, sheds_var_raw, house_size_temp, person_gender_temp, person_age_temp, "Communal", 0)
	PUC_use_profile$person_index <- picked_person_idx

	return (PUC_use_profile)
}



sample_communal_PUC_step1 <- function(product_in, sheds_var_raw, HCP_output_sub){
	################################################
	# function decide which communal product will 
	# be used at the household level
	################################################
	n_product = dim(product_in)[1]  # number of products
	product_use_profile <- data.frame()     # empty df to store results

	# select products
	if (n_product>0){
		# generate random numbers to compare against prev_hh to decide if a product is used at household level 
		product_in$rnd <- runif(dim(product_in)[1], min=0, max=1) 
		product_in$select_hh <- apply(product_in, 1, function(x) {x["Prev_hh"] >= x["rnd"]})

		# only keep the selected communal products
		product_in_selected <- product_in[product_in$select_hh==TRUE, ]
		n_product_selected <- dim(product_in_selected)[1]

		if (n_product_selected>0){
			for (kk in 1:n_product_selected){
				product_use_profile_t <- sample_communal_PUC_step2(sheds_var_raw, product_in_selected[kk,], HCP_output_sub)
				product_use_profile <- rbind(product_use_profile, product_use_profile_t)
			}
		}
	}

	return (product_use_profile)

}




CPS <- function(HUPoutput, HCP_output_sub, sheds_var_raw){

	### Step 1 is to split HUP filtered products to communal cluster and communal noncluster group

	# subset products filtered by HUP, which are the potential ones can be used by the household
	sheds_var_product_sub <-  sheds_var_raw[(sheds_var_raw$PUCID_productype %in% HUPoutput), ]

	# communal cluster
	sheds_var_product_sub_cluster <- sheds_var_product_sub[(sheds_var_product_sub$cluster != 0) & 
	                                                       (sheds_var_product_sub$personal_communal != "Personal") &
	                                                       (sheds_var_product_sub$refined==0), ]

	# communal non-cluster
	sheds_var_product_sub_noncluster <- sheds_var_product_sub[(sheds_var_product_sub$cluster == 0) & 
	                                                          (sheds_var_product_sub$personal_communal != "Personal") &
	                                                          (sheds_var_product_sub$refined==0), ]

	### Step 2 assign noncluster PUCs
	noncluster_PUC_assigned <- sample_communal_PUC_step1(sheds_var_product_sub_noncluster, sheds_var_raw, HCP_output_sub)
	noncluster_PUC_assigned$cluster <- ""

	### Step 3 assign cluster PUCs (similar to noncluster except one 
	### has to back track all products from the same cluster and starts there)
	clusters_pool <- unique(sheds_var_product_sub_cluster$cluster)
	n_cluster <- length(clusters_pool)
	cluster_use_profile <- data.frame()

	if ((n_cluster)>0){
		for (zzz in 1:n_cluster){
			### select a cluster
			cluster_name_t <- clusters_pool[zzz]

			# list all communal products which belong to the same cluster (after filtered by HUP)
			cluster_product_level <- sheds_var_raw[(sheds_var_raw$PUCID_productype %in% HUPoutput) &
			                                       (sheds_var_raw$cluster==cluster_name_t) &
			                                       (sheds_var_raw$personal_communal != "Personal") &
			                                       (sheds_var_raw$refined==0), ]

			# for those products, need to decide which one (ones) will be picked by the house
			picked_refine <- sample_communal_PUC_step1(cluster_product_level, sheds_var_raw, HCP_output_sub)

			if (dim(picked_refine)[1]>0){
				picked_refine$cluster <- cluster_name_t
			}
			cluster_use_profile <- rbind(cluster_use_profile, picked_refine)
		}
	}

	# Need to post-process person assignment for cluster PUCs. 
	# Currently we randomly pick a person for a refined PUC, however,
	# since each cluster will be used by one person, 
	# let's pick the first designated person to use all refined PUCs

	selected_cluster <- unique(cluster_use_profile$cluster)
	for (pp in selected_cluster){
		harmonized_person_index <- cluster_use_profile[cluster_use_profile$cluster==pp, ]$person_index[1]
		cluster_use_profile[cluster_use_profile$cluster==pp, ]$person_index <- harmonized_person_index
	}

	CPS_df <- rbind(cluster_use_profile, noncluster_PUC_assigned)

	return (CPS_df)
}


