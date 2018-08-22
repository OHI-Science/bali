## functions.R.
## Each OHI goal model is a separate R function. The function name is the 2- or 3- letter code for each goal or subgoal; for example, FIS is the Fishing subgoal of Food Provision (FP).

FIS <- function(layers){


	#1.status based catch compared with MSY, modified from
	#Selig et al (2015) & Helpern et al (2015)
	#if catch/msy  <0.95, SS = catch/msy
	#if 0.95 <= catch/msy <= 1.05, SS = 1
	#if catch/msy > 1.05, SS = max{1-a(catch/msy-1.05),b}, a = 0.5, b = 0.25
	########################################################################

		#catch and msy
		##############
		catch_lyrs <- c("fis_catch")
		catch <- SelectData2(catch_lyrs)


		catchmsy_lyrs <- c("fis_msy")
		catchmsy <- SelectData2(catchmsy_lyrs)


		catch_all <- merge(catch, catchmsy, by = c("rgn_id", "scenario_year", "data_year"))



		catch_all$score <- catch_all$ton/catch_all$msy


			am <- NULL
			for (aa in 1:nrow(catch_all)){

				al <- catch_all[aa,]

				if (al$score < 0.95){

					al$status_score <- al$score
					am <- rbind(am, al)

				}else{
				if (al$score <= 1.05){

					al$status_score <- 1
					am <- rbind(am, al)

				}else{

					al$status_score <- max(1-(0.5*(al$score-1.05)), 0.25)
					am <- rbind(am, al)
				}
				}
			}



		am <- am[, c("rgn_id", "scenario_year", "status_score")]

		names(am) <- c("rgn_id", "year", "status_score")


		#2. calculate trend (latest 5 years)based on status score
		##########################################################

		r.trend <- am%>%filter(year >= min(am$year))%>%
				filter(!is.na(status_score))%>%
				group_by(rgn_id)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()

		r.trend <- r.trend%>%group_by(rgn_id)%>%do(mdl=lm(status_score~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5)%>%ungroup()




		#status FIS
		###########
		am <- am%>%group_by(rgn_id)%>%summarize(score = round(mean(status_score, na.rm = T), 2)*100)%>%ungroup()
		status <- am%>%mutate(dimension = "status")%>%rename(region_id = rgn_id, score = score, dimension = dimension)



		#trend FIS
		##########

		trend <- r.trend%>%mutate(dimension = "trend")%>%rename(region_id = rgn_id, score = trend, dimension = dimension)




	#assembles dimensions (status, trend)
	#####################################
	scores <- rbind(status, trend)%>%mutate(goal = "FIS")%>%data.frame()

	print(scores)


	return(scores)

}

MAR <- function(layers){


	#1.#status score based on modification of Elfes et al (2014)
	#############################################################
	extent_lyrs <- c("mar_extent")

		extent <- SelectData2(extent_lyrs)


	prod_lyrs <- c("mar_production"	)

		prod <- SelectData2(prod_lyrs)


		status <- merge(extent, prod, by = c("rgn_id", "scenario_year", "data_year"))


		#mariculture sustainability index(MSI) derived from Trujillo (2008)
		msi <- 4.88

		#total potential mariculture 46773 ha, derived from Bali fisheries agency
		potential_extent <- 46.773


		status$y_smi <- status$ton*msi

		w <- status%>%group_by(rgn_id)%>%summarize(total_ysmi = sum(y_smi, na.rm = T))%>%ungroup()

		status <- merge(status, w, by = c("rgn_id"))

		status$w <- status$y_smi/status$total_ysmi



			#calculate reference sustainable production)
			ref <- status%>%group_by(rgn_id)%>%summarize(max_prod = max(ton, na.rm = T))%>%ungroup()


			status <- merge(status, ref, by = c("rgn_id"))

			status$ref <- status$max_prod/status$km2




		status$x <- status$y_smi/(status$ref * potential_extent)

		status$score <- round(status$w * status$x , 4)

		status <- status[, c("rgn_id", "scenario_year", "score")]

		names(status) <- c("rgn_id", "year", "score")


		#2. calculate trend (latest 5 years)based on status score
		##########################################################

		r.trend <- status%>%filter(year >= min(status$year))%>%
				filter(!is.na(score))%>%
				group_by(rgn_id)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()

		r.trend <- r.trend%>%group_by(rgn_id)%>%do(mdl=lm(score~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5)%>%ungroup()


		#status MAR
		###########
		status <- status%>%group_by(rgn_id)%>%summarize(score = round(sum(score, na.rm = T)*100, 1))%>%ungroup()
		status <- status%>%mutate(dimension = "status")%>%rename(region_id = rgn_id, score = score, dimension = dimension)

		#trend MAR
		##########

		trend <- r.trend%>%mutate(dimension = "trend")%>%rename(region_id = rgn_id, score = trend, dimension = dimension)

		#assembles dimensions (status, trend)
		#####################################
		scores <- rbind(status, trend)%>%mutate(goal = "MAR")%>%data.frame()

		print(scores)

		return(scores)

}


AO <- function(layers){

	#1.status based catch compared with MSY, modified from
	#Selig et al (2015) & Helpern et al (2015)
	#if catch/msy  <0.95, SS = catch/msy
	#if 0.95 <= catch/msy <= 1.05, SS = 1
	#if catch/msy > 1.05, SS = max{1-a(catch/msy-1.05),b}, a = 0.5, b = 0.25
	########################################################################

		#catch and msy
		##############
		catch_lyrs <- c("ao_banyar", "ao_gulamah", "ao_terbang", "ao_kakap", "ao_kembung", "ao_kerapu", "ao_kuniran", "ao_kuwe", "ao_layang", "ao_manyung", "ao_swanggi")
		catch <- SelectData2(catch_lyrs)

		#print(catch)

		catchmsy_lyrs <- c("ao_banyar_msy", "ao_gulamah_msy", "ao_terbang_msy", "ao_kakap_msy", "ao_kembung_msy", "ao_kerapu_msy", "ao_kuniran_msy", "ao_kuwe_msy", "ao_layang_msy",
						"ao_manyung_msy", "ao_swanggi_msy")
		catchmsy <- SelectData2(catchmsy_lyrs)

		#print(catchmsy)

		catch_all <- merge(catch, catchmsy, by = c("rgn_id", "scenario_year", "data_year", "species"))

		catch_all$score <- catch_all$catch/catch_all$msy


			am <- NULL
			for (aa in 1:nrow(catch_all)){

				al <- catch_all[aa,]

				if (al$score < 0.95){

					al$status_score <- al$score
					am <- rbind(am, al)

				}else{
				if (al$score <= 1.05){

					al$status_score <- 1
					am <- rbind(am, al)

				}else{

					al$status_score <- max(1-(0.5*(al$score-1.05)), 0.25)
					am <- rbind(am, al)
				}
				}
			}





		am <- am[, c("rgn_id", "scenario_year", "species", "status_score")]

		names(am) <- c("rgn_id", "year", "species","status_score")

		#2. calculate trend (latest 5 years)based on status score
		##########################################################

		r.trend <- am%>%filter(year >= min(am$year))%>%
				filter(!is.na(status_score))%>%
				group_by(rgn_id, species)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()

		r.trend <- r.trend%>%group_by(rgn_id, species)%>%do(mdl=lm(status_score~year, data=.))%>%
				summarize(rgn_id = rgn_id, species = species, trend = coef(mdl)['year']*5)%>%ungroup()



		#status AO
		###########
		am <- am%>%group_by(rgn_id)%>%summarize(score = round(mean(status_score, na.rm = T), 4)*100)%>%ungroup()

		status <- am%>%mutate(dimension = "status")%>%rename(region_id = rgn_id, score = score, dimension = dimension)


		#trend AO
		##########


		trend <- r.trend%>%group_by(rgn_id)%>%summarize(score = round(mean(trend, na.rm = T), 4))%>%ungroup()

		trend <- trend%>%mutate(dimension = "trend")%>%rename(region_id = rgn_id, score = score, dimension = dimension)




	#assembles dimensions (status, trend)
	#####################################
	scores <- rbind(status, trend)%>%mutate(goal = "AO")%>%data.frame()

	print(scores)

	return(scores)



}


CS <- function(layers){


	#extent
	#######
	extent_lyrs <- c("cs_mangrove_extent", "cs_seagrass_extent")
	extent <- SelectData2(extent_lyrs)

	#print(extent)

	#health
	#######
	health_lyrs <- c("cs_mangrove_health", "cs_seagrass_health")
	health <- SelectData2(health_lyrs)

	#print(health)


	#status scores follows CS equation in Elfes et al (2014)
	########################################################

		#total extent each habitat
		###########################
		total_extent <- extent%>%group_by(habitat)%>%summarize(total_extent = sum(km2, na.rm = T))%>%ungroup()

		#reference values for each habitat and region id, the best values is choosed
		############################################################################
		reference_point <- health%>%group_by(habitat, rgn_id)%>%summarize(reference_point = max(health, na.rm = T))%>%ungroup()


		#merge extent, health, total_extent, & reference point
		######################################################
		cs_all <- merge(extent, health, by = c("rgn_id", "scenario_year", "data_year", "habitat"))

		cs_all <- cs_all%>%select(rgn_id, scenario_year, data_year, habitat, km2, health)

		cs_all <- merge(cs_all, total_extent, by = c("habitat"))

		cs_all <- merge(cs_all, reference_point, by = c("rgn_id", "habitat"))

		#calculate status score
		#######################
		cs_all$status_score <- (cs_all$health/cs_all$reference_point)*(cs_all$km2/cs_all$total_extent)


		am <- cs_all[, c("rgn_id", "scenario_year", "habitat", "status_score")]

		names(am) <- c("rgn_id", "year", "habitat","status_score")


		#2. calculate trend (latest 5 years)based on status score
		##########################################################

		r.trend <- am%>%filter(year >= min(am$year))%>%
				filter(!is.na(status_score))%>%
				group_by(rgn_id, habitat)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()

		r.trend <- r.trend%>%group_by(rgn_id, habitat)%>%do(mdl=lm(status_score~year, data=.))%>%
				summarize(rgn_id = rgn_id, habitat = habitat, trend = coef(mdl)['year']*5)%>%ungroup()



		#status CS
		###########
		am <- am%>%group_by(rgn_id)%>%summarize(score = round(sum(status_score, na.rm = T), 4)*100)%>%ungroup()

		status <- am%>%mutate(dimension = "status")%>%rename(region_id = rgn_id, score = score, dimension = dimension)

		#trend CS
		##########

		trend <- r.trend%>%group_by(rgn_id)%>%summarize(score = round(mean(trend, na.rm = T), 4))%>%ungroup()

		trend <- trend%>%mutate(dimension = "trend")%>%rename(region_id = rgn_id, score = score, dimension = dimension)

	scores <- rbind(status, trend)%>%mutate(goal = "CS")%>%data.frame()

	## set ranks for each habitat
	#############################
	habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)




  ## create weights file for pressures/resilience calculations
  #############################################################
	weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cs_km2_x_storage") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)

	layers$data$element_wts_cs_km2_x_storage <- weights


	print(scores)

	return(scores)

  }


CP <- function(layers){


	#extent
	#######
	extent_lyrs <- c("cs_mangrove_extent", "cs_seagrass_extent", "cp_coral_extent")
	extent <- SelectData2(extent_lyrs)

	#print(extent)

	#health
	#######
	health_lyrs <- c("cs_mangrove_health", "cs_seagrass_health", "cp_coral_health")
	health <- SelectData2(health_lyrs)

	#print(health)


	#status scores follows CP equation in Elfes et al (2014)
	########################################################

		#total extent each habitat
		###########################
		total_extent <- extent%>%group_by(habitat)%>%summarize(total_extent = sum(km2, na.rm = T))%>%ungroup()

		#reference values for each habitat and region id, the best values is choosed
		############################################################################
		reference_point <- health%>%group_by(habitat, rgn_id)%>%summarize(reference_point = max(health, na.rm = T))%>%ungroup()


		#merge extent, health, total_extent, & reference point
		######################################################
		cp_all <- merge(extent, health, by = c("rgn_id", "scenario_year", "data_year", "habitat"))

		cp_all <- cp_all%>%select(rgn_id, scenario_year, data_year, habitat, km2, health)

		cp_all <- merge(cp_all, total_extent, by = c("habitat"))

		cp_all <- merge(cp_all, reference_point, by = c("rgn_id", "habitat"))

		#rank habitat, seagrass = 1, mangrove & coral = 4
		cp_all$rank <- ifelse(habitat == "seagrass", 1, 4)

		#status score
		##############
		cp_all$status_score <- (cp_all$health/cp_all$reference_point)*(cp_all$rank/max(cp_all$rank, na.rm = T)) * (cs_all$km2/cs_all$total_extent)


		am <- cp_all[, c("rgn_id", "scenario_year", "habitat", "status_score")]

		names(am) <- c("rgn_id", "year", "habitat","status_score")


		#2. calculate trend (latest 5 years)based on status score
		##########################################################

		r.trend <- am%>%filter(year >= min(am$year))%>%
				filter(!is.na(status_score))%>%
				group_by(rgn_id, habitat)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()

		r.trend <- r.trend%>%group_by(rgn_id, habitat)%>%do(mdl=lm(status_score~year, data=.))%>%
				summarize(rgn_id = rgn_id, habitat = habitat, trend = coef(mdl)['year']*5)%>%ungroup()



		#status CP
		###########
		am <- am%>%group_by(rgn_id)%>%summarize(score = round(sum(status_score, na.rm = T), 4)*100)%>%ungroup()

		status <- am%>%mutate(dimension = "status")%>%rename(region_id = rgn_id, score = score, dimension = dimension)

		#trend CP
		##########

		trend <- r.trend%>%group_by(rgn_id)%>%summarize(score = round(mean(trend, na.rm = T), 4))%>%ungroup()

		trend <- trend%>%mutate(dimension = "trend")%>%rename(region_id = rgn_id, score = score, dimension = dimension)

		scores <- rbind(status, trend)%>%mutate(goal = "CP")%>%data.frame()

	## set ranks for each habitat
	#############################
	habitat.rank <- c('mangrove'         = 4,
                    'coral'		        = 4,
                    'seagrass'         = 1)



  ## create weights file for pressures/resilience calculations
  #############################################################

  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cp_km2_x_protection") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)

  layers$data$element_wts_cp_km2_x_protection <- weights

	# return scores
	return(scores)

	print(scores)



  }




FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  print(rgns)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}

## Helper functions ----

# function to link data and scenario years based on
# conf/scenario_data_years.csv information

get_data_year <- function(layer_nm, layers=layers) { #layer_nm="le_wage_cur_base_value"

  all_years <- conf$scenario_data_years %>%
    mutate(scenario_year= as.numeric(scenario_year),
           data_year = as.numeric(data_year)) %>%
    filter(layer_name %in% layer_nm) %>%
    select(layer_name, scenario_year, year=data_year)


  layer_vals <- layers$data[[layer_nm]]



  layers_years <- all_years %>%
    left_join(layer_vals, by="year") %>%
    select(-layer)

  names(layers_years)[which(names(layers_years)=="year")] <- paste0(layer_nm, "_year")


  return(layers_years)
}


# useful function for compiling multiple data layers
# only works when the variable names are the same across datasets
# (e.g., coral, seagrass, and mangroves).
# it relies on get_data_year(), a function defined immediately above.
SelectData2 <- function(layer_names){
  data <- data.frame()
  for(e in layer_names){ # e="le_jobs_cur_base_value"
    data_new <- get_data_year(layer_nm=e, layers=layers)
    names(data_new)[which(names(data_new) == paste0(e, "_year"))] <- "data_year"
    data <- rbind(data, data_new)
  }
  return(data)
}
