#' Go from wide to long data
#'
#' Original data has depth imbedded in column names
#' In this case, it's actually the layer number, but will
#' be tranformed to depth later in workflow 
#' 
#' @param in_dat The wide data
#' @param temp_type The type of temperature data, to be used as the temperature column name
#' @return A long dataframe with columns date, depth, value, temp_type
make_dat_long <- function(in_dat, temp_type) {
  
  out_dat <- in_dat %>%
    tidyr::gather(key = depth, value = !!temp_type, -date) %>%
    mutate(depth = as.numeric(gsub('depth_', '', depth)))
  
  return(out_dat)

}

#' Retrieves all model output and observations for target lake
#' Relies on "in/PGDTL_Labels" and "in/PGDTL_Outputs" from Jared
#' 
#' @param target_id NHDID of target lake
#' @param dat_mods The summary model table
#' @param lake_meta Lake metadata table
#' @return A dataframe with paired observations and model output to be used for bias calculations
get_target_dat <- function(target_id, dat_mods, lake_meta) {
  
  temp_row <- filter(dat_mods, `target lake` == target_id)
  
  # get source lake IDs in 1,2,3 order
  source_lake_ids <- c(temp_row$`1st source lake nhd id`, temp_row$`2nd source lake nhd id`, temp_row$`3rd source lake nhd_id`)
  
  # get source lake names in 1,2,3 order
  source_lake_names <- filter(lake_meta, nhd_id %in% paste0('nhd_', source_lake_ids)) %>%
    pull(lake_name) %>% as.character()
  
  # get labels = observed data
  dat_labels <- feather::read_feather(file.path('in', 'PGDTL_Labels', paste0(target_id, '_full.feather')))

  dat_obs_long <- dat_labels %>%
    tidyr::gather(key = depth, value = Observed, -dates) %>%
    mutate(depth = as.numeric(gsub('depth', '', depth))) %>%
    filter(!is.na(Observed))
  
  # output = ensemble modeled data
  dat_out <- feather::read_feather(file.path('in', 'PGDTL_Outputs', paste0(target_id, '_outputTripleModel.feather')))

  dat_out_long <- make_dat_long(dat_out, 'Ensemble')
  
  # source models
  target_files<- grep(paste0(target_id, '.feather'), ind_models, value = TRUE)  
  
  s1_dat <- feather::read_feather(file.path('in', 'PGDTL_SingleOutputs', grep(source_lake_ids[1], target_files, value = TRUE)))
  s2_dat <- feather::read_feather(file.path('in', 'PGDTL_SingleOutputs', grep(source_lake_ids[2], target_files, value = TRUE)))
  s3_dat <- feather::read_feather(file.path('in', 'PGDTL_SingleOutputs', grep(source_lake_ids[3], target_files, value = TRUE)))
  
  s1_long <- make_dat_long(s1_dat, 'S1')
  s2_long <- make_dat_long(s2_dat, 'S2')
  s3_long <- make_dat_long(s3_dat, 'S3')
  
  # bring together and calculate bias
  dat_compare <- left_join(dat_out_long, dat_obs_long, by = c('date'='dates', 'depth')) %>%
    left_join(s1_long) %>% left_join(s2_long) %>% left_join(s3_long) %>%
    mutate(depth = depth/2)
  
 
  
  
  
  return(dat_compare)
  
}


#' Retrieves source lake observed temperature
#' 
#' @param target_id NHDID of target lake
#' @param dat_mods The summary model table
#' @param all_obs daily temperature values for all lakes (output from lake_temperature_model_prep)
#' @return A long dataframe with all source lake observations
get_source_obs <- function(target_id, dat_mods, all_obs) {
  temp_row <- filter(dat_mods, `target lake` == target_id)
  
  # get source lake IDs in 1,2,3 order
  source_lake_ids <- c(temp_row$`1st source lake nhd id`, temp_row$`2nd source lake nhd id`, temp_row$`3rd source lake nhd_id`)
  
  # add source lake observed dates
  s1 <- filter(all_obs, nhd_id == paste0('nhd_', source_lake_ids[1])) %>%
    mutate(variable = 'S1 bias') %>%
    select(date, variable) %>%
    distinct()
  s2 <- filter(all_obs, nhd_id == paste0('nhd_', source_lake_ids[2])) %>%
    mutate(variable = 'S2 bias')%>%
    select(date, variable) %>%
    distinct()
  s3 <- filter(all_obs, nhd_id == paste0('nhd_', source_lake_ids[3])) %>%
    mutate(variable = 'S3 bias')%>%
    select(date, variable) %>%
    distinct()
  

  source_obs <- bind_rows(s1, s2, s3) %>%
    mutate(source_obs = TRUE)
  
  return(source_obs)
  
}

#' Calculates bias of each source and ensemble model
#' 
#' @param combined_dat output from get_target_dat
#' @param source_obs output from get_source_obs
#' @return A dataframe with observations and bias calculations summarized by bin
calculate_bias <- function(combined_dat, source_obs) {
  depth_max <- max(combined_dat$depth)
  depth_by <- ifelse(depth_max <= 25, 2, 3)
  depth_by <- ifelse(depth_max >=50, 4, depth_by)
  # bias calculation
  ts_dat_bias <- combined_dat %>%
    mutate(ensemble_bias = Ensemble - Observed,
           s1_bias = S1 - Observed,
           s2_bias = S2 - Observed,
           s3_bias = S3 - Observed) %>%
    mutate(depth_bin = cut(depth, breaks = seq(0, depth_max, by = depth_by),
                           include.lowest = TRUE)) %>%
    filter(!is.na(depth_bin)) %>%  # exclude Mendota depths that are >depth_max (20m)
    group_by(date, depth_bin) %>%
    summarize(`Observed` = median(Observed, na.rm = TRUE),
              `Ensemble bias` = median(ensemble_bias, na.rm = TRUE),
              `S1 bias` = median(s1_bias, na.rm = TRUE),
              `S2 bias` = median(s2_bias, na.rm = TRUE),
              `S3 bias` = median(s3_bias, na.rm = TRUE))
  
  ts_dat_long <- tidyr::gather(ts_dat_bias, key = 'variable', value = 'value', -date, -depth_bin) %>%
    filter(!is.na(value)) %>%
    ungroup() %>%
    mutate(date = as.Date(date))
  
  source <- source_obs %>%
    mutate(depth_bin = unique(ts_dat_long$depth_bin)[1])
  
  ts_dat_long <- full_join(ts_dat_long, source) %>%
    mutate(year = lubridate::year(date),
           doy = lubridate::yday(date))
  
  return(ts_dat_long)
}

#' Find year with most observations in target lake
#' 
#' @param bias_dat output from calculate_bias
#' @return Top year value
calc_top_year <- function(bias_dat) {
  year_summary <- bias_dat %>%
    filter(variable == 'Observed') %>%
    filter(!is.na(value)) %>%
    group_by(year) %>%
    summarize(n = n())
  
  top_year <- year_summary$year[which.max(year_summary$n)]
  
  return(top_year)
}

#' Ranks lakes based most improved ensemble model (most to least)
#' 
#' @param dat_mods The summary model table
#' @return A vector of NHDIDs from most to least improved
rank_lakes <- function(dat_mods) {
  dat_order <- dat_mods %>%
    rowwise() %>%
    mutate(min_RMSE =  min(c(`1st source lake model rmse`, `2nd source lake model rmse`, `3rd source lake model rmse`), na.rm = TRUE)) %>%
    mutate(min_diff = `RMSE On All Target Lake Observations` - min_RMSE) %>%
    arrange(min_diff) %>%
    pull(`target lake`)
  
  return(dat_order)
}




