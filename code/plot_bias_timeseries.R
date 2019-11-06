# test
library(dplyr)
library(ggplot2)

# source functions
source('code/plot_bias_functions.R')

# lake metadata
# get lakes metadata
drive_download(as_id('https://docs.google.com/spreadsheets/d/1FPFi4QSnlIZkutrEQlapYhX5mkhEwiQrtQq3zFiPo3c/edit#gid=88060065'),
               path = 'in/lakes_metadata.csv', overwrite = TRUE)

lake_meta <- read.csv('in/lakes_metadata.csv')

# model results
drive_download(as_id('https://docs.google.com/spreadsheets/d/1fr4dD5Opa4TJ5HnMgfSahfA35cJdMGqj4snQ87SdgoQ/edit#gid=0'),
               path = 'in/pgdtl_results.csv', overwrite = TRUE)
dat_mods <- read.csv('in/pgdtl_results.csv', check.names = FALSE, nrows = 68) %>%
  select(1:22)

# in google drive folders shared by Jared:
# should also have in/PGDTL_Outputs and in/PTDTL_Labels (caled in functions)

ind_models <- list.files('in/PGDTL_SingleOutputs')

# from here: https://drive.google.com/drive/u/1/folders/1F-Xw5xFYVP7wbvHsV7siHvnyDy0yqMG9
all_obs <- feather::read_feather('in/merged_temp_data_daily.feather')

# function to transform model output


lake_order <- rank_lakes(dat_mods)

for (i in 1:length(lake_order)) {
  dat_combined <- get_target_dat(target_id = lake_order[i], dat_mods = dat_mods, lake_meta = lake_meta)
  source_obs <- get_source_obs(target_id = lake_order[i], dat_mods = dat_mods, all_obs = all_obs)
  bias_dat <- calculate_bias(dat_combined, source_obs)
  top_year <- calc_top_year(bias_dat)
  
 
  
  plot_dat <- filter(bias_dat, year == top_year) %>%
    mutate(source_obs = ifelse(is.na(source_obs), NA, floor(min(value, na.rm = TRUE))))
  
  min_temp_value <- floor(min(plot_dat$value[plot_dat$variable == 'Observed'], na.rm = TRUE))
  
  plot_dat <- mutate(plot_dat, source_obs = ifelse(variable == 'Observed', min_temp_value, source_obs))
  
  # find date ranges of target lake
  min_date <- min(plot_dat$doy[plot_dat$variable == 'Observed'], na.rm = TRUE)
  max_date <- max(plot_dat$doy[plot_dat$variable == 'Observed'], na.rm = TRUE)
  
  # find min and max for bias scales
  bias_scales <- filter(plot_dat, variable != 'Observed')
  bias_range <- range(bias_scales$value, na.rm = TRUE)
  bias_low <- floor(bias_range[1]) - 1
  bias_high <- ceiling(bias_range[2]) + 1
  

  
  # add dummy obs to get scales the same
  dummy_dat <- data.frame(date = NA,
                          depth_bin = factor(levels(plot_dat$depth_bin)[1], levels = levels(plot_dat$depth_bin)),
                          variable = rep(c('S1 bias', 'S2 bias', 'S3 bias', 'Ensemble bias'), 2),
                          value = c(rep(bias_low, 4), rep(bias_high, 4)),
                          year = top_year,
                          doy = NA)
  
  plot_dat <- bind_rows(plot_dat, dummy_dat) %>%
    mutate(variable = factor(variable, levels = c('Observed', 'S1 bias', 'S2 bias', 'S3 bias', 'Ensemble bias')))
  
  date_labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  date_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  
  dummy_hline <- data.frame(variable = unique(plot_dat$variable),
                            int = c(NA, rep(0, 4)))
  
  target_name <- as.character(lake_meta$lake_name[lake_meta$nhd_id == paste0('nhd_', lake_order[i])])
  
  # plot!
  
  p <- ggplot(plot_dat, aes(x = doy, y = value)) +
    geom_line(aes(group = depth_bin, color = depth_bin), alpha = 0.7, size = 1.1) +
    geom_point(aes(group = depth_bin, color = depth_bin),
               alpha = 0.7, size = 0.9, fill = 'white') +
    geom_point(data = plot_dat, aes(x = doy, y = source_obs), shape = '|', color = 'darkgray', size = 2) +
    geom_hline(data = dummy_hline, aes(yintercept = int), linetype = 2) +
    viridis::scale_color_viridis(discrete = TRUE, direction = -1) + #, palette = 'RdYlBu'
    #scale_shape_manual(values = c(21, 22, 23)) +
    scale_x_continuous(breaks = date_breaks,
                       labels = date_labels, limits = c(min(plot_dat$doy, na.rm = TRUE)-5, max(plot_dat$doy, na.rm = TRUE)+5))+
    #facet_wrap(Depth_cat~year, ncol = 3, scales = 'free_y') +
    facet_grid(rows = vars(variable),  scales = 'free_y') +
    coord_cartesian(xlim = c(min_date-5, max_date+5)) +
    theme_bw()+
    theme(#strip.text = element_blank(),
      strip.background = element_blank(),
      legend.position = 'right',
      legend.direction="horizontal",
      legend.key.size = unit(1.5, 'lines'),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = 'transparent', color = NA),
      legend.box.background = element_rect(color = NA),
      panel.spacing = unit(0.8, 'lines'),
      legend.spacing.y = unit(2, 'lines'),
      text = element_text(size = 24),
      axis.text.y = element_text(size = 18)
    ) +
    labs( y = 'Observed Temperature or Bias (deg C)', x = '', title = paste0(target_name, ', ', top_year)) +
    guides(color = guide_legend(title = 'Depth (m)', title.position = 'top', ncol = 1,
                                label.position = 'left', direction = "vertical"))
  
  file_name <- file.path('out', paste0(i, '_', lake_order[i], '_', gsub(' ', '', target_name), '_bias_plot.png'))
  
  ggsave(file_name, p, height = 11, width = 10, units = 'in', bg = 'transparent')
}
