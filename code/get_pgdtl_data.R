library(googledrive)
library(dplyr)
library(ggplot2)

# get lakes metadata
drive_download(as_id('https://docs.google.com/spreadsheets/d/1FPFi4QSnlIZkutrEQlapYhX5mkhEwiQrtQq3zFiPo3c/edit#gid=88060065'),
                                 path = 'in/lakes_metadata.csv', overwrite = TRUE)

lake_meta <- read.csv('in/lakes_metadata.csv')

# get PGDTL model results
drive_download(as_id('https://docs.google.com/spreadsheets/d/1fr4dD5Opa4TJ5HnMgfSahfA35cJdMGqj4snQ87SdgoQ/edit#gid=0'),
               path = 'in/pgdtl_results.csv', overwrite = TRUE)

dat <- read.csv('in/pgdtl_results.csv', check.names = FALSE, nrows = 68) %>%
  select(1:22) %>%
  mutate(target_lake = paste0('nhd_', `target lake`)) %>%
  left_join(select(lake_meta, nhd_id, lake_name), by = c('target_lake' = 'nhd_id'))

dat_long <- dat %>%
  select(lake_name, `RMSE On All Target Lake Observations`, `1st source lake model rmse`, `2nd source lake model rmse`, `3rd source lake model rmse`, `GLM uncal RMSE 1/3rd data test set`) %>%
  tidyr::gather(key = 'model', value = 'RMSE', -lake_name)

dat_order <- dat %>%
  rowwise() %>%
  mutate(min_RMSE =  min(c(`1st source lake model rmse`, `2nd source lake model rmse`, `3rd source lake model rmse`), na.rm = TRUE)) %>%
  mutate(min_diff = `RMSE On All Target Lake Observations` - min_RMSE) %>%
  arrange(min_diff) %>%
  pull(lake_name)

dat_long$lake_name <- factor(dat_long$lake_name, levels = dat_order)

ggplot(dat_long, aes(x = lake_name, y = RMSE)) +
  geom_point(aes(shape = model, color = model), alpha = 0.9) +
  scale_color_manual(values = c(  '#006d2c','#41ae76', '#99d8c9','black', 'black')) +
  scale_shape_manual(values = c(16,16,16, 1, 16)) +
  scale_size_manual(values = c(2,2,2,1,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(dat_long, aes(x = model, y = RMSE)) +
  geom_violin(draw_quantiles = 0.5) +
  geom_point(shape = '-', size = 6, alpha = 0.5)


dat_sub <- select(dat, lake_name, 
                  first_distance = `1st source lake normalized distance from target`,
                  second_distance = `2nd source lake normalized distance from target`, 
                  third_distance = `3rdsource lake normalized distance from target`,
                  first_rmse = `1st source lake model rmse`, 
                  second_rmse = `2nd source lake model rmse`, 
                  third_rmse = `3rd source lake model rmse`,
                  glm_uncal = `GLM uncal RMSE 1/3rd data test set`) %>%
  mutate(one_two = second_rmse - first_rmse,
         two_three = third_rmse - second_rmse,
         min_diff = min(c(first_rmse, second_rmse, third_rmse), na.rm = TRUE))

dat_distance <- select(dat_sub, -first_rmse, -second_rmse, -third_rmse, -glm_uncal) %>%
  tidyr::gather(key = 'model', value = 'distance', -target_lake)

dat_rmse <- select(dat_sub, -first_distance, -second_distance, -third_distance) %>%
  tidyr::gather(key = 'model', value = 'rmse', -target_lake)

dat_all_rmse <- dat %>%
  select(target_lake = `target lake`, rmse = `RMSE On All Target Lake Observations`) %>%
  mutate(model = )

  
ggplot(dat, aes(x = `1st source lake normalized distance from target`, y = `1st source lake model rmse`)) +
  geom_point(color = 'green', size = 3) +
  geom_point(data = dat, aes(x = `2nd source lake normalized distance from target`, y = `2nd source lake model rmse`), color = 'yellow', size = 3)+
  geom_point(data = dat, aes(x = `3rdsource lake normalized distance from target`, y = `3rd source lake model rmse`), color = 'red', size = 3)

  
