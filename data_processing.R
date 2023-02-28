data_path <- "./data"
if(!dir.exists(data_path)) dir.create(data_path)
acc_dat.dir <- file.path(data_path, "IU_walking_driving_climbing")
acc_dat.zip <- file.path(data_path, "IU_walking_driving_climbing.zip")
acc_dat.zip.url <- "https://www.dropbox.com/s/pf5l2ki9t2ae2df/IU_walking_driving_climbing.zip?dl=1"
if (!dir.exists(acc_dat.dir) & !file.exists(acc_dat.zip)){
  ## Download .zip file
  download.file(acc_dat.zip.url, acc_dat.zip)
  ## Unzip into data/ directory
  unzip(acc_dat.zip, exdir = data_path)
  ## Remove .zip file
  file.remove(acc_dat.zip)
}
files <- list.files("/Users/lilykoffman/Documents/walking_fingerprint/data/IU_walking_driving_climbing/raw_accelerometry_data",
                    full.names = T)
read_files <- function(x){
  test <- readr::read_csv(x, show_col_types = F) %>% mutate(
    ID = sub(".csv.*", "",sub(".*id", "", x))
  )
}

library(purrr)
df_all <- files %>% map_dfr(read_files) 
# filter to just walking
df_all <- df_all %>% filter(activity==1)  %>% group_by(ID) %>% mutate(
  t = row_number(),
  J = floor(time_s)
) %>% group_by(ID) %>% mutate(minJ = min(J)) %>% ungroup() %>% 
  mutate(J = J-minJ+1) %>% dplyr::select(-minJ) %>% mutate(
    signal_lw = sqrt(lw_x^2 +lw_y^2+lw_z^2),
    signal_lh = sqrt(lh_x^2 +lh_y^2+lh_z^2),
    signal_la = sqrt(la_x^2 +la_y^2+la_z^2),
    signal_ra = sqrt(ra_x^2 +ra_y^2+ra_z^2),
  ) %>% mutate(ID2 = as.numeric(as.factor(ID)))

write.csv(df_all, "df_all.csv")
