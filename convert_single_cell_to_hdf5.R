

# import:
single_cell_raw <- readRDS("data/cleaned_single_cell_data/SingleCell_nocontrols_allChannels_fileID1.rds")


# it is 8.3 GB!!!:
format(object.size(single_cell_raw),units = "auto")



library(rhdf5)
