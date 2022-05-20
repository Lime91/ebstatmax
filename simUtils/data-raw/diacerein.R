# run this script from the simUtils package root: "Rscript data-raw/diacerein.R"

devtools::load_all()
dataset_path <- file.path("..", "data", "Diacerein_study-setup.txt")
diacerein <- simUtils::read_data(dataset_path, simUtils::CONFIG)
usethis::use_data(diacerein, overwrite=TRUE)
