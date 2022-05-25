# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

library(conflicted)

# Set target options:
tar_option_set(
  packages = c("data.table", "stringr", "dplyr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Load the R scripts with your custom functions:
# for (file in list.files("R", full.names = TRUE)) source(file)
source("./r/data_cleaning.R")

data.table::setDTthreads(parallel::detectCores() - 1)


# Replace the target list below with your own:
list(
  tar_target(
    name = txt_files,
    command = list.files("./data/txt/",
      recursive = TRUE, pattern = "txt$",
      full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    name = sentences,
    command = tokenize_sentences(import_txts(txt_files)),
    format = "fst_dt"
  ),
  tar_target(extracted_names,
    command = extract_names(sentences)
  ),
  tar_target(
    name = harmonized_names,
    command = harmonize_names(extracted_names),
    format = "fst_dt"
  )
)
