# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

library(conflicted)

# Set target options:
tar_option_set(
  packages = c("data.table", "stringr", "dplyr", "tidymodels"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr(workers = parallelly::availableCores() - 1))
options("future.globals.maxSize" = 2000 * 1024^2)

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
    name = harmonized_parent_names,
    command = harmonize_names(extracted_names),
    format = "fst_dt"
  ),
  ## Name match with historical candidate data
  tar_target(
    name = cand_data_file,
    command = "./data/cand_data.csv.gz",
    format = "file"
  ),
  tar_target(
    name = cand_data,
    command = clean_cand_data(fread(cand_data_file)),
    format = "fst_dt"
  ),
  tar_target(
    name = cand_parent_string_dists,
    command = get_string_sims(
      parent_names = harmonized_parent_names,
      cand_data = cand_data[, .(
        ano, id_candidato_bd, id_municipio_tse, sequencial,
        cand_normalized_name, data_nascimento
      )]
    ),
    format = "fst_dt"
  ),
  tar_target(
    name = training_data_files,
    command = list.files("./data/string_matching_training_data", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    name = training_data,
    command = clean_training_data(
      unique(rbindlist(lapply(training_data_files, fread))),
      cand_parent_string_dists
    ),
    format = "fst_dt"
  ),
  tar_target(
    name = name_matches,
    command = gen_cand_matches(training_data, cand_parent_string_dists)
  ),
  tar_target(
    name = exported_parents,
    command = fwrite(harmonized_parent_names, "./output/politician_parents.csv")
  ),
  tar_render(
    name = descriptive_statistics,
    path = "./output/descriptive_statistics.Rmd"
  )
)
