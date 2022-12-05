# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

library(conflicted)

# Set target options:
tar_option_set(
  packages = c("data.table", "stringr", "dplyr", "tidymodels"), # packages that your targets need to run
  format = "rds" # default storage format,
  # Set other options as needed.
)


future::plan(future.callr::callr(workers = parallelly::availableCores() - 1))
options("future.globals.maxSize" = 2000 * 1024^2)

source("./r/data_cleaning.R")

data.table::setDTthreads(parallel::detectCores() - 1)


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
  tar_target(name = vp_codes,
             command = gen_vp_codes(cand_data, elec_results)
             ),

  tar_target(
    name = cand_data20_file,
    command = "./data/consulta_cand_2020_BRASIL.csv.gz",
    format = "file"
  ),
  tar_target(
    name = cand_data20,
    command = fread(cand_data20_file,
                    encoding = "Latin-1", keepLeadingZeros = TRUE),
    format = "fst_dt"
  ),
  tar_target(
    name = elec_results_file,
    command = "./data/electoral_results.csv.gz",
    format = "file"
  ),
  tar_target(
    name = elec_results,
    command = fread(elec_results_file),
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
    name = parents_data,
    command = add_identifiers(harmonized_parent_names = harmonized_parent_names,
                              cand_data20 = cand_data20,
                              cand_data = cand_data)
  ),

  tar_target(name = nonmissing_states,
             command = gen_nonmissing_states(cand_data, parents_data,
                                             prop_missing = .15)),

  tar_target(name = cand_subset,
             command = gen_cand_subset(cand_data = cand_data,
                                       nonmissing_states = nonmissing_states,
                                       name_matches = name_matches,
                                       elec_results = elec_results,
                                       vp_codes = vp_codes
                                       )),
  tar_target(
    name = exported_parents,
    command = fwrite(parents_data, "./output/politician_parents.csv")
  )
)
