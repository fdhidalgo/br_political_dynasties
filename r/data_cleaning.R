import_txts <- function(txt_files, normalize = FALSE) {
     txts <- readtext::readtext(txt_files)
     setDT(txts)
     txts[, tse_munid := gsub(
          pattern = "^([0-9]{1,})/.*",
          replacement = "\\1",
          x = doc_id
     )]

     txts[, cand_id := gsub(
          pattern = "^[0-9]{1,}/([0-9]{1,})/.*",
          replacement = "\\1",
          x = doc_id
     )]

     if (normalize == TRUE) {
          txts[
               ,
               text := stringi::stri_trans_general(
                    tolower(text),
                    "latin-ascii"
               )
          ]
     }

     ## Remove campaign platforms
     txts <- txts[grepl("proposta|/5_", doc_id, ignore.case = TRUE) == FALSE]
     txts
}

tokenize_sentences <- function(txt_data) {
     ## Use udpipe package to tokenize sentences
     txt_data[, doc_id := paste0(seq_along(txt_data$text))]
     udmodel <- udpipe::udpipe_load_model(file = "./data/portuguese-gsd-ud-2.5-191206.udpipe")

     sentences <- udpipe::udpipe(
          x = txt_data$text, object = udmodel,
          parallel.cores = parallel::detectCores(),
          parser = "none", tagger = "none", trace = TRUE
     ) |>
          as.data.table()
     sentences <- unique(sentences[, .(doc_id, sentence)])
     sentences <- merge(txt_data[, .(doc_id, cand_id, tse_munid)], sentences,
          all.x = FALSE, all.y = FALSE
     )
     sentences
}

extract_filho_pattern <- function(sentences) {
     ## This extracts names from sentences with the "filho de" pattern
     filho_sent <- sentences[sentence %ilike% "\\bfilh[oa] de\\b" |
          sentence %flike% "filho(a)"]
     filho_sent$extract <- sub(".*?(\\bfilh[oa] de\\b|filho\\(a\\) de)",
          replacement = "\\1",
          x = filho_sent$sentence, ignore.case = TRUE
     ) |>
          str_remove(",.*") |>
          str_remove(regex("filh[oa] de|filho\\(a\\) de", ignore_case = TRUE))
     filho_extracted <- filho_sent[, .(split = (str_split(
          extract,
          regex(" e ",
               ignore_case = TRUE
          )
     ))),
     by = c("doc_id", "cand_id", "tse_munid")
     ] |>
          tidyr::unnest(cols = c(split)) |>
          as.data.table()
     filho_extracted[, split := str_squish(split)]
     filho_extracted[, split := ifelse(nchar(split) <= 4, NA, split)]
     filho_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) >= 6, NA, split)]
     filho_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) == 1, NA, split)]
     filho_extracted <- na.omit(filho_extracted[, .(cand_id, tse_munid, split)])
     setnames(filho_extracted, "split", "parent_name")
}

extract_filiacao_pattern <- function(sentences) {
     ## This extracts names from sentences with the "filiacao" pattern
     filiacao_removal_patterns <- c(
          "Endereço.*", "\\bos dados.*", "\\bdos documentos.*",
          "\\bconstantes.*", "\\bCertid[ãa]o.*", "\\bdata\\b.*",
          "\\bCERTIFICO\\b.*", "\\bnacionalidade\\b.*", "\\btotal\\b.*",
          "\\bOcupação\\b.*", "\\bObservações\\b"
     )

     filiacao_sent <- sentences[sentence %ilike% "\\bfilia[cç][aã]o\\b" &
          sentence %ilike% "partid[aá]ria" == FALSE]
     filiacao_sent$extract <- sub(".*?(\\bfilia[cç][aã]o\\b)",
          replacement = "\\1",
          x = filiacao_sent$sentence, ignore.case = TRUE
     ) |>
          gsub(paste(filiacao_removal_patterns, collapse = "|"),
               replacement = "",
               x = _, ignore.case = TRUE
          )
     filiacao_sent[, extract := str_remove(extract, regex("^Filiação", ignore_case = TRUE))]

     filiacao_extracted <- filiacao_sent[, .(split = (str_split(
          extract,
          regex("Filia[çc][ãa]o|;|/| e |\\-",
               ignore_case = TRUE
          )
     ))),
     by = c("doc_id", "cand_id", "tse_munid")
     ] |>
          tidyr::unnest(cols = c(split)) |>
          as.data.table()
     filiacao_extracted[, split := str_squish(split)]
     filiacao_extracted[, split := ifelse(nchar(split) <= 4, NA, split)]
     filiacao_extracted[, split := str_squish(str_remove(string = split, pattern = "\\:"))]
     filiacao_extracted[, split := str_squish(str_remove(string = split, pattern = regex("[12]")))]
     filiacao_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) >= 6, NA, split)]
     filiacao_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) == 1, NA, split)]
     filiacao_extracted <- na.omit(filiacao_extracted[, .(cand_id, tse_munid, split)])
     setnames(filiacao_extracted, "split", "parent_name")
     filiacao_extracted
}


extract_nome_pattern <- function(sentences) {
     ## This extracts names from sentences with the "nome de mai / nome de pai" pattern
     sentences[, nome_ind := grepl("\\bnome (\\bd[aeo] )?m[aã]e\\b|\\bnome (\\bd[aeo] )?pai\\b",
          sentence,
          ignore.case = TRUE
     )]
     nome_sent <- sentences[nome_ind == TRUE]
     mae_name <- gsub(
          pattern = ".*(\\bnome (\\bd[aeo] )?m[aã]e\\b.*).*", replacement = "\\1",
          x = nome_sent$sentence, ignore.case = TRUE
     ) |>
          gsub(
               pattern = "\\bnome (\\bd[aeo] )?pai\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bdata\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnos registros\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnos verificou\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bestado\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bcertid[ãa]o\\b.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bverificou.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnada consta .*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bObservações.*", replacement = "\\1",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnome (\\bd[aeo] )?m[aã]e\\b|:", replacement = "", x = _,
               ignore.case = TRUE
          ) |>
          str_remove(" do $")
     mae_name[lengths(strsplit(mae_name, "\\W+")) > 10] <- NA
     mae_name[mae_name == ""] <- NA
     mae_name <- str_squish(mae_name)

     pai_name <- gsub(
          pattern = ".*(\\bnome (\\bd[aeo] )?pai\\b.*).*", replacement = "\\1",
          x = nome_sent$sentence, ignore.case = TRUE
     )
     pai_name[grepl("\\bnome (\\bd[aeo] )?pai\\b", pai_name, ignore.case = TRUE) == FALSE] <- NA
     pai_name <- gsub(
          pattern = "\\bdata de nascimento.*", replacement = "",
          x = pai_name, ignore.case = TRUE
     ) |>
          gsub(
               pattern = "\\bcertid[ãa]o.*", replacement = "",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnome (\\bd[aeo] )?m[aã]e\\b.*", replacement = "",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\btipo documento.*", replacement = "",
               x = _, ignore.case = TRUE
          ) |>
          gsub(
               pattern = "\\bnome (\\bd[aeo] )?pai\\b|:", replacement = "", x = _,
               ignore.case = TRUE
          )
     pai_name[lengths(strsplit(pai_name, "\\W+")) > 10] <- NA
     pai_name[pai_name == ""] <- NA
     pai_name <- str_squish(pai_name)

     rbind(
          nome_sent[, .(cand_id, tse_munid, parent_name = mae_name, gender = "F")],
          nome_sent[, .(cand_id, tse_munid, parent_name = pai_name, gender = "M")]
     ) |>
          na.omit()
}

extract_names <- function(sentences) {
     ## This function extracts names from the 3 identified patterns
     filho_pattern_parents <- extract_filho_pattern(sentences)
     filiacao_pattern_parents <- extract_filiacao_pattern(sentences)
     nome_pattern_parents <- extract_nome_pattern(sentences)
     rbind(filho_pattern_parents,
          filiacao_pattern_parents,
          nome_pattern_parents,
          fill = TRUE
     )
}

cluster_names <- function(names, threshold) {
     ## This function clusters names based on the similarity of the names
     if (length(names) > 1) {
          stringdists <- stringdist::stringdistmatrix(names, names, method = "jw", p = .1)
          colnames(stringdists) <- names
          rownames(stringdists) <- names
          t <- hclust(as.dist(stringdists), method = "single")
          memb <- cutree(t, h = threshold)
          df <- data.table(name = c(names), b = c(memb), stringsAsFactors = F)
          df <- df[, .(n = .N), by = c("name", "b")]
          df <- df[order(b, n, decreasing = TRUE)]
          merge(df, df[, .(
               clustered_name = head(name, 1),
               n_cat = .N
          ), by = "b"])[, .(name, clustered_name)]
     } else {
          data.table(name = names, clustered_name = names)
     }
}

harmonize_names <- function(extracted_names) {
     ## This function normalizes and clusters names
     ## It selects 2 names with most frequent mentions (keeping ties)
     extracted_names <- extracted_names[parent_name != ""]
     extracted_names[, unnormalized_parent_name := parent_name]
     extracted_names[, parent_name := str_squish(tolower(parent_name))]
     extracted_names[, parent_name := str_remove_all(parent_name, "[:punct:]|\\||[:digit:]")]
     extracted_names[, parent_name := stringi::stri_trans_general(parent_name, "latin-ascii")]
     extracted_names[, parent_name := str_squish(str_remove_all(parent_name, "\\bde\\b|\\bda\\b|\\bdos\\b|\\bdas\\b|\\bdo\\b"))]

     # save unnormalized names and collapse
     unnormalized_names <- unique(extracted_names[, .(cand_id, parent_name, unnormalized_parent_name)])
     ## choose one unormalized name
     unnormalized_names <- unnormalized_names[, head(.SD, 1), by = c("cand_id", "parent_name")]

     extracted_names[, unnormalized_parent_name := NULL]


     clustered_names <- group_by(extracted_names, cand_id, tse_munid) %>%
          tidyr::nest() %>%
          mutate(clustered_names = purrr::map(data, ~ cluster_names(.x$parent_name, threshold = .1))) %>%
          select(cand_id, tse_munid, clustered_names) %>%
          tidyr::unnest(cols = c(clustered_names)) %>%
          rename(parent_name = name)


     extracted_names <- merge(extracted_names, clustered_names)
     # Preserve documented gender and randomly choose ties
     gender <- unique(na.omit(extracted_names[, .(cand_id, clustered_name, gender)]))
     gender <- gender[, head(.SD, 1), by = c("cand_id", "clustered_name")]
     setnames(gender, "clustered_name", "parent_name")

     extracted_names[, parent_name := NULL]
     setnames(extracted_names, "clustered_name", "parent_name")

     ## Calculate number of mentions of each name variant
     extracted_names <- extracted_names[, .(n_mentions = .N),
          by = c("cand_id", "tse_munid", "parent_name")
     ]

     # To choose two parents, choose top 2 mentions of each name variant and keep ties
     extracted_names <- group_by(extracted_names, cand_id, tse_munid) %>%
          slice_max(order_by = n_mentions, n = 2) %>%
          mutate(n_parents = n()) %>%
          as.data.table()

     # Merge in gender for those who have it
     extracted_names <- merge(extracted_names, gender, all.x = TRUE, all.y = FALSE)
     # Merge in unnormalized name
     extracted_names <- merge(extracted_names, unnormalized_names, all.x = TRUE, all.y = FALSE)

     extracted_names[, .(cand_id, tse_munid, parent_name, n_mentions, n_parents, gender, unnormalized_parent_name)]
}

clean_cand_data <- function(cand_data) {
     cand_data <- cand_data[!is.na(id_candidato_bd)]
     cand_data <- cand_data[!is.na(id_municipio)]
     cand_data <- cand_data[!is.na(data_nascimento)]
     cand_data[, cand_normalized_name := tolower(nome)]
     cand_data[, cand_normalized_name := stringi::stri_trans_general(cand_normalized_name, "latin-ascii")]
     cand_data[, cand_normalized_name := str_squish(str_remove_all(
          cand_normalized_name,
          "\\bde\\b|\\bda\\b|\\bdos\\b|\\bdas\\b|\\bdo\\b"
     ))]
     cand_data
}

calc_shared_words <- function(string, string_vec) {
     ## this function calculates the normalized share of words shared between two strings
     ## Note that this disregards word order
     string <- string %>%
          str_extract_all(pattern = "\\w+") %>%
          unlist()
     n_common_words <- purrr::map_int(
          .x = str_extract_all(string_vec, pattern = "\\w+"),
          .f = ~ length(intersect(string, .x))
     )
     max_words <- purrr::map_int(
          .x = str_extract_all(string_vec, pattern = "\\w+"),
          .f = ~ length(unique(c(string, .x)))
     )
     prop_common <- n_common_words / max_words
     prop_common
}

get_string_sims_1cand <- function(parent_normalized_name, child_sequencial, cand_data) {
     ## This function calculates various string similarity measures between parent's name and political candidates

     # child_id_candidato_bd is the identifier for the child of the parent
     # cand_data should usually be subsetted to a municipality (or state)
     cand_names <- unique(cand_data[
          ,
          .(id_candidato_bd, sequencial, cand_normalized_name, cand_bdate = data_nascimento)
     ])
     cand_names$parent_normalized_name <- parent_normalized_name
     cand_names$child_sequencial <- child_sequencial
     cand_names$child_bdate <- cand_data[sequencial == bit64::as.integer64(child_sequencial) & ano == 2020, "data_nascimento"][[1]][1]
     cand_names$child_id_candidato_bd <- cand_data[sequencial == bit64::as.integer64(child_sequencial) & ano == 2020, "id_candidato_bd"][[1]][1]
     cand_names$bod_diff_years <- lubridate::interval(
          lubridate::ymd(cand_names$cand_bdate),
          lubridate::ymd(cand_names$child_bdate)
     ) / lubridate::years(1)

     ## Name string lengths
     cand_names$parent_string_length <- nchar(parent_normalized_name)
     cand_names$child_string_length <- nchar(cand_names$cand_normalized_name)

     ## Estimate gender
     cand_names$parent_female_prob <- genderBR::get_gender(cand_names$parent_normalized_name,
          prob = TRUE
     )
     cand_names$child_female_prob <- genderBR::get_gender(cand_names$cand_normalized_name,
          prob = TRUE
     )

     ## Proportion of shared words across two strings
     cand_names$prop_shared_words <- calc_shared_words(parent_normalized_name, cand_names$cand_normalized_name)

     ## String similarity measures
     cand_names$osa_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "osa"
     )
     cand_names$lv_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "lv"
     )
     cand_names$dl_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "dl"
     )
     cand_names$lcs_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "lcs"
     )
     cand_names$qgram1_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "qgram", q = 1
     )
     cand_names$qgram2_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "qgram", q = 2
     )
     cand_names$jaro_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "jw", p = 0
     )
     cand_names$jw_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "jw", p = .2
     )
     cand_names <- cand_names[order(osa_sim)]
     cand_names
}

get_string_sims <- function(parent_names, cand_data) {
     furrr::future_map_dfr(
          .x = seq_len(nrow(parent_names)),
          .f = ~ get_string_sims_1cand(
               parent_normalized_name = parent_names$parent_name[.x],
               child_sequencial = parent_names$cand_id[.x],
               cand_data = cand_data[id_municipio_tse == as.integer(parent_names$tse_munid[.x])]
          ),
          .options = furrr::furrr_options(packages = "data.table")
     )
}

clean_training_data <- function(training_data, cand_parent_string_dists) {
     setDT(training_data)

     training_data$child_sequencial <- as.character(training_data$child_sequencial)
     training_data$bod_diff_years <- NULL

     training_data <- merge(training_data, cand_parent_string_dists,
          by = c(
               "id_candidato_bd", "sequencial", "cand_normalized_name",
               "parent_normalized_name", "child_sequencial"
          ),
          all.x = TRUE,
          all.y = FALSE
     )
     training_data
}

gen_cand_matches <- function(training_data, cand_parent_string_dists) {

  ##Set up machine learning model
     ranger_recipe <-
          recipe(formula = match ~ bod_diff_years + parent_string_length + child_string_length +
               prop_shared_words + osa_sim + lv_sim + lcs_sim + qgram1_sim + qgram2_sim +
               jaro_sim + jw_sim + parent_female_prob + child_female_prob, data = training_data) %>%
          step_string2factor(one_of("match"), skip = TRUE) %>%
          step_mutate(diff_gender_prob = parent_female_prob - child_female_prob) %>%
          step_impute_mean(diff_gender_prob, bod_diff_years) %>%
          step_rm(parent_female_prob, child_female_prob)

     ## Tune random forest hyper-parameters
     ranger_spec <-
          rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
          set_mode("classification") %>%
          set_engine("ranger")

     ranger_workflow <-
          workflow() %>%
          add_recipe(ranger_recipe) %>%
          add_model(ranger_spec)

     ## Five-fold cross-validation
     cvfolds <- vfold_cv(training_data, v = 5)

     set.seed(2803)
     ranger_tune <-
          tune_grid(ranger_workflow,
               resamples = cvfolds, grid = 50,
               metrics = metric_set(accuracy, precision, recall, spec)
          )

     ##Select model based on accuracy
     best_rf <- select_best(ranger_tune, metric = "accuracy")
     final_wf <- ranger_workflow %>%
          finalize_workflow(best_rf)

     fitted_model <- fit(final_wf, data = training_data)

     preds <- predict(fitted_model, new_data = cand_parent_string_dists, type = "prob")
     cand_parent_string_dists$match_prob <- preds$.pred_y

     ##Choose matches that have a probability of a match greater than .5
     matches <- cand_parent_string_dists[
          match_prob > .5 & is.na(child_id_candidato_bd) == FALSE,
          .(parent_id_candidato_bd = id_candidato_bd, child_id_candidato_bd, match_prob)
     ] |>
          unique()
     matches
}

add_identifiers <- function(harmonized_parent_names, cand_data20, cand_data) {
  ##sequencial data seems to be wrong in base dos dados data, so using TSE data to obtain CPF
  ##then use CPF to merge in base dos dados identifier
  harmonized_cpf <- merge(harmonized_parent_names,
                                   cand_data20[, .(sequencial =
                                                     as.character(SQ_CANDIDATO), cpf = NR_CPF_CANDIDATO)],
                                   by.x = "cand_id", by.y = "sequencial",
                                   all.x = TRUE, all.y = FALSE)

  parents_data <- merge(harmonized_cpf,
        cand_data[ano == 2020, .(cpf, id_candidato_bd)],
        by.x = "cpf", by.y = "cpf", all.x = TRUE, all.y = FALSE)
  setnames(parents_data, c("cpf", "id_candidato_bd"), c("cpf_child", "id_candidato_bd_child"))
  parents_data
}

gen_nonmissing_states <- function(cand_data, parents_data, prop_missing) {
  #number of parents found by candidate
  parents_found <- merge(cand_data[ano == 2020 & cargo == "prefeito", .(id_candidato_bd, sigla_uf)],
                         unique(parents_data[, .(id_candidato_bd_child, n_parents)]),
                         by.x = "id_candidato_bd", by.y = "id_candidato_bd_child", all.x = TRUE, all.y = FALSE)
  parents_found[is.na(n_parents), n_parents := 0]

  ##missingness by state
  prop_missing_by_state <- parents_found[, .(prop_missing = mean(n_parents == 0)), by = "sigla_uf"][order(prop_missing)]
  nonmissing_states <- prop_missing_by_state$sigla_uf[prop_missing_by_state$prop_missing <= prop_missing]
  nonmissing_states
}

gen_cand_subset <- function(cand_data, nonmissing_states, name_matches,
                            elec_results, vp_codes) {
  ##candidate data in non-missing states
  cand_subset <- cand_data[ano == 2020 & sigla_uf %in% nonmissing_states & cargo == "prefeito"]
  ##dynastic candidates are candidates with any parent who has run before
  cand_subset$dynastic <- ifelse(cand_subset$id_candidato_bd %in% name_matches$child_id_candidato_bd,
                                 TRUE, FALSE)
  ##parent candidate data
  parent_cand_data <- cand_data[id_candidato_bd %in% name_matches$parent_id_candidato_bd]
  ##parent was a candidate for mayor or vice mayor
  name_matches[, parent_mayor_cand := ifelse(parent_id_candidato_bd %in%
                                               parent_cand_data$id_candidato_bd[parent_cand_data$cargo == "prefeito" |
                                                                                  parent_cand_data$cargo == "vice-prefeito"],
                                             TRUE, FALSE)]
  ##Note that vice-prefeito is not in results data, so add coalition results for vice-prefeito
  ##parent was a mayor or vice mayor
  vp_results <- tibble(ano = vp_codes$ano, turno = 1,
         tipo_eleicao = "eleicao ordinaria",
         cargo = "vice-prefeito",
         id_candidato_bd = vp_codes$vp_id_candidato_bd,
         resultado = vp_codes$resultado,
         votos = vp_codes$votos)
  elec_results <- bind_rows(elec_results, vp_results)

  name_matches[, parent_elected_mayor := ifelse(parent_id_candidato_bd %in%
                                                  elec_results$id_candidato_bd[(elec_results$cargo == "prefeito" |
                                                                                  elec_results$cargo == "vice-prefeito") &
                                                                                 elec_results$resultado == "eleito"],
                                                TRUE, FALSE)]
  ##parent ran in 2016
  name_matches[, parent_ran_mayor_2016 := ifelse(parent_id_candidato_bd %in%
                                                   cand_data$id_candidato_bd[cand_data$ano == 2016 & cand_data$cargo %in% c("prefeito", "vice-prefeito")],
                                                 TRUE, FALSE)]
  #create dynastic mayor dummy
  cand_subset$dynastic_mayor <- ifelse(cand_subset$id_candidato_bd %in%
                                         name_matches$child_id_candidato_bd[name_matches$parent_mayor_cand == TRUE],
                                       TRUE, FALSE)
  ##create dynastic mayor and elected dummy
  cand_subset$dynastic_elected_mayor <- ifelse(cand_subset$id_candidato_bd %in%
                                                 name_matches$child_id_candidato_bd[name_matches$parent_elected_mayor == TRUE],
                                               TRUE, FALSE)
  cand_subset$dynastic_mayor16 <- ifelse(cand_subset$id_candidato_bd %in%
                                           name_matches$child_id_candidato_bd[name_matches$parent_ran_mayor_2016 == TRUE],
                                         TRUE, FALSE)

  ##create results data
  vote_totals20 <- elec_results[ano == 2020 & turno == 1 & tipo_eleicao == "eleicao ordinaria" & cargo == "prefeito",
                                .(total_votes = sum(votos)),
                                by = c("id_municipio_tse")]
  elec_prefeito_results20 <- merge(elec_results[ano == 2020 & turno == 1 & tipo_eleicao == "eleicao ordinaria" &
                                                  cargo == "prefeito",
                                                .(id_candidato_bd,  id_municipio_tse, resultado, votos)],
                                   vote_totals20, all.x = TRUE, all.y = FALSE)

  cand_subset <- merge(cand_subset, elec_prefeito_results20,
                       all.x = TRUE, all.y = FALSE)
  cand_subset[, elected := ifelse(resultado != "nao eleito", 1, 0)]
  cand_subset[ , vote_pct := 100 * votos / total_votes]
  cand_subset
}

gen_vp_codes <- function(cand_data, elec_results) {
  pref_cands <- elec_results[cargo == "prefeito" & !is.na(id_candidato_bd),
                             .(ano, id_municipio_tse, pref_id_candidato_bd = id_candidato_bd,
                               numero = numero_candidato)]
  vp_cands <- cand_data[cargo == "vice-prefeito",
                        .(ano, id_municipio_tse, vp_id_candidato_bd = id_candidato_bd, numero)]
  vp_cands <- merge(vp_cands, pref_cands, all.x = FALSE, all.y = FALSE)
  vp_cands[, numero := NULL]
  vp_cands <- merge(vp_cands, elec_results[, c("ano", "id_municipio_tse", "id_candidato_bd",
                                               "resultado", "votos")],
                    by.x = c("ano", "id_municipio_tse", "pref_id_candidato_bd"),
                    by.y = c("ano", "id_municipio_tse", "id_candidato_bd"),
                    all.x = TRUE, all.y = FALSE)
  vp_cands
}
