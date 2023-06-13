import_txts <- function(txt_files, normalize = FALSE) {
     get_txt <- function(txt_file) {
          txt <- readtext::readtext(txt_file)
          txt$tse_munid <- gsub(".*txt//([0-9]{1,})/.*", "\\1", txt_file)
          txt$cand_id <- gsub(".*/([0-9]{1,}).zip", "\\1", txt_file)
          ## subset on txt files with 3 specific patterns
          txt[txt$text %ilike% "\\bfilh[oa] de\\b" |
               txt$text %flike% "filho(a)" |
               txt$text %ilike% "\\bfilia[cç][aã]o\\b" |
               txt$text %ilike% "\\bnome (\\bd[aeo] )?m[aã]e\\b" |
               txt$text %ilike% "\\bnome (\\bd[aeo] )?pai\\b", ]
     }
     txts <- map(txt_files, get_txt,
          .progress = "Importing Text Files"
     )
     txts <- rbindlist(txts)
     setDT(txts)

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

     ## Remove very large text documents
     txts <- txts[nchar(txts$text) < 1000000]

     txts
}

extract_patterns <- function(txts) {
     filiacao_txts <- txts[str_detect(str_to_lower(txts$text), "\\bfilia[cç][aã]o\\b")]
     filiacao_txts$extr <- map(filiacao_txts$text,
          get_words_after_phrase,
          phrase = "\\bfilia[cç][aã]o\\b",
          n_words = 15
     ) |>
          unlist()
     filiacao_txts$pattern <- "filiacao"

     filho_txts <- txts[str_detect(str_to_lower(txts$text), "\\bfilh[oa] de\\b")]
     filho_txts$extr <- map(filho_txts$text,
          get_words_after_phrase,
          phrase = "\\bfilh[oa] de\\b",
          n_words = 15
     ) |>
          unlist()
     filho_txts$pattern <- "filho"

     mae_txts <- txts[str_detect(
          str_to_lower(txts$text),
          "\\bnome (\\bd[aeo] )?m[aã]e\\b"
     )]
     mae_txts$extr <- map(mae_txts$text,
          get_words_after_phrase,
          phrase = "\\bnome (\\bd[aeo] )?m[aã]e\\b",
          n_words = 10
     ) |>
          unlist()
     mae_txts$pattern <- "mae"

     pai_txts <- txts[str_detect(
          str_to_lower(txts$text),
          "\\bnome (\\bd[aeo] )?pai\\b"
     )]
     pai_txts$extr <- map(pai_txts$text,
          get_words_after_phrase,
          phrase = "\\bnome (\\bd[aeo] )?pai\\b",
          n_words = 10
     ) |>
          unlist()
     pai_txts$pattern <- "pai"

     extracted <- rbindlist(list(filiacao_txts, filho_txts, mae_txts, pai_txts))
     extracted[, text := NULL]

     extracted
}

get_words_after_phrase <- function(text, phrase, n_words = 10) {
     text <- str_to_lower(text) |>
          paste(collapse = " ")

     # Locate the phrase in the text
     loc <- str_locate(text, phrase)

     # If the phrase is not found, return a warning
     if (is.na(loc[1])) {
          warning("Phrase not found in the text.")
          return(NULL)
     }

     # Extract the part of the text after the phrase
     following_text <- substr(text, loc[1], nchar(text))

     # Split this text into words
     words_list <- unlist(strsplit(following_text, "\\s+"))

     # Return the next 10 words if they exist, else return as many as are available
     if (length(words_list) >= n_words) {
          return(paste(words_list[1:n_words], collapse = " "))
     } else {
          return(paste(words_list, collapse = " "))
     }
}

extract_names <- function(extracted_txts) {
     filho_extracted <- extract_filho_pattern(extracted_txts[pattern == "filho"])
     mae_extracted <- extract_mae_pattern(extracted_txts[pattern == "mae"])
     pai_extracted <- extract_pai_pattern(extracted_txts[pattern == "pai"])
     fil_extracted <- extract_filiacao_pattern(extracted_txts[pattern == "filiacao"])

     extracted <- rbindlist(list(filho_extracted, mae_extracted, pai_extracted, fil_extracted), fill = TRUE)

     extracted
}


extract_filho_pattern <- function(filho_extracted) {
     ## remove everything after comma
     filho_extracted[, cleaned := str_remove(extr, ",.*")]
     ## remove punctuation, digits, and control characters
     filho_extracted[, cleaned := str_remove_all(cleaned, "[[:punct:]]|[[:cntrl:]]|\\||[[:digit:]]")]
     filho_extracted[, cleaned := str_squish(cleaned)]
     filho_extracted[, cleaned := str_remove(
          cleaned,
          regex("filh[oa] de|filho\\(a\\) de", ignore_case = TRUE)
     )]
     ## remove accents and diacritics
     filho_extracted[, cleaned := stringi::stri_trans_general(cleaned, "latin-ascii")]

     ## split on the word "e" and reshape dataset
     filho_extracted <- filho_extracted[, .(split = (str_split(
          cleaned,
          regex(" e ",
               ignore_case = TRUE
          )
     ))),
     by = c("doc_id", "cand_id", "tse_munid")
     ]

     filho_extracted <- filho_extracted[,
          .(split = unlist(split)),
          by = c("doc_id", "cand_id", "tse_munid")
     ]

     filho_extracted[, split := str_squish(split)]
     filho_extracted[, split := ifelse(nchar(split) <= 4, NA, split)]
     filho_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) >= 6, NA, split)]
     filho_extracted[, split := ifelse(lengths(strsplit(split, "\\W+")) == 1, NA, split)]
     filho_extracted <- na.omit(filho_extracted[, .(cand_id, tse_munid, split)])
     setnames(filho_extracted, "split", "parent_name")
}

extract_filiacao_pattern <- function(fil_extracted) {
     fil_extracted[, cleaned := stringi::stri_trans_general(extr, "latin-ascii")]
     ## remove "filiacao"
     fil_extracted[, cleaned := stringr::str_remove(cleaned, "^filiacao")]

     ## Split and reshape
     fil_extracted <- fil_extracted[, .(split = (str_split(
          cleaned,
          regex(";|/| e |\\-|filiacao",
               ignore_case = TRUE
          )
     ))),
     by = c("doc_id", "cand_id", "tse_munid")
     ]

     fil_extracted <- fil_extracted[,
          .(cleaned = unlist(split)),
          by = c("doc_id", "cand_id", "tse_munid")
     ]

     ## remove punctuation, digits, and control characters
     fil_extracted[, cleaned := str_remove_all(cleaned, "[[:punct:]]|[[:cntrl:]]|\\||[[:digit:]]")]

     nonname_words <- c(
          "de", "certidao", "a", "observacoes", "expedida", "data",
          "nada", "do", "gratuitamente", "consta", "estado", "civil", "nos", "nome",
          "dos", "nascimento", "registros", "ou", "casado", "cpf", "informado", "nao",
          "processo", "distribuicao", "situacao", "das", "as", "nacionalidade", "solteiro",
          "o", "nº", "emitida", "emissao", "emitida", "expedicao",
          "cpfcnpj", "documentos", "email", "telefone", "documentocpf", "rg",
          "solicitacao", "cadastrada", "endereco", "declarou", "possuir", "registro",
          "genitor", "filiacao", "dados", "nessa", "brasileira", "certifico", "casadoa",
          "conforme", "declarada", "ocupacao", "peloa", "eleitora", "residencial", "extratos",
          "solicitante", "total", "solteiroa", "eleitor", "vigente", "abaixo", "rua", "povoado",
          "partidaria", "anexos", "referido", "legislacao", "identificado", "av", "documento", "oficial",
          "identificacao", "respectivo", "zona", "constantes", "informados", "divorciadoa", "avenida", "praca",
          "dispoe", "centro", "pai", "mae", "nascidoa", "pr", "nesta", "residencia", "eleitorais",
          "naturalidade", "rural", "profissao", "completo", "pesquisas", "cidade", "distribuidor", "dr"
     )
     nonname_words <- c(
          stringi::stri_trans_general(quanteda::stopwords("pt"), "latin-ascii"),
          nonname_words,
          letters
     )

     fil_extracted[, cleaned := str_squish(remove_words(nonname_words, cleaned))]
     fil_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) >= 6, NA, cleaned)]
     fil_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) == 1, NA, cleaned)]
     fil_extracted[cleaned == "", cleaned := NA_character_]
     fil_extracted <- fil_extracted[!is.na(cleaned)]
     fil_extracted[, .(cand_id, tse_munid, parent_name = cleaned)]
}

remove_words <- function(patterns, strings) {
     for (pattern in patterns) {
          strings <- str_replace_all(strings, regex(paste0("\\b", pattern, "\\b"), ignore_case = TRUE), "")
     }
     return(strings)
}

extract_mae_pattern <- function(mae_extracted) {
     mae_extracted[, cleaned := stringi::stri_trans_general(extr, "latin-ascii")]
     ## remove "nome da mae" from beginning of string
     mae_extracted[, cleaned := stringr::str_remove(cleaned, "^nome.*mae")]
     ## remove punctuation, digits, and control characters
     mae_extracted[, cleaned := str_remove_all(cleaned, "[[:punct:]]|[[:cntrl:]]|\\||[[:digit:]]|°")]
     ## remove "nome do pai" and everything after
     mae_extracted[, cleaned := str_remove(cleaned, "nome.*pai.*")]

     nonname_words <- c(
          "de", "certidao", "a", "observacoes", "expedida", "data",
          "nada", "do", "gratuitamente", "consta", "estado", "civil", "nos", "nome",
          "dos", "nascimento", "registros", "ou", "casado", "cpf", "informado", "nao",
          "processo", "distribuicao", "situacao", "das", "as", "nacionalidade", "solteiro",
          "o", "nº", "emitida", "emissao", "emitida", "expedicao",
          "cpfcnpj", "documentos", "email", "telefone", "documentocpf", "rg",
          "solicitacao", "cadastrada"
     )
     nonname_words <- c(
          stringi::stri_trans_general(quanteda::stopwords("pt"), "latin-ascii"),
          nonname_words,
          letters
     )

     mae_extracted[, cleaned := str_squish(remove_words(nonname_words, cleaned))]
     mae_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) >= 6, NA, cleaned)]
     mae_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) == 1, NA, cleaned)]
     mae_extracted[cleaned == "", cleaned := NA_character_]
     mae_extracted <- mae_extracted[!is.na(cleaned)]

     mae_extracted[, .(cand_id, tse_munid, parent_name = cleaned, gender = "F")]
}

extract_pai_pattern <- function(pai_extracted) {
     pai_extracted[, cleaned := stringi::stri_trans_general(extr, "latin-ascii")]
     ## remove "nome da pai" and everything after
     pai_extracted[, cleaned := stringr::str_remove(cleaned, "nome mae.*|nome da mae.*|nome de mae.*")]
     pai_extracted[, cleaned := stringr::str_remove(cleaned, "^nome do pai|^nome pai|^nome de pai")]
     ## remove punctuation, digits, and control characters
     pai_extracted[, cleaned := str_remove_all(cleaned, "[[:punct:]]|[[:cntrl:]]|\\||[[:digit:]]|°")]

     nonname_words <- c(
          "de", "certidao", "a", "observacoes", "expedida", "data",
          "nada", "do", "gratuitamente", "consta", "estado", "civil", "nos", "nome",
          "dos", "nascimento", "registros", "ou", "casado", "cpf", "informado", "nao",
          "processo", "distribuicao", "situacao", "das", "as", "nacionalidade", "solteiro",
          "o", "nº", "emitida", "emissao", "emitida", "expedicao",
          "cpfcnpj", "documentos", "email", "telefone", "documentocpf", "rg",
          "solicitacao", "cadastrada", "endereco", "declarou", "possuir", "registro",
          "genitor"
     )
     nonname_words <- c(
          stringi::stri_trans_general(quanteda::stopwords("pt"), "latin-ascii"),
          nonname_words,
          letters
     )

     pai_extracted[, cleaned := str_squish(remove_words(nonname_words, cleaned))]

     pai_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) >= 6, NA, cleaned)]
     pai_extracted[, cleaned := ifelse(lengths(strsplit(cleaned, "\\W+")) == 1, NA, cleaned)]
     pai_extracted[cleaned == "", cleaned := NA_character_]
     pai_extracted <- pai_extracted[!is.na(cleaned)]

     pai_extracted[, .(cand_id, tse_munid, parent_name = cleaned, gender = "M")]
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

     ## remove names with ony 1 word or less
     extracted_names[, n_words := str_count(parent_name, "\\w+")]
     extracted_names <- extracted_names[n_words > 1]

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

     ## Proportion of shared words across two strings
     cand_names$prop_shared_words <- calc_shared_words(parent_normalized_name, cand_names$cand_normalized_name)

     ## String similarity measures
     cand_names$osa_sim <- stringdist::stringsim(parent_normalized_name,
          cand_names$cand_normalized_name,
          method = "osa"
     )

     ## Remove pairs with very little similarity
     cand_names <- cand_names[prop_shared_words > 0 & osa_sim >= .6]

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

     ## Estimate gender
     cand_names$parent_female_prob <- genderBR::get_gender(cand_names$parent_normalized_name,
          prob = TRUE
     )
     cand_names$child_female_prob <- genderBR::get_gender(cand_names$cand_normalized_name,
          prob = TRUE
     )

     cand_names
}

get_string_sims <- function(parent_names, cand_data) {
     furrr::future_map(
          .x = seq_len(nrow(parent_names)),
          .f = ~ get_string_sims_1cand(
               parent_normalized_name = parent_names$parent_name[.x],
               child_sequencial = parent_names$cand_id[.x],
               cand_data = cand_data[id_municipio_tse == as.integer(parent_names$tse_munid[.x])]
          ),
          .progress = TRUE, .options = furrr::furrr_options(packages = "data.table")
     ) |>
          rbindlist()
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
     training_data <- training_data[is.na(prop_shared_words) == FALSE]
     training_data <- training_data[, .SD[1] ,by = id_candidato_bd]
     training_data
}

tune_name_matching_model <- function(training_data, cand_parent_string_dists) {
     ## Set up machine learning model
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

     list(workflow = ranger_workflow, tune_out = ranger_tune)
}

gen_cand_matches <- function(workflow, tune_out, training_data, cand_parent_string_dists) {
     ## Select model based on accuracy
     best_rf <- select_best(tune_out, metric = "accuracy")
     final_wf <- workflow %>%
          finalize_workflow(best_rf)

     fitted_model <- fit(final_wf, data = training_data)

     preds <- predict(fitted_model, new_data = cand_parent_string_dists, type = "prob")
     cand_parent_string_dists$match_prob <- preds$.pred_y

     ## Choose matches that have a probability of a match greater than .5
     matches <- cand_parent_string_dists[
          match_prob > .5 & is.na(child_id_candidato_bd) == FALSE,
          .(parent_id_candidato_bd = id_candidato_bd, child_id_candidato_bd, match_prob)
     ] |>
          unique()
     matches
}

add_identifiers <- function(harmonized_parent_names, cand_data20, cand_data) {
     ## sequencial data seems to be wrong in base dos dados data, so using TSE data to obtain CPF
     ## then use CPF to merge in base dos dados identifier
     harmonized_cpf <- merge(harmonized_parent_names,
          cand_data20[, .(
               sequencial =
                    as.character(SQ_CANDIDATO), cpf = NR_CPF_CANDIDATO
          )],
          by.x = "cand_id", by.y = "sequencial",
          all.x = TRUE, all.y = FALSE
     )

     parents_data <- merge(harmonized_cpf,
          cand_data[ano == 2020, .(cpf, id_candidato_bd)],
          by.x = "cpf", by.y = "cpf", all.x = TRUE, all.y = FALSE
     )
     setnames(parents_data, c("cpf", "id_candidato_bd"), c("cpf_child", "id_candidato_bd_child"))
     parents_data
}

gen_nonmissing_states <- function(cand_data, parents_data, prop_missing) {
     # number of parents found by candidate
     parents_found <- merge(cand_data[ano == 2020 & cargo == "prefeito", .(id_candidato_bd, sigla_uf)],
          unique(parents_data[, .(id_candidato_bd_child, n_parents)]),
          by.x = "id_candidato_bd", by.y = "id_candidato_bd_child", all.x = TRUE, all.y = FALSE
     )
     parents_found[is.na(n_parents), n_parents := 0]

     ## missingness by state
     prop_missing_by_state <- parents_found[, .(prop_missing = mean(n_parents < 2)), by = "sigla_uf"][order(prop_missing)]
     nonmissing_states <- prop_missing_by_state$sigla_uf[prop_missing_by_state$prop_missing <= prop_missing]
     nonmissing_states
}

gen_cand_subset <- function(cand_data, nonmissing_states, name_matches,
                            elec_results, vp_codes) {
     ## candidate data in non-missing states
     cand_subset <- cand_data[ano == 2020 & sigla_uf %in% nonmissing_states & cargo == "prefeito"]
     ## dynastic candidates are candidates with any parent who has run before
     cand_subset$dynastic <- ifelse(cand_subset$id_candidato_bd %in% name_matches$child_id_candidato_bd,
          TRUE, FALSE
     )
     ## parent candidate data
     parent_cand_data <- cand_data[id_candidato_bd %in% name_matches$parent_id_candidato_bd]
     ## parent was a candidate for mayor or vice mayor
     name_matches[, parent_mayor_cand := ifelse(parent_id_candidato_bd %in%
          parent_cand_data$id_candidato_bd[parent_cand_data$cargo == "prefeito" |
               parent_cand_data$cargo == "vice-prefeito"],
     TRUE, FALSE
     )]
     ## Note that vice-prefeito is not in results data, so add coalition results for vice-prefeito
     ## parent was a mayor or vice mayor
     vp_results <- tibble(
          ano = vp_codes$ano, turno = 1,
          tipo_eleicao = "eleicao ordinaria",
          cargo = "vice-prefeito",
          id_candidato_bd = vp_codes$vp_id_candidato_bd,
          resultado = vp_codes$resultado,
          votos = vp_codes$votos
     )
     elec_results <- bind_rows(elec_results, vp_results)

     name_matches[, parent_elected_mayor := ifelse(parent_id_candidato_bd %in%
          elec_results$id_candidato_bd[(elec_results$cargo == "prefeito" |
               elec_results$cargo == "vice-prefeito") &
               elec_results$resultado == "eleito"],
     TRUE, FALSE
     )]
     ## parent ran in 2016
     name_matches[, parent_ran_mayor_2016 := ifelse(parent_id_candidato_bd %in%
          cand_data$id_candidato_bd[cand_data$ano == 2016 & cand_data$cargo %in% c("prefeito", "vice-prefeito")],
     TRUE, FALSE
     )]
     # create dynastic mayor dummy
     cand_subset$dynastic_mayor <- ifelse(cand_subset$id_candidato_bd %in%
          name_matches$child_id_candidato_bd[name_matches$parent_mayor_cand == TRUE],
     TRUE, FALSE
     )
     ## create dynastic mayor and elected dummy
     cand_subset$dynastic_elected_mayor <- ifelse(cand_subset$id_candidato_bd %in%
          name_matches$child_id_candidato_bd[name_matches$parent_elected_mayor == TRUE],
     TRUE, FALSE
     )
     cand_subset$dynastic_mayor16 <- ifelse(cand_subset$id_candidato_bd %in%
          name_matches$child_id_candidato_bd[name_matches$parent_ran_mayor_2016 == TRUE],
     TRUE, FALSE
     )

     ## create results data
     vote_totals20 <- elec_results[ano == 2020 & turno == 1 & tipo_eleicao == "eleicao ordinaria" & cargo == "prefeito",
          .(total_votes = sum(votos)),
          by = c("id_municipio_tse")
     ]
     elec_prefeito_results20 <- merge(
          elec_results[
               ano == 2020 & turno == 1 & tipo_eleicao == "eleicao ordinaria" &
                    cargo == "prefeito",
               .(id_candidato_bd, id_municipio_tse, resultado, votos)
          ],
          vote_totals20,
          all.x = TRUE, all.y = FALSE
     )

     cand_subset <- merge(cand_subset, elec_prefeito_results20,
          all.x = TRUE, all.y = FALSE
     )
     cand_subset[, elected := ifelse(resultado != "nao eleito", 1, 0)]
     cand_subset[, vote_pct := 100 * votos / total_votes]
     cand_subset
}

gen_vp_codes <- function(cand_data, elec_results) {
     pref_cands <- elec_results[
          cargo == "prefeito" & !is.na(id_candidato_bd),
          .(ano, id_municipio_tse,
               pref_id_candidato_bd = id_candidato_bd,
               numero = numero_candidato
          )
     ]
     vp_cands <- cand_data[
          cargo == "vice-prefeito",
          .(ano, id_municipio_tse, vp_id_candidato_bd = id_candidato_bd, numero)
     ]
     vp_cands <- merge(vp_cands, pref_cands, all.x = FALSE, all.y = FALSE)
     vp_cands[, numero := NULL]
     vp_cands <- merge(vp_cands, elec_results[, c(
          "ano", "id_municipio_tse", "id_candidato_bd",
          "resultado", "votos"
     )],
     by.x = c("ano", "id_municipio_tse", "pref_id_candidato_bd"),
     by.y = c("ano", "id_municipio_tse", "id_candidato_bd"),
     all.x = TRUE, all.y = FALSE
     )
     vp_cands
}
