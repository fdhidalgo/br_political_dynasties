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

# name extraction code ----------------------------------------------------

extract_filho_pattern <- function(sentences) {
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
     extracted_names <- extracted_names[parent_name != ""]
     extracted_names[, unnormalized_parent_name := parent_name]
     extracted_names[, parent_name := str_squish(tolower(parent_name))]
     extracted_names[, parent_name := str_remove_all(parent_name, "[:punct:]|\\||[:digit:]")]
     extracted_names[, parent_name := stringi::stri_trans_general(parent_name, "latin-ascii")]
     extracted_names[, parent_name := str_squish(str_remove_all(parent_name, "\\bde\\b|\\bda\\b|\\bdos\\b|\\bdas\\b"))]

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