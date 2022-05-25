# Families of Brazilian Politicians

This code extracts the names of parents of Brazilian politicians from official documents filed with the Supreme Electoral Tribunal ( *Tribunal Superior Eleitoral* or TSE) of Brazil. Currently, the data has been collected and cleaned for candidates for mayor in the 2020 elections. The data can be found [here](./output/politician_parents.csv).

The legal documents in PDF format associated with the candidacies were scraped from the TSE website: [https://divulgacandcontas.tse.jus.br](https://divulgacandcontas.tse.jus.br). After extracting the from the PDF files, we use regular expressions to extract the names of the parents of politicians.  In a minority of the cases,  the gender of the parents is also reported. 

Most politicians are associated with 1 or 2 parents, but in some cases, the data report more than two parents. Politicians are associated with more than 2 parents when the extraction code produces more than 2 names and we cannot determine the correct names without manual inspection of the original documents. This data has not been manually checked so some names will be incorrect or incomplete. 

Some descriptive statistics can be found [here](./output/descriptive_statistics.md).

The structure of the data is as follows: 

- `cand_id`: Unique candidate identifier. This is the `SQ_CANDIDATO` field in the results reported by the TSE. 
- `tse_munid`: TSE municipality identifier. 
- `parent_name`: Name of the parent. This name has been normalized by removing diacritical marks, converting to lower case,  and removing prepositions (e.g. "de").
- `n_mentions`: Number of times the parent name was mentioned in the documents.
- `n_parents`: Number of parents associated with the candidate. 
- `gender`: Gender of the parent, if available. 
- `unnormalized_parent_name`: Parent name without normalization. 

## Running the Code
The data cleaning code is written in [`R`](https://www.r-project.org) and uses the [`targets`](https://docs.ropensci.org/targets/) package to manage the pipeline. `R` version  4.2.0 or greater is required. The pipeline can be found in [\_targets.R](\_targets.R). To run the code run [run.R](run.R) in the root directory of the repository.

The text files extracted from the the legal documents are not in the github repository due to the size of the files. Contact me for access.
