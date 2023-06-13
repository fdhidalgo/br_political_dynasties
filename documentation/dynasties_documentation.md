# Methodology
F. Daniel Hidalgo

## Original Documents

We download all candidate documents from the Supreme Electoral
Tribunal’s electoral candidacy
[site](https://divulgacandcontas.tse.jus.br/divulga/#/). Almost all
documents are in PDF format, but we convert a small number of documents
in other image formats to PDF. Most PDF documents have embedded text,
but a significant proportion do not. For PDFs without embedded text, we
use the Tesseract OCR
[engine](https://github.com/tesseract-ocr/tesseract) via the `tesseract`
R package (Ooms 2022) to retrieve the text.

## Text Processing

Through manual inspection we identified common patterns used in the
candidacy legal documents to report parent names. Specifically, the
three common patterns we identified were:

- “filho de” or “filha de”
- “filiação de”
- “nome da mãe” or “nome do pai”

We use regular expressions to extract the names from the text following
those phrases. Once we have extracted all potential parent names from
the documents associated with each candidate, we use a clustering
algorithm to combine very similar names. This clustering procedure helps
account for minor spelling or typographical errors.

For a small number of candidates, the above procedures results in more
than 2 unique names. We tabulate the number of times each name is
mentioned and keep the 2 most frequently mentioned names. If there are
ties among the 2 most frequently mentioned names, then we keep all the
ties. As a result of this, a small number of candidates have more than 2
parents.

When the gender of the parent is clear in the documents due to being
explicitly labelled as the mother or father, we also record gender.

### Missing Data

<div id="okblehevqr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#okblehevqr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#okblehevqr thead, #okblehevqr tbody, #okblehevqr tfoot, #okblehevqr tr, #okblehevqr td, #okblehevqr th {
  border-style: none;
}
&#10;#okblehevqr p {
  margin: 0;
  padding: 0;
}
&#10;#okblehevqr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#okblehevqr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#okblehevqr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#okblehevqr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#okblehevqr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#okblehevqr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#okblehevqr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#okblehevqr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#okblehevqr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#okblehevqr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#okblehevqr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#okblehevqr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#okblehevqr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#okblehevqr .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#okblehevqr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#okblehevqr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#okblehevqr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#okblehevqr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#okblehevqr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#okblehevqr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#okblehevqr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#okblehevqr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#okblehevqr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#okblehevqr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#okblehevqr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#okblehevqr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#okblehevqr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#okblehevqr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#okblehevqr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#okblehevqr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#okblehevqr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#okblehevqr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#okblehevqr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#okblehevqr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#okblehevqr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#okblehevqr .gt_left {
  text-align: left;
}
&#10;#okblehevqr .gt_center {
  text-align: center;
}
&#10;#okblehevqr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#okblehevqr .gt_font_normal {
  font-weight: normal;
}
&#10;#okblehevqr .gt_font_bold {
  font-weight: bold;
}
&#10;#okblehevqr .gt_font_italic {
  font-style: italic;
}
&#10;#okblehevqr .gt_super {
  font-size: 65%;
}
&#10;#okblehevqr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#okblehevqr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#okblehevqr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#okblehevqr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#okblehevqr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#okblehevqr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#okblehevqr .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Office">Office</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="No Parents">No Parents</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="1 Parent">1 Parent</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="2 Parents">2 Parents</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="&amp;gt; 2 Parents">&gt; 2 Parents</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="cargo" class="gt_row gt_left">Councilor</td>
<td headers="n" class="gt_row gt_right">57,459</td>
<td headers="parents0" class="gt_row gt_right">24%</td>
<td headers="parents1" class="gt_row gt_right">16%</td>
<td headers="parents2" class="gt_row gt_right">58%</td>
<td headers="parents3" class="gt_row gt_right">3%</td></tr>
    <tr><td headers="cargo" class="gt_row gt_left">Mayor</td>
<td headers="n" class="gt_row gt_right">17,925</td>
<td headers="parents0" class="gt_row gt_right">21%</td>
<td headers="parents1" class="gt_row gt_right">15%</td>
<td headers="parents2" class="gt_row gt_right">61%</td>
<td headers="parents3" class="gt_row gt_right">3%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

The candidate documents do not always include parent names. In
**?@tbl-missing-data**, we show the percentage of candidates with the
number of parents found by office. Overall, we found at least 1 parent
for 77% of candidates and at least 2 parents for 61.7% of candidates.

<img
src="dynasties_documentation_files/figure-commonmark/fig-missing-data-state-1.png"
id="fig-missing-data-state"
alt="Figure 1: Percentage of candidates with no parent data by state." />

There is considerable variation in the availability of data by state. In
[Figure 1](#fig-missing-data-state), we show the proportion of
candidates with no parent data by state.

## Merging Parents with Candidate Data

To identify parents with previous political experience, we merge the
parent names with candidate data provided by the TSE and harmonized by
the organization “[Base dos Dados](https://basedosdados.org)”. This data
encompasses all candidates to local office since the 2000 elections for
a total of 1,758,738 candidates.

To match parent names with candidate data, we first condition on the
municipality and then use supervised learning method proposed by Kaufman
and Klevs (2022). This method uses a combination of string distance
metrics and machine learning to match names. We compute a variety of
string distance metrics between the parent names and potential
matches[^1] and other features of the names, such as string length. In
addition, we compute the difference in ages between child of the parent,
i.e. the 2020 election candidate, and the potential match from prior
elections. The difference in ages is useful because child and parents
with very similar or very different ages are unlikely to be a match.
Finally, we also use the Meireles (2021) package to compute the
difference in the probability of each name being a a female name.

To provide the needed to train the model, we assess a sample of
potential matches to determine whether they are a match or not. Random
sampling would be inefficient because the vast majority of potential
matches are not matches. Instead, we use an adaptive approach where we
initially sample potential matches that are more likely to be a match.
We then use the results of the initial sample to train a model. Afer
this initial training, we use the model to select additional training
samples, which are then manually assessed[^2]. We repeat this process
until our false positive error, as estimated using cross-validation, is
acceptably low. The final size of the training set is 492 observations.

To estimate predicted probabilities of a match, we use a random forest
model[^3] to predict matches as a function of string distance, string
characteristics, and age differences. Our cross validated accuracy rate
is 0.95, while precision and recall are 0.96 and 0.94, respectively.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-kaufman_klevs_2022" class="csl-entry">

Kaufman, Aaron R., and Aja Klevs. 2022. “Adaptive Fuzzy String Matching:
How to Merge Datasets with Only One (Messy) Identifying Field.”
*Political Analysis* 30 (4): 590–96.
<https://doi.org/10.1017/pan.2021.38>.

</div>

<div id="ref-tidymodels" class="csl-entry">

Kuhn, Max, and Hadley Wickham. 2020. *Tidymodels: A Collection of
Packages for Modeling and Machine Learning Using Tidyverse Principles.*
<https://www.tidymodels.org>.

</div>

<div id="ref-RJ-2014-011" class="csl-entry">

Loo, Mark P. J. van der. 2014. “<span class="nocase">The stringdist
Package for Approximate String Matching</span>.” *The R Journal* 6 (1):
111–22. <https://doi.org/10.32614/RJ-2014-011>.

</div>

<div id="ref-genderbr" class="csl-entry">

Meireles, Fernando. 2021. *genderBR: Predict Gender from Brazilian First
Names*. <https://CRAN.R-project.org/package=genderBR>.

</div>

<div id="ref-tesseract" class="csl-entry">

Ooms, Jeroen. 2022. *Tesseract: Open Source OCR Engine*.
<https://CRAN.R-project.org/package=tesseract>.

</div>

<div id="ref-JSSv077i01" class="csl-entry">

Wright, Marvin N., and Andreas Ziegler. 2017. “Ranger: A Fast
Implementation of Random Forests for High Dimensional Data in c++ and
r.” *Journal of Statistical Software* 77 (1): 1–17.
<https://doi.org/10.18637/jss.v077.i01>.

</div>

</div>

[^1]: We use the following string distance metrics using the
    `stringdist` R package (Loo 2014): the proportion of shared words,
    Restricted Damerau-Levenshtein distance, Levenshtein distance,
    Longest Common Substring distance, Q-gram distance, Jaro similarity,
    and Jaro-Winkler similarity.

[^2]: The specifics of the algorithm are in Kaufman and Klevs (2022).
    Briefly, we apply the trained model to the unlabelled potential
    matches and select an additional sample of potential matches. We
    select the potential matches with predicted probabilities closest to
    0.5, i.e. the potential matches which the model is most uncertain
    about. We then manually assess the potential matches, combine the
    new data with the already-labelled data, and use the results to
    train a new model.

[^3]: We use the `tidymodels` (Kuhn and Wickham 2020) framework to train
    the model and use the `ranger` (Wright and Ziegler 2017) package
    implementation of random forests. The `mtry` and `min_n`
    hyperparaters are tuned via cross-validation. The number of trees is
    set to 1000.
