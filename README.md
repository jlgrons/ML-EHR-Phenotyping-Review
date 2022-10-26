# Machine learning approaches for electronic health records phenotyping: A methodical review

R code for replication of the analyses in our systematic review of the machine learning based phenotyping literature.  

## Aricle Information

Machine learning approaches for electronic health records phenotyping: A methodical review

Siyue Yang, Paul Varghese, Ellen Stephenson, Karen Tu, Jessica Gronsbell

medRxiv 2022.04.23.22274218

doi: https://doi.org/10.1101/2022.04.23.22274218

## Review Data

Basic information on all articles identified from our search of PubMed and Web of Science as well as detailed recording of 37 variables on 100 articles selected for the review.

- [Article Review Data](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/article_charting_101722.csv)

## Analysis Results

Results from the key analysis steps of our review.

- [Merging](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Reports/1-merging.pdf): Results of merging articles from PubMed and Web of Science. 

- [Exclusion](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Reports/2-exclusion.pdf): Results from excluding articles not meeting the selection criteria.

- [Analysis](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Reports/3-analysis.pdf): Results summarizing characteristics of the 100 articles selected for our review.

## Analysis Code + Data

R code and corresponding data for the key analysis steps of our review.

We performed all analyses using R version 4.1.0 and used a variety of packages for data processing, graphing, and reporting.

The code for the key steps of our analysis are provided in:

- [1-merging.Rmd](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/R%20code/1-merging.Rmd): Merges search results from PubMed and Web of Science. Data files used for merging include:
  - [PubMed data](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/pubmed20220414.csv): All articles extracted from PubMed.  
  - [Web of Science data](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/webofscience20220414.csv): All articles extracted from Web of Science.
  - [AMIA Publication Date data](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/amia20220414.csv): Correct publication dates for AMIA articles.  Used to resolve a publication date conflict observed in PubMed.

- [2-exclusion.Rmd](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/R%20code/2-exclusion.Rmd): Excludes articles not meeting our selection criteria. Data files used for exclusion include:
  - [Annotated article data](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/article_charting_101722.csv): Detailed recording of 37 variables on 100 articles selected for the review.

- [3-analysis.Rmd](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/R%20code/3-analysis.Rmd): Analysis of the selected articles.  Helper functions are in [analysis_functions.R](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/R%20code/analysis_functions.R).  Data files for analysis include:
  - [Annotated article data with citations](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/article_charting_101722_with_citations.csv): Detailed recording of 37 variables on 100 articles selected for the review with citations from the manuscript recorded in [citations.csv](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/Data/citations.csv) and appended by [add_citations.R](https://github.com/jlgrons/ML-EHR-Phenotyping-Review/blob/main/R%20code/add_citations.R).  
