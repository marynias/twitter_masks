# Code
**mask_project.Rmd** - Rmarkdown notebook containing all the analyses other than topic modelling with Corex and Top2Vec method 
which are contained in the appropriate notebooks.

**mask_project_funs.R** - utility functions used in the mask_project notebook

**ggplot_theme_Publication-2.R** - code for ggplot2 theme used for plotting figures

**CorEx_analysis.ipynb** - IPython notebook with code for carrying out [CorEx](https://github.com/gregversteeg/CorEx) topic modelling

**Top2Vec_analysis.ipynb** - IPython notebook with code for carrying out [Top2Vec](https://github.com/ddangelov/Top2Vec) topic modelling 

# Analysis
## mask_project_short.html
HTML rendering of the first part of the mask_project.Rmd notebook ran on the final tweet dataset. 
I could not render the whole notebook due to untractable error (rendering works fine on a smaller dataset including a subset of tweets). 
Nevertheless, all the figures and tables referenced in the notebook are in the *pdf*, *csv* and *topic_modelling* folders referenced below.
## Folder: csv
Table (csv) outputs from the mask_project notebook *except for* topic modelling 
## Folder: pdf
Figure (pdf) outputs from the mask_project notebook *except for* topic modelling 
## Folder: topic_modelling
### Folder: Corex
Folder contains results for this LDA-based method which allows anchoring individual topics with keywords. 
We anchored our topics based on previous research on topics of interest 
as well as results from Top2Vec (looking at words co-occuring with previously identified keywords). Each run takes 5-10 hours. 
When number of topics was set at low (2-20), we did not get good seperation of tweets towards our topics of interest, so presenting results for k=200.

**CorEx_analysis_new_lemma_k200_mask.html** - Executed Corex notebook with k = 200, and inclusion of the word "mask" for topic 3

**CorEx_analysis - Jupyter Notebook.html** - Executed Corex notebook with k = 200, and elimination of the word "mask" for topic 3

**Corex_examples.xlsx** - Random selection of 20 lemmatised tweets assigned by Corex to each of the 4 topics and their manual assessment (tweets matching topic according to curation are highlighted in bold)

**topicX_tweets_corex_partial_new_lemma_k200_mask.csv** - All the lemmatised tweets matching a given topic in the CorEx output (k = 200, and inclusion of the word "mask" for topic 3)

**topicX_tweets_corex_partial_new_lemma_k200_nomask.csv** - All the lemmatised tweets matching a given topic in the CorEx output (k = 200, and elimination of the word "mask" for topic 3)

**pdf** - folder containing figure output from the mask_project notebook for the 4 topics identified in the CorEx run (k = 200, and elimination of the word "mask" for topic 3)

**csv** - folder containing tabular output from the mask_project notebook for the 4 topics identified in the CorEx run (k = 200, and elimination of the word "mask" for topic 3)

### Folder: keyword_search
Folder containing tabular and figure output from the mask_project notebook for manual topic modelling using keywords derived from previous knowledge and Top2Vec output (looking at words co-occuring with already identified keywords).

**topic_grep_all.xlsx** - A spreadsheet showing results of manual querying of the original (not filtered) tweet dataset for keywords in each of the 4 topics and showing number of tweets with matches for keywords.
### Folder: LDA
Folder containing tabular (*csv*) and figure (*pdf*) output from the mask_project notebook for LDA-based topic modelling. Number of topics tested: 1-20, 50, 100, 150, 200.
### Folder: Top2Vec
**Top2Vec_analysis_latest.html** - Executed Top2Vec notebook with overview of results for 6,000 topics identified.
**topc2vec_model_13_05.txt** - Saved Top2Vec model ran on our tweets which can be reloaded and evaluated.