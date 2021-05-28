loadFile <- function(x){
  df <- read.csv(x, header=T, stringsAsFactors=F,row.names=NULL,encoding='UTF-8')
  df
}


removeMispelledContract <- function(x) {
  x$text <- stringr::str_replace_all(x$text, regex('\\bdoesnt\\b', ignore_case = T), "does not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bdont\\b', ignore_case = T), "do not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bdidnt\\b', ignore_case = T), "did not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bwasnt\\b', ignore_case = T), "was not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bwerent\\b', ignore_case = T), "were not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bwont\\b', ignore_case = T), "will not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bwouldnt\\b', ignore_case = T), "would not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bim\\b', ignore_case = T), "I am")
  x$text <- stringr::str_replace_all(x$text, regex('\\bive\\b', ignore_case = T), "I have")
  x$text <- stringr::str_replace_all(x$text, regex('\\barent\\b', ignore_case = T), "are not")
  x$text <- stringr::str_replace_all(x$text, regex('\\bshes\\b', ignore_case = T), "she is")
  x$text <- stringr::str_replace_all(x$text, regex('\\bhes\\b', ignore_case = T), "he is")
  x$text <- stringr::str_replace_all(x$text, regex('\\bisnt\\b', ignore_case = T), "is not")
  x$text <- stringr::str_replace_all(x$text, regex('\\byoure\\b', ignore_case = T), "you are")
  x$text <- stringr::str_replace_all(x$text, regex('\\btheyre\\b', ignore_case = T), "they are")
  x$text <- stringr::str_replace_all(x$text, regex('\\bppl\\b', ignore_case = T), "people")
  x
}

countWords <- function(x) {
    x  <- x %>% 
    dplyr::count(word, sort = TRUE) %>%
    dplyr::mutate(word = reorder(word, n)) 
    x
}

plotBarchart <- function(x) {
  barchart_top30 <- x %>% # gives you a bar chart of the most frequent words found in the tweets
    dplyr::top_n(30) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = "Most frequent words found in the tweets about masks",
         subtitle = "Stop words removed from the list") +
    scale_y_continuous(labels=comma) +
    theme(plot.title = element_text(hjust = 0)) + 
    theme_Publication()
  barchart_top30
}

plotHistogram <- function(x, z) {
  histogram <- x %>% # gives you a histogram of the values, displayed in log10 scale
    ggplot(aes_string(x = z)) +
    geom_histogram(fill="seagreen3", col="black") +
    scale_y_continuous(labels=comma) +
    scale_x_continuous(trans = 'log10') +
    labs(x = "count (log10)", y = "frequency") +
    #theme_fira()
    theme_Publication()
  histogram
}

plotWordCloud <- function(x, prefix, suffix) {
  set.seed(1234)
  output_file <- paste(prefix, "_", suffix, ".pdf", sep="")
  pdf(output_file, 
      width = 5, 
      height = 4)
  wordcloud(x, min.freq=50, scale=c(3, .4), random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}

plotTimeLineChart <- function(x) {
    ggplot(x) + 
    geom_line(aes(x=created, y=value, colour=variable), size=1) +
    scale_colour_manual(labels = c('n' = 'tweets', 'unique_users' = 'users'),
    values = c('n' = 'turquoise', 'unique_users' = 'tan1')) +
    xlab('') +
    ylab('number') + 
    scale_y_continuous(labels=scales::comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
    theme_Publication() +
    theme(axis.text.x = element_text(
      colour = 'black', angle = 90, size = 13,
      hjust = 0.5, vjust = 0.5), legend.title=element_blank()) 
  
}

plotEmotionBars <- function (x, myColors) {
  emotion_scores_barchart <- ggplot(data=x,aes(x=emotion_type,y=ave_emotion))+
  geom_bar(aes(fill=emotion_type),stat = "identity")+
  theme(legend.position="none")+
  xlab("Emotions")+ylab("Total score")+
  scale_y_continuous(labels=scales::comma)+
  ggtitle("Total tweet scores per emotion")+
  theme_minimal() +
  scale_fill_manual(values = myColors) + 
  labs(fill = "Emotion type") +
  theme_Publication()
}

plotEmotionAreaChart <- function (x, my_tweets, my_cols) {
  ggplot(x) + 
    geom_area(aes(x=created, y=value, fill=emotion_type)) +
    xlab('') +
    labs(fill = "Emotion type") +
    ggtitle("Tweets expressing given emotions through time") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
    scale_y_continuous(name = "number of tweets", labels=scales::comma, 
                       sec.axis = sec_axis( trans=~./dim(my_tweets)[1]*100, name="% all tweets")) + 
    theme_Publication() +
    theme(axis.text.x = element_text(
      colour = 'black', angle = 90, size = 13,
      hjust = 0.5, vjust = 0.5)) +
    scale_fill_manual(values = my_cols) 
}

plotNetworkGraph <- function(x, title, legg) {
  a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
  ggraph(x, layout = "fr") +
    geom_edge_link(aes_string(edge_alpha = legg), show.legend = TRUE,
                   arrow = a, end_cap = circle(.07, 'inches'), edge_colour="red") +
    geom_node_point(color = "cyan4", size = 3) +
    geom_node_label(aes(label = name), vjust = 1, hjust = 1, repel = T) +
    ggtitle(title) +
    coord_cartesian(clip = "off") + 
    theme_void()
  
}

plotAssociatedWords <- function(x, word_vec) {
    x %>%
    filter(item1 %in% word_vec) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    facet_wrap(~ item1, scales = "free") +
    labs(x = NULL) + 
    coord_flip() + 
    theme_Publication()  
  
}

retrieveOriginalTweets <- function(terms) {
  to_match <- unlist(lapply(terms, function(x) gsub("\\s", ".+", x)))
  matching_rows <- unique (grep(paste(to_match,collapse="|"), 
                                my_tweets_processed$text, value=FALSE, ignore.case=TRUE))
  untouched_tweets <- my_tweets[matching_rows,]					
}
  
getBigramDF <- function(x) {
  x %>%
    dplyr::select(text) %>% unique() %>%
    tidytext::unnest_tokens(word, text, token = "ngrams", n = 2)  %>%
    tidyr::separate(word, c("word1", "word2"), sep = " ") %>%
    #eliminate if any word is a stop word
    dplyr::filter(!word1 %in% stop_words$word,
                  !word2 %in% stop_words$word) %>%
    #eliminate if any word is a number for cleanup
    dplyr::filter(!stringr::str_detect(word1, "\\d+"),
                  !stringr::str_detect(word2, "\\d+")) %>%
    #Remove NAs
    dplyr::filter(word1 != "NA", word2 != "NA") %>%
    tidyr::unite("word", c("word1", "word2"), sep= " ", remove = FALSE) %>%
    count(word, sort = TRUE) %>%
    dplyr::mutate(word = reorder(word, n))
}

getTrigramDF <- function(x) {
  x %>%
    dplyr::select(text) %>% unique() %>%
    tidytext::unnest_tokens(word, text, token = "ngrams", n = 3)  %>%
    tidyr::separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    dplyr::filter(!word1 %in% stop_words$word,
                  !word2 %in% stop_words$word,
                  !word3 %in% stop_words$word) %>%
    #eliminate if any word is a number for cleanup
    dplyr::filter(!stringr::str_detect(word1, "\\d+"),
                  !stringr::str_detect(word2, "\\d+"),
                  !stringr::str_detect(word2, "\\d+")) %>%
    #Remove NAs
    dplyr::filter(word1 != "NA", word2 != "NA", word3 != "NA") %>%
    tidyr::unite("word", c("word1", "word2", "word3"), sep= " ", remove = FALSE) %>%
    count(word, sort = TRUE) %>%
    dplyr::mutate(word = reorder(word, n)) 
}

getBigramNetwork <- function(x) {
  bigram_graph <- as_tibble(x) %>%
    dplyr::filter(n > 3000)
  bigram_graph <- bigram_graph %>%                 
    tidyr::separate(word, c("word1", "word2"), sep = " ")
  bigram_graph <- bigram_graph %>%
    igraph::graph_from_data_frame()
  
}

getTrigramNetwork <- function(x) {
  trigram_graph <- as_tibble(x) %>%
    dplyr::filter(n > 1000)
  trigram_graph <- trigram_graph %>%                 
    tidyr::separate(word, c("word1", "word2", "word3"), sep = " ")
  trigram_graph <- trigram_graph %>%
    igraph::graph_from_data_frame()
}

plotTopicTerms <- function(x, topic_colors, my_k) {
  x %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    group_by(topic, term) %>%    
    arrange(desc(beta)) %>%  
    ungroup() %>%
    ggplot(aes(beta, term, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    scale_y_reordered() +
    labs(title = "Top 10 terms in each LDA topic",
         x = expression(beta), y = NULL) +
    facet_wrap(~ topic, ncol = 3, nrow = ceiling(my_k/3), scales = "free") +
    scale_fill_manual(values = topic_colors) +
    theme_Publication()
}
plotTopicProb <- function(x, topic_colors, my_k) {
ggplot(x, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3, nrow = ceiling(my_k/3), scales = "free") +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma)) +
  scale_fill_manual(values = topic_colors) +
  theme_Publication() 
}

plotTopicTimeLine <- function(x, topic_colors, y_label) {
  ggplot(x, aes(x=created, y=value, group=topic, color=topic)) + 
    geom_line(size=0.6) +
    xlab('') +
    ylab(y_label) + 
    scale_y_continuous(labels=comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
    theme_Publication() +
    theme(axis.text.x = element_text(
      colour = 'black', angle = 90, size = 13,
      hjust = 0.5, vjust = 0.5)) +
    scale_colour_manual(values = topic_colors)
  
}

plotEmotionTopWords <- function (x, my_cols, main_f, n_col, n_row, freq, x_title) {
  x %>%   
    group_by(get(main_f)) %>%
    slice_max(get(freq), n = 15) %>% 
    ungroup() %>%
    mutate(word = reorder(word, get(freq))) %>%
    ggplot(aes(get(freq), word, fill = as.factor(get(main_f)))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~get(main_f), scales = "free_y",ncol = n_col, nrow = n_row) +
    labs(x = x_title,
         y = NULL) + 
    scale_fill_manual(values = my_cols) +
    theme_Publication() +
    theme(plot.title = element_text(size=12)) + 
    scale_x_continuous(labels = scales::comma)
}

