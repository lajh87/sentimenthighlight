    library(sentimentr)
    library(magrittr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    set.seed(2)

    hu_liu_cannon_reviews %>%
      filter(review_id %in% sample(unique(review_id), 3)) %>%
      mutate(review = get_sentences(text)) %$%
      sentiment_by(review, review_id) %>%
      highlight(open = FALSE)

    ## Saved in C:\Users\lukeh\AppData\Local\Temp\RtmpkPjljk/polarity.html
