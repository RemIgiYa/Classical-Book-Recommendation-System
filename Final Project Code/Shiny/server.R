library(shiny)
library(dplyr)
library(data.table)
library(gutenbergr)
library(tidytext)
library(topicmodels)
library(tidyr)


shinyServer(function(input, output) {
  
  f1 <- read.csv("fictionalBooks.csv")
  
    gBooks<-read.csv("books.csv")
  names(gBooks)[1] <- "title"
  
  GldaTopics <- read.csv("LDAtopics.csv")
  
  GldaTopics <- subset(GldaTopics, select = c(title, LDAtopics))

  gBooks <- merge(x=gBooks, y=GldaTopics, by = "title", all.y=TRUE)

  TopWbTopic <- read.csv("SummaryTopTermsbyTopic.csv")
  
  TopWbTopic<- subset(TopWbTopic, select = c(topic, term))

  TopWbTopic<- as.data.table(TopWbTopic)[, toString(term), by = list(topic)]

  names(TopWbTopic)[1] <- "LDAtopics"

  gBooks2 <- merge(x=TopWbTopic, y=gBooks, by = "LDAtopics", all.y=TRUE)
  
  
  
  
  
  SampleUser <- reactive({
    SampleUser <- sample_n(gBooks2, input$obs)
  })
  
  
  sample <- reactive({

    fictionalBooks<-f1
    
    sample <- sample_n(f1, input$clB)

  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- sample()
    summary(dataset)
  })
  
  output$User <- renderTable({
    user<- SampleUser()
    user <- subset(user, select=c(title, Author))
    head(user, n = input$obs)
  })
  
  
  
  output$View <- renderTable({
    
    sample<- sample()
    
    bookText = data.frame()

    for (i in 1:dim(sample)[1]){
      bookText <- rbind(bookText, gutenberg_download(sample$gutenberg_id[i], meta_fields = "title", mirror = "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/"))
    }

    sampleUserBookData <- subset(SampleUser(), select = c(V1, title, Summary))

    sampleUserBookData <- transform(sampleUserBookData, text=paste(Summary, V1, sep=" "))
    
    sampleUserBookData <- subset(sampleUserBookData, select = c(title, text))

    bookText<- subset(bookText, select = c(text, title))

    FbookText <- rbind(bookText, sampleUserBookData)

    names <- read.csv("names.csv")
    
    names$name <- tolower(names$name)

    by_chapter <- FbookText %>%
      group_by(title) %>%
      ungroup()

    by_chapter_word <- by_chapter %>%
      unnest_tokens(word, text)

    word_counts <- by_chapter_word %>%
      filter(!word %in% c(stop_words$word, names$name)) %>%
      count(title, word, sort = TRUE) %>%
      ungroup()

    word_counts = word_counts %>% bind_tf_idf(word, title, n) %>%
      arrange(desc(tf_idf))

    avg = mean(word_counts$tf_idf)
    
    word_counts = subset(word_counts, tf_idf>avg)

    chapters_dtm <- word_counts %>%
      cast_dtm(title, word, n)
    
    m2 <- as.matrix(chapters_dtm)

    chapters_lda <- LDA(chapters_dtm, k = input$tNo, control = list(seed = 20))
    
    chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
    
    chapters_gamma <- chapters_gamma %>%
      separate(document, c("title"), sep = "_", convert = TRUE)
    
    chapter_classifications <- chapters_gamma %>%
      group_by(title) %>%
      slice_max(gamma) %>%
      ungroup()
    
    book_topics <- chapter_classifications %>%
      count(title, topic) %>%
      group_by(title) %>%
      top_n(1, n) %>%
      ungroup() %>%
      transmute(consensus = title, topic)
    
    
    names(book_topics)[1] <- "title"
    names(book_topics)[2] <- "LDAtopics"

    FavBtopics <- merge(x=book_topics, y=sampleUserBookData, by = "title", all.y=TRUE)

    Topic <- FavBtopics$LDAtopics

    SimilarBooksByTopic <- data.frame()
    
    j=1
    TopicNum <- length(Topic)
    for (j in 1:TopicNum){
      sample <- filter(book_topics, LDAtopics %in% c(Topic[j]))
      
      SimilarBooksByTopic<- rbind(SimilarBooksByTopic, sample)
      
      SimilarBooksByTopic <- unique(SimilarBooksByTopic)
    }

    word_counts2 <- merge(x=word_counts, y=SimilarBooksByTopic, by = "title", all.y=TRUE)

    similarB_dtm <- word_counts2 %>%
      cast_dtm(title, word, n)
    
    m2Sim <- as.matrix(similarB_dtm)

    sim <- m2Sim / sqrt(rowSums(m2Sim * m2Sim))
    sim <- sim %*% t(sim)

    LikedBooks <- sampleUserBookData$title

    numB <- length(LikedBooks)

    recommendations <- data.frame()
    
    k=1
    for (k in 1:numB){
      simBooksSample <- subset(sim, select=c(LikedBooks[k]))
      
      simBooksS22 <- simBooksSample[order(-simBooksSample[,1]),]
      
      simBooksS22 <- as.data.frame(simBooksS22)
      
      setDT(simBooksS22, keep.rownames = "titles")[]
      
      simBooksS22 <-filter(simBooksS22, !titles %in% c(sampleUserBookData$title))
      
      RecBooks <- simBooksS22 %>%
        slice(1:5)
      
      recommendations <- rbind(recommendations, RecBooks)
      
    }

    toRec <- recommendations[order(-recommendations[,2]),]

    toRec <- toRec %>%
      slice(1:5)
    
    fRec<- subset(toRec, select = c(titles, simBooksS22))
    
    names(fRec)[2] <- "Similarity"
    
    head(fRec, n = 5)
    
    
  })

  
})