library(bibliometrix)
library(tidyverse)
library(wordcloud)
library(tm)

df <- convert2df("raw/savedrecs (2).bib", dbsource = "isi", format = "bibtex")



# 2. biblioAnalysis and summary functions ---------------------------------


results <- biblioAnalysis(df, sep = ";")

results_summary <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

CR <- citations(df, field = "author", sep = ";")

cbind(CR$Cited[1:10])


# 3. Network plot and Author Production over Time ----------------------------


topAU <- authorProdOverTime(df, k = 10, graph = TRUE)

NetMatrix <- biblioNetwork(df, analysis = "co-occurrences", network = "keywords", sep = ";")

net = networkPlot(NetMatrix, normalize = "association", weighted = T, n = 15, Title = "Keyword Co-occurrences", type = "fruchterman", size = T, edgesize = 5, labelsize = 0.7)

pub_count <- df |> 
  filter(PY < 2023) |> 
  group_by(PY) |> 
  tally()

ggplot(pub_count, aes(x = PY, y = n)) +
  geom_line(size = 1.05, color = "darkgreen")+
  xlab("Year") +
  ylab("Number of Publications") +
  theme_classic()

pub_count$search <- "Bacteria"

df2 <- convert2df("raw/savedrecs (4).bib", dbsource = "isi", format = "bibtex")

pub_count2 <- df2 |> 
  filter(PY < 2023) |> 
  group_by(PY) |> 
  tally()

pub_count2$search <- "Fungi"

pub_count_combo <- rbind(pub_count, pub_count2)



# 4. Publications through time --------------------------------------------



ggplot(pub_count_combo, aes(x = PY, y = n, color = search)) +
  geom_line(size = 1.05) +
  scale_color_manual("Taxon \nSearch Term", values = c("darkgreen", "darkblue")) +
  xlab("Year") +
  ylab("Number of Publications") +
  theme_classic()


pub_count_combo_sum <- pub_count_combo |> 
  group_by(search) |> 
  mutate(csum = cumsum(n))

ggplot(pub_count_combo_sum, aes(x = PY, y = csum, color = search)) +
  geom_line(size = 1.05) +
  scale_color_manual("Taxon \nSearch Term",
                     values = c("darkgreen","darkblue")) +
xlab("Year") +
  ylab("Cumulative Number of Publications") +
  theme_classic()


# Word Cloud --------------------------------------------------------------

words <- as.character(df$AB)
docs <- Corpus(VectorSource(words))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, content_transformer
               (tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)

matrix <- as.matrix(dtm)

words <- sort(rowSums(matrix), decreasing = TRUE)

df_words <- data.frame(word = names(words), 
                       freq = words)

wordcloud(words = df_words$word, freq = 
            df_words$freq, min.freq = 32, max.words = 150, rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))



