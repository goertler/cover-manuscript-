##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 5/08/2026
# Description: draft wordclouds for author discussion
##########################################################

# library
#install.packages("wordcloud2")
library(tm)
library(wordcloud)
library(RColorBrewer)

# data
data <- read.csv("results_words.csv")
docs <- Corpus(VectorSource(data$words))
inspect(docs)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#plot
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# custom colors plot
data <- read.csv("wordcloud.csv")

# need to make data frame with word frequency and custom colors by 'conclusion' variable
unique(data$conclusion)
pos <- subset(data, conclusion == "positive")
neg <- subset(data, conclusion == "negative")
temp <- subset(data, conclusion == "temperature")
hab <- subset(data, conclusion == "habitat extent")
bed <- subset(data, conclusion == "bed stability")
vel <- subset(data, conclusion == "velocity")

dtm <- TermDocumentMatrix(vel$words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

pos_dat = d
pos_dat$color <- "#009E73"
neg_dat = d
neg_dat$color <- "#D55E00"
temp_dat = d
temp_dat$color <- "#E69F00"
hab_dat = d
hab_dat$color <- "#CC79A7"
bed_dat = d
bed_dat$color <- "#56B4E9"
vel_dat = d
vel_dat$color <- "#0072B2"


custom_dat <- rbind(pos_dat, neg_dat, temp_dat, hab_dat, bed_dat, vel_dat)
write.csv(custom_dat, "wordcloud_plot.csv")

png("wordcloud_plot.png", width = 6, height = 6, units = "in", res = 300)

layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))

wordcloud(custom_dat$word, custom_dat$freq, colors=custom_dat$color,
          min.freq = 1,
          max.words=200, random.order=FALSE, ordered.colors = TRUE, rot.per=0.35,
          family = "Helvetica")

plot(type = "n")
par(family = "Helvetica")
legend("center", title = "Conclusion Category", c("bed stability","habitat extent",
                    "temperature", "velocity","positive","negative"),
       col = c("#56B4E9","#CC79A7","#E69F00","#0072B2","#009E73","#D55E00"),
       pch = 15, cex = 1)

dev.off()
