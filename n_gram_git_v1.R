
### run the library ###
options(java.parameters = "-Xmx7g")
library(tm)
library(RCurl)
library(RWeka)
library(RWekajars)
library(sqldf)
library(data.table)
library(dplyr)
library(rJava)
library(NLP)
library(SnowballC)
library(rvest)
library(bindrcpp)
library(twitteR)
library(readxl)

###### select the file location from your computer location where the data should be contain in single coulmn.

file<- read.csv(file.choose(),header = T,sep=",")

f<-file


#### -- filter and cleaning process ####
temp <- f
temp <- gsub("[]$*+?[^{|(\\%&~_/<=>'!,:;`\")}@-]","",file[,1])
temp <- removeNumbers(temp)
temp <- gsub("."," ",temp,fixed = TRUE)
temp <- gsub(","," ",temp,fixed = TRUE)
temp <- removePunctuation(temp)
temp <- tolower(temp)
#corpus = tm_map(corpus, removeWords, c("hey","what","get","rt","i","will","can","twitter","a","may","the","we","us","its","at","u","n",stopwords("english")))
temp <- removeWords(temp, c("http","https","httpt","httpst","rt","etc","tco","auspost","hey","what","get","rt","i","will","can","twitter"
                            ,"a","may","the","we","us","its","at","u","n", stopwords("english")))
temp <- removeNumbers(temp)
temp <- removeWords(temp, stopwords("SMART"))


#### -- ngrams -----
## -- Setting up functions
f_tokens <- function(x,mn,mx) NGramTokenizer(x, Weka_control(min =mn,max = mx))
f_phr.len <- function(x) length(unlist(strsplit(x," ")))
f_nGram.tokens.select.gr <- function(len) nGram.tokens[which(nGram.tokens.phr.len==len)]

## -- Processing the data
corpus <- Corpus(VectorSource(temp))

## -- Tokenization
nGram.tokens <- f_tokens(x=unlist(temp),mn=1,mx=2)
nGram.tokens.phr.len <- sapply(1:length(nGram.tokens), function(i) f_phr.len(nGram.tokens[i]))


## -- Sorting according to requirements 
final_ngrams <- data.frame(matrix(nrow = 50))
for(len in c(1:4)){
  ngrams_df = data.frame(x = f_nGram.tokens.select.gr(len), stringsAsFactors = FALSE)
  ngrams_df = ngrams_df %>% group_by(x) %>% summarize(freq = n()) %>% mutate(prop = freq/sum(freq),
                                                                             ngram_len = len) %>% arrange(desc(freq))
  ngrams_df = ngrams_df[1:50,c(1,2)]
  # Take top 200 for length 1,2,3 and 4
  final_ngrams <- cbind(final_ngrams,ngrams_df)
}
final_ngrams <- final_ngrams[,-1]
colnames(final_ngrams) <-  c("ngrams - 1", "Frequency","ngrams - 2", "Frequency","ngrams - 3", "Frequency","ngrams - 4", "Frequency")


Input<-f[1:50,]

h<-data.frame(Input,final_ngrams)


write.csv(final_ngrams,"file_name.csv", row.names = F)


