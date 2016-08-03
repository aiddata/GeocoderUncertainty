#--PDF Corpus--#
install.packages("tm")
library(tm)
files <- list.files(pattern = "pdf$")
pdf.A <- readPDF(control = list(text = "-layout"))(elem=list(uri=files),
                                                   language="en",
                                                   id="id1")

meta(pdf.A)

pdf.corpus <- Corpus(VectorSource(pdf.A))
pdf.tdm <- TermDocumentMatrix(pdf.corpus)


most_common <- findFreqTerms(opinions.tdbm, lowfreq = 50, highfreq = Inf)
termFreq(pdf.A)[most_common]
past_tense <- c("was", "were")
termFreq(pdf.A)[past_tense]


present_tense_words <- 10
past_tense_words <- 10

if(present_tense_words > past_tense_words)
{
  tense="present"
}

install.packages("tm")
library(tm)

files <- list.files(pattern = "pdf$")
pdf.A <- readPDF(control = list(text = "-layout"))(elem=list(uri=files),
                                                   language="en",
                                                   id="id1")
meta(pdf.A)

pdf.corpus <- Corpus(VectorSource(pdf.A))
pdf.tdm <- TermDocumentMatrix(pdf.corpus)


most_common <- findFreqTerms(pdf.tdm, lowfreq = 15, highfreq = Inf)
termFreq(pdf.A)[most_common]
#--PastTense--#
past_tense <- c("was", "were", "been", "had", "improved", "increased", "provided", 
                "achieved", "included", "supported")
termFreq(pdf.A)[past_tense]
past_tense_words <- sum(termFreq(pdf.A)[past_tense]) 
past_tense_words 

#--PresentTense--#
present_tense <- c("are","have", "can", "has", "including")
present_tense_words <- sum(termFreq(pdf.A)[present_tense])
present_tense_words

if(past_tense_words > present_tense_words)
{
  print("past")
} else {print("present")}


