#Loading Data#
Metrics <- read.csv("/home/aiddata/Desktop/Github/GeocoderUncertainty/Metric.csv")

#---Descriptives--#
#View(Metrics)
hist(Metrics$X..of.Relevant.Mentions)
hist(Metrics$X..of.Mentions)
hist(Metrics$X..of.Relevant.Mentions, xlab= "relevent mentions", border="blue", col="pink")
hist(Metrics$X, main = "projects", border = "green", col = "gold")
barplot(Metrics$X..of.Mentions)
barplot(Metrics$X..of.Relevant.Mentions)
summary(Metrics$X..of.Relevant.Mentions)
summary(Metrics)
str(Metrics)
Metrics$Tense.of.Language != Metrics$X
hist(Metrics$X..of.Relevant.Mentions)
hist(Metrics$X..of.Relevant.Mentions, main = "Relevant Mentions", xlab = "Mentions", col = "green")

#---Analysis--#
Metrics["Binary"] = 0 
Metrics["Binary"][Metrics$Correctly.in.AidData.File == "Yes",] <- 1
summary(Metrics$Binary)

Metrics_onlyYes <- Metrics[Metrics$Correctly.in.AidData.File == "Yes",]
Metrics_onlyNo <- Metrics[Metrics$Correctly.in.AidData.File == "No",]

MetricsYesNo <- rbind(Metrics_onlyYes, Metrics_onlyNo)
summary(MetricsYesNo)

names(MetricsYesNo)

names(MetricsYesNo)[2] = "Document"
names(MetricsYesNo)

prediction <- lm(Binary~ X..of.Emails,data = MetricsYesNo)
summary(prediction) 
plot(MetricsYesNo$X..of.Emails, MetricsYesNo$Binary)
