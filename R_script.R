#New
Metric <- read.csv("/home/aiddata/Desktop/Github/GeocoderUncertainty/Metric.csv")

CopyA <- Metric



# Split Title by Name and String
Metric$Doc <- as.character(Metric$Doc)
Doc.Name <- strsplit(Metric$Doc, " ")

Doc.Name.List <- as.data.frame(matrix(NA,0,1))
for(i in 1:length(Doc.Name)){
  Name.temp <- Doc.Name[[i]][1]
  Doc.Name.List <- rbind(Doc.Name.List, as.data.frame(matrix(Name.temp,1,1)))
}

Doc.Name.List <- as.data.frame(Doc.Name.List)
Metric$Doc.Name <- Doc.Name.List$V1

#View(Metric)
Metric$Binary = 0
if (Metric$Correctly.in.AidData.File == "Yes") {Metric$Binary = 0}
> Metric["Binary"][Metric$Correct_File == "Yes",] <- 1
summary(Metric$Binary)

#--Analysis--#
#---Analysis--#
  Metric["Binary"] = 0 
Metric["Binary"][Metric$Correct_File == "Yes",] <- 1
summary(Metric$Binary)

Metric_onlyYes <- Metric[Metric$Correct_File == "Yes",]
Metric_onlyNo <- Metric[Metric$Correct_File == "No",]

Metric <- Metric[complete.cases(Metric),]

MetricYesNo <- rbind(Metric_onlyYes, Metric_onlyNo)
summary(Metric_onlyYes)

summary(MetricYesNo)
View(MetricYesNo)
prediction0 <- lm(Binary~Rel_Mentions + Tense_Lang, Open.Closed,
                 data = Metric)
summary(prediction)
install.packages("aod")
prediction1 <- lm(Binary~Tense_Lang + factor(Doc.Name), 
                  data = Metric)
summary(prediction1)
install.packages("ggplot2")
names(Metric)

prediction2 <- lm(Binary~Tense_Lang + Prec_Code,
                  data = Metric)
summary(prediction2)

prediction3 <- lm(Binary~factor(Doc.Name) + Rel_Mentions,
                  data = Metric)
summary(prediction3)

prediction4 <- lm(Binary~Mentions + Rel_Mentions,
                  data = Metric)
summary(prediction4)

prediction5 <- lm(Binary~Mentions + Tense_Lang,
                  data = Metric)
summary(prediction5)

prediction6 <- lm(Binary~Mentions + Tense_Lang + factor(Doc.Name),
                  data = Metric)
summary(prediction6)

prediction7 <- lm(Binary~Mentions + Tense_Lang + Open.Closed,
                  data = Metric)
summary(prediction7)
Metric$prediction7<-predict(prediction7, data=Metric)
plot(Metric$prediction7, Metric$Binary)



prediction13 <- lm(Binary~Mentions + Prec_Code,
                   data = Metric)
summary(prediction13)

install.packages(Rcpp)
predic_log7 <- glm(Binary~Mentions + Tense_Lang + Open.Closed,
       data = Metric, family = binomial)
summary(predic_log7)
summary(prediction7)

#--New Rows--#
newRow115 <- data.frame(X = 115, Doc = "PAD (2002)", ID = "P002952", ID_2 = "P002952_448225", 
                     Location = "Yumbe", Prec_Code = 3, Open.Closed = "Closed",  
                     Mentions = 1, Rel_Mentions = 1, Maps = 1, Rel_Maps = 0, Emails = 2, 
                     People = 2, Addresses = 1, Tense_Lang = "Future", Correct_File = "Yes", 
                     Doc.Name = "PAD", Binary = 1)
#names(Metric)
#names(newRow)
Metric <- rbind(Metric,newRow115)
View(Metric)

prediction7 <- lm(Binary~Mentions + Tense_Lang + Open.Closed,
                  data = Metric)
summary(prediction7)

prediction10 <- lm(Binary~Mentions + Tense_Lang + Open.Closed, + Doc.Name,
                  data = Metric)
summary(prediction10)

#--Logit Regression--#
mylogit <- glm(Binary ~ Mentions + Tense_Lang + Open.Closed, data = Metric, family = "binomial")
summary(mylogit)

#--Stepwise--#
Metric1 <- subset(Metric, select = -c(Correct_File))
null <- lm(Binary~1, data=Metric1)
full <- lm(Binary~., data=Metric1)
stepwise <- step(mylogit, scope = list(upper=full), data=Metric1, direction="both")
names(Metric1)
#View(Metric1)

