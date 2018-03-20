
#Data Wrangling Part
library(dplyr)
RegularSeasonDetailedResults <- read.csv("~/Desktop/R for Data Scientists/Midterm Project/RegularSeasonDetailedResults.csv")
myData = RegularSeasonDetailedResults
myData = myData[-c(2,4,6:8)]
newLData = summarise(
  select(
    group_by(myData,Season,Lteam),
    c(17:29)
  ),
  Lfgm = sum(Lfgm, na.rm = TRUE),
  Lfga = sum(Lfga, na.rm = TRUE),
  Lfgm3 = sum(Lfgm3, na.rm = TRUE),
  Lfga3 = sum(Lfga3, na.rm = TRUE),
  Lftm = sum(Lftm, na.rm = TRUE),
  Lfta = sum(Lfta, na.rm = TRUE),
  Lor = sum(Lor, na.rm = TRUE),
  Ldr = sum(Ldr, na.rm = TRUE),
  Last = sum(Last, na.rm = TRUE),
  Lto = sum(Lto, na.rm = TRUE),
  Lstl = sum(Lstl, na.rm = TRUE),
  Lblk = sum(Lblk, na.rm = TRUE),
  Lpf = sum(Lpf, na.rm = TRUE),
  countL = n()
)

newWData = summarise(
  select(
    group_by(myData,Season,Wteam),
    c(4:16)
  ),
  Wfgm = sum(Wfgm, na.rm = TRUE),
  Wfga = sum(Wfga, na.rm = TRUE),
  Wfgm3 = sum(Wfgm3, na.rm = TRUE),
  Wfga3 = sum(Wfga3, na.rm = TRUE),
  Wftm = sum(Wftm, na.rm = TRUE),
  Wfta = sum(Wfta, na.rm = TRUE),
  Wor = sum(Wor, na.rm = TRUE),
  Wdr = sum(Wdr, na.rm = TRUE),
  Wast = sum(Wast, na.rm = TRUE),
  Wto = sum(Wto, na.rm = TRUE),
  Wstl = sum(Wstl, na.rm = TRUE),
  Wblk = sum(Wblk, na.rm = TRUE),
  Wpf = sum(Wpf, na.rm = TRUE),
  countW = n() 
)
combinedData = merge(newLData,newWData,by.x = c("Season","Lteam"), by.y = c("Season","Wteam") )
mergedCount = mutate(combinedData, countTot = countW + countL)
mergedCount = mergedCount[,-c(16,30)]

for(i in 1:nrow(mergedCount)) {
  for(j in 3:15){
    mergedCount[i,j] = (mergedCount[i,j]+mergedCount[i,j+13])/mergedCount[i,29]
  }
}

mergedCount = mergedCount[, -c(16:29)]
names(mergedCount) = c("Season","Team", "fgm","fga", "fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf")

#Clustering Part

#Rename Dataset name
myData = RegularSeasonDetailedResultsSummary

summary(myData)
dim(myData)
scaledData = scale(myData)
scaledData = scaledData[,-1]
scaledData = scaledData[,-1]
scaledData = scaledData[,-1]
 

k.max <- 15 
 wss <- sapply(2:k.max,function(k){kmeans(scaledData, k,iter.max = 50, nstart=10 )$tot.withinss})
 


plot(2:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

#We select 4 clusters from the plotted elbow

km_p2 = kmeans(scaledData,4)
km_p2

#(between_SS / total_SS =  30.9 %)

pca_data = prcomp(scaledData, center = TRUE)


pca_data.var =pca_data$sdev ^2
pve=pca_data.var/sum(pca_data.var )

 plot(pve , xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')
 
 plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
 Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
 type='b')

 final_km_data = kmeans(pca_data$x[,1:2],4)
 final_km_data
 
 #between_SS / total_SS =  64.1 %
 

 #Ward D2 H-clusters
 p2_hcw2 = hclust(dist(pca_data$x[,1:2]),"ward.D2")
plot(p2_hcw2)
cutree(p2_hcw2,4)

library(dplyr)
RegularSeasonDetailedResultsSummary <- read.csv("~/Desktop/R for Data Scientists/Midterm Project/RegularSeasonDetailedResultsSummary.csv")
TourneyCompactResults <- read.csv("~/Desktop/R for Data Scientists/Midterm Project/TourneyCompactResults.csv")
myData = RegularSeasonDetailedResultsSummary
myData = myData[,-1]
myData2 = TourneyCompactResults
dataWithWTeamScore = inner_join(myData, myData2, by = c("Season" = "Season", "Team" = "Wteam"))
dataWithWTeamScore = dataWithWTeamScore[, -c(16,18:21)]
dataWithLTeamScore = inner_join(myData, myData2, by = c("Season" = "Season", "Team" = "Lteam"))
dataWithLTeamScore = dataWithLTeamScore[ , -c(16:18,20:21)]
colnames(dataWithWTeamScore)[16] <- "score"
colnames(dataWithLTeamScore)[16] <- "score"
combinedScore= rbind(dataWithWTeamScore,dataWithLTeamScore)
groupedData = select( group_by(combinedScore,Season, Team) , c(1:16))
arrangedData = arrange(combinedScore, Season, Team)
#Regression
lr.model = lm(score~fgm+to+or, data=arrangedData)
summary(lr.model)
#Evaluation of the model
mean(lr.model$residuals^2)
sqrt(mean(lr.model$residuals^2))/mean(arrangedData$score)
#Cross Validation
library("glmnet")
library("boot")
lr.model.2 = glm(score~fgm+to+or, data=arrangedData)
summary(lr.model.2)
cv.error.2 = cv.glm(arrangedData,lr.model.2)
cv.error.2$delta
