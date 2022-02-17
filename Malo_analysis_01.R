malo <- read.csv("/Users/yemoonkim/R/Malo4/dataset.csv")
malo2<-malo[,c(-1,-2,-3,-7,-17,-18,-21)]
#malo3 = subset(malo2, layout_name == "8_15_days")
#malo4 <- malo3[-grep("YES",malo3$answer)]

library(stringr)
malo2$analysis_id <- str_sub(malo2$analysis_id, 16,17)
malo2$analysis_id <- as.numeric(malo2$analysis_id)

malo2$layout_name <- as.factor(malo2$layout_name)
malo2$weight <- as.factor(malo2$weight)
malo2$hearing <- as.factor(malo2$hearing)
malo2$asd <- as.factor(malo2$asd)
malo2$vision <- as.factor(malo2$vision)
malo2$section_title <- as.factor(malo2$section_title)

write.csv(malo2, "/Users/yemoonkim/R/Malo4/malo2data2.csv")

malo2 <- read.csv("/Users/yemoonkim/R/Malo4/malo2data2.csv")


#######################################################
###                 Clustering                #########
#######################################################

library(ggplot2)
#glm(formula = admit ~ gre + gpa + rank, family = "binomial", data = mydata)

library (plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

# Prepare dataset using SQL. col = count of questions, grade, avg(analysis_id)

malo2_cluster <- read.csv("/Users/yemoonkim/R/Malo4/sql_data_clustering.csv")
malo3 <- as.matrix(malo2_cluster[,2:4])

wss <- numeric(30)
for(k in 1:30){
  wss[k]<-sum(kmeans(malo3, centers=k, nstart = 25, iter.max = 50)$withinss)
}
plot(1:30, wss, type="b", xlab="Number of Clusters" , ylab="Within Sum of Squares" )
km<-kmeans(malo3,3,nstart=25)
km


df=as.data.frame(malo3)
df$cluster=factor(km$cluster)
centers=as.data.frame(km$centers)

g1=ggplot(data=df, aes(x=malo2_cluster$questions, y=malo2_cluster$grade, color=cluster)) + geom_point()+
  geom_point(data=centers, aes(x=centers$questions, y=centers$grade), color=c("indianred1", "khaki3", "lightgreen"), size=8, show.legend=FALSE)

g2=ggplot(data=df, aes(x=malo2_cluster$questions, y=malo2_cluster$avg_analysis_id, color=cluster)) + geom_point()+
  geom_point(data=centers, aes(x=centers$questions, y=centers$avg_analysis_id), color=c("indianred1", "khaki3", "lightgreen"), size=8, show.legend=FALSE)

g3=ggplot(data=df, aes(x=malo2_cluster$grade, y=malo2_cluster$avg_analysis_id, color=cluster)) + geom_point()+
  geom_point(data=centers, aes(x=centers$grade, y=centers$avg_analysis_id), color=c("indianred1", "khaki3", "lightgreen"), size=8, show.legend=FALSE)

grid.arrange(g1,g2,g3)

#######################################################
###                 Association Rules         #########
#######################################################



#Prepare data for Arules

# 1. satisfied = FALSE
# 2. grade != 0
# 3. analysis_id >= 3

malo4_Arules <- malo2[malo2$analysis_id >= 3 & malo2$grade != 0 & malo2$satisfied == FALSE, ]


#Prepare dataframe of unique human_id list 

itemset <- data.frame(humanid = unique(malo4_Arules$human_id))

#practice
#if (itemset$humanid =="wg0Ul7hdonOdTyQJLwIEGxcTddthMXa"){
#  itemset[itemset$humanid =="wg0Ul7hdonOdTyQJLwIEGxcTddthMXa","item1""] <-"xb"}

#for i (1: nrow(itemset["wg0Ul7hdonOdTyQJLwIEGxcTddthMXa"]))
#itemset[itemset$humanid =="wg0Ul7hdonOdTyQJLwIEGxcTddthMXa",2] <-"xi"


list = list(c(itemset$humanid))

# pile up question_id horizontally according to human_id
for (i in 1:742){
  
  for (j in 1:nrow(malo4_Arules[malo4_Arules$human_id == list[[1]][i], ])){
    
   a <- malo4_Arules[malo4_Arules$human_id == list[[1]][i], ]$section_title[j]
   itemset[itemset$humanid == list[[1]][i], j+1] <- a
  }
}


#itemset[ list[[1]][2], ]
#nrow(malo2[ malo2$human_id == list[[1]][i], ])

itemset$humanid <- c(1:742)

write.csv(itemset, "/Users/yemoonkim/R/Malo4/market_basket3_section2.csv", row.names = F)


#And then, on Excel, bracket. I manually touched data. removed "NA" etc...


library('arules', warn.conflicts=F, quietly=T)
library('arulesViz', warn.conflicts=F, quietly=T)
library('readxl', warn.conflicts=F, quietly=T)


suppressWarnings(retail<-read.transactions('/Users/yemoonkim/R/Malo4/market_basket3_section.csv', format = 'basket', sep=','))


#  A summary of the dataset

retail
summary(retail)

class(retail)



# Frequent itemset Generation

itemsets1 <- apriori(retail,parameter=list(minlen=1,maxlen=1,support=0.001,target="frequent itemsets"))

summary(itemsets1)


# Top 10 frequent 1-itemsets

inspect(head(sort(itemsets1, by = "support"), 10))


# Frequent 2-itemset Generation

itemsets2 <- apriori(retail,parameter=list(minlen=2,maxlen=2,support=0.001,target="frequent itemsets"))

summary(itemsets2)


# Top 10 frequent 2-itemsets

inspect(head(sort(itemsets2, by = "support"), 10))


# Frequent 3-itemset Generation

itemsets3 <- apriori(retail,parameter=list(minlen=3,maxlen=3,support=0.001,target="frequent itemsets"))

summary(itemsets3)


# Top 10 frequent 3-itemsets

inspect(head(sort(itemsets3, by = "support"), 10))

# Create some rules

rules <- apriori(retail, parameter = list(supp=0.001, conf=0.8))

rules <- sort(rules, by='confidence', decreasing = TRUE)

summary(rules)

plot(rules)

# Finding subsets of rules containing "COFFEE" items

COFFEE_rules <- subset(rules, items %in% "COFFEE")
inspect(head(sort(COFFEE_rules , by="lift") , 10))

# Finding subsets of rules that precede "SUGAR JARS" purchases
SUGAR_rules <- subset(rules, rhs %pin% "SUGAR JARS")
inspect(SUGAR_rules)


# The top ten rules sorted by the lift

inspect(head(sort(rules , by="lift") , 10))

# Graph visualization of the top ten rules sorted by lift

highLiftRules<-head(sort(rules, by="lift"),10)
plot(highLiftRules, method="graph" , control=list (type="items" ))






