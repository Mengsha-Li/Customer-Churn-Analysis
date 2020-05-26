library(readxl)
library(ggplot2)
colnames(revenue) <- c("camp", "id","monbe","monaf","age","joinornot","churn","spent")
ggplot(revenue, aes(comp, c(monbe,monaf))) +
  geom_boxplot(colour = "grey50") +
  geom_jitter()


ggplot(revenue, aes(comp, monaf)) +
  geom_boxplot(colour = "grey50") +
  geom_jitter()

ggplot(revenue, aes(comp, monbe)) +
  geom_jitter(width = 0.1, height = 0.1)

ggplot(revenue, aes(comp, monaf)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1))

jitter <- position_jitter(width = 0.1, height = 0.1)
ggplot(revenue) +
  geom_point(position = jitter, aes(comp, monbe)) +
  geom_point(position = jitter, color = "red", aes(comp + 0.2, monaf))

pie(revenue$comp, labels = "comp")
pie(revenue$comp)
ggplot(revenue,aes(monbe, monaf)) +
  geom_boxplot(colour = "grey50")

# For campaign
campaign <- c(sum(revenue$camp), nrow(revenue)-sum(revenue$camp))
source <- c("Campaign", "Organic")
pct1 <- round(campaign/nrow(revenue)*100)
source <- paste(source, pct1) # add percents to labels
source <- paste(source,"%",sep="") # ad % to labels
pie(campaign, labels = source, main="User Source")

join <- c(sum(revenue$joinornot), nrow(revenue)-sum(revenue$joinornot))
label <- c("Join", "NotJoin")
pct2 <- round(join/nrow(revenue)*100)
label <- paste(label, pct2) # add percents to labels
label <- paste(label,"%",sep="") # ad % to labels
pie(join, labels = label, main="User Community Participation")

churn <- c(sum(revenue$churn), nrow(revenue)-sum(revenue$churn))
label <- c("Churn", "NotChurn")
pct3 <- round(churn/nrow(revenue)*100)
label <- paste(label, pct3) # add percents to labels
label <- paste(label,"%",sep="") # ad % to labels
pie(churn, labels = label, main="Customer Churn")

hist(revenue$monbe)
hist(revenue$monaf)
hist(revenue$spent, col=rgb(1,0,0,0.5), main="Average Spend Last 3 months of Life with the firm",
     xlab="Average Spend")
hist(revenue$age, col=rgb(1,0,0,0.5), main="Customer Age with the firm",
     xlab="Age by month")
hist(revenue$monbe, col=rgb(1,0,0,0.5), xlim = c(0,220),main="Monthly Spend Before and After",
     xlab="Monthly Spend")
hist(revenue$monaf, col=rgb(0,0,1,0.5), add=T)
box()
