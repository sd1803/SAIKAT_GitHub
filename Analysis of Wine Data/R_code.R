rm(list=ls())
library(readxl)
library(dplyr)
library(moments)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(visdat)
library(lattice)
library(caret)
library(magrittr)
library(pROC)
library(broom)
library(GGally)
wine <- read_excel("C:\\Users\\SAIKAT DATTA\\OneDrive\\Desktop\\wine dissertation\\wine.xlsx")
View(wine)
#To visualize missing values
sum(is.na(wine))
vis_miss(wine)
skewness(wine)
#Distribution of the response
ggplot(data = wine)+
  geom_boxplot(aes(x=quality),color='black',fill='4')+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of the respose variable (quality)')+
  theme(plot.title = element_text(hjust=0.5))

quality_data <- melt(wine$quality, id = c("Type")) %>%
  group_by(wine$quality) %>% summarize(count = n())

ggplot(data = quality_data,aes(x=`wine$quality`,y=count,
                               fill=`wine$quality`))+
  geom_bar(stat = "identity",show.legend = F)+
  labs(title = 'Barplot of the respose variable (quality)',
       x='quality',
       y='Frequency')+
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(label = count), vjust = 0,lwd=5)

#Boxplot of predictors and descriptive measures
colnames(wine)
b1 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`fixed acidity`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Fixed acidity')+
  theme(plot.title = element_text(hjust=0.5))
b2 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`volatile acidity`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Volatile acidity')+
  theme(plot.title = element_text(hjust=0.5))
b3 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`citric acid`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Citric acid')+
  theme(plot.title = element_text(hjust=0.5))
b4 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`residual sugar`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Residual sugar')+
  theme(plot.title = element_text(hjust=0.5))
b5 <- ggplot(data = wine)+
  geom_boxplot(aes(x=chlorides,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Chlorides')+
  theme(plot.title = element_text(hjust=0.5))
b6 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`free sulfur dioxide`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Free sulfur dioxide')+
  theme(plot.title = element_text(hjust=0.5))
b7 <- ggplot(data = wine)+
  geom_boxplot(aes(x=`total sulfur dioxide`,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Total sulfur dioxide')+
  theme(plot.title = element_text(hjust=0.5))
b8 <- ggplot(data = wine)+
  geom_boxplot(aes(x=density,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Density')+
  theme(plot.title = element_text(hjust=0.5))
b9 <- ggplot(data = wine)+
  geom_boxplot(aes(x=pH,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of pH')+
  theme(plot.title = element_text(hjust=0.5))
b10 <- ggplot(data = wine)+
  geom_boxplot(aes(x=sulphates,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Sulphates')+
  theme(plot.title = element_text(hjust=0.5))
b11 <- ggplot(data = wine)+
  geom_boxplot(aes(x=alcohol,fill=quality))+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Boxplot of Alcohol')+
  theme(plot.title = element_text(hjust=0.5))
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,
             b11,nrow=4)

#Descriptive measures of the predictors
sw <- summary(wine[,1:11]);sw
skewness(wine[,1:11])
kurtosis(wine[,1:11])

#Histogram of features
h1 <- ggplot(data = wine)+
  geom_histogram(aes(x=`fixed acidity`,
                     y=..density..),col='black',fill=4,
                 bins = 100)+
  geom_density(aes(`fixed acidity`),color='blue',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of fixed density')+
  theme(plot.title = element_text(hjust=0.5))
h2 <- ggplot(data = wine)+
  geom_histogram(aes(x=`volatile acidity`,
                     y=..density..),col='black',fill='burlywood',
                 bins = 100)+
  geom_density(aes(`volatile acidity`),color='brown',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of volatile acidity')+
  theme(plot.title = element_text(hjust=0.5));h2
h3 <- ggplot(data = wine)+
  geom_histogram(aes(x=`citric acid`,
                     y=..density..),col='black',fill='darkseagreen',
                 bins = 100)+
  geom_density(aes(`citric acid`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of citric acid')+
  theme(plot.title = element_text(hjust=0.5))
h4 <- ggplot(data = wine)+
  geom_histogram(aes(x=`residual sugar`,
                     y=..density..),col='black',fill='#F6FC57',
                 bins = 100)+
  geom_density(aes(`residual sugar`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of residual sugar')+
  theme(plot.title = element_text(hjust=0.5))
h5 <- ggplot(data = wine)+
  geom_histogram(aes(x=`chlorides`,
                     y=..density..),col='black',fill='#FF99CC',
                 bins = 100)+
  geom_density(aes(`chlorides`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of chlorides')+
  theme(plot.title = element_text(hjust=0.5))
h6 <- ggplot(data = wine)+
  geom_histogram(aes(x=`free sulfur dioxide`,
                     y=..density..),col='black',fill='#9999FF',
                 bins = 100)+
  geom_density(aes(`free sulfur dioxide`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of free sulfur dioxide')+
  theme(plot.title = element_text(hjust=0.5))
h7 <- ggplot(data = wine)+
  geom_histogram(aes(x=`total sulfur dioxide`,
                     y=..density..),col='black',fill='#FFFF99',
                 bins = 100)+
  geom_density(aes(`total sulfur dioxide`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of total sulfur dioxide')+
  theme(plot.title = element_text(hjust=0.5))
h8 <- ggplot(data = wine)+
  geom_histogram(aes(x=`density`,
                     y=..density..),col='black',fill='#FFCCCC',
                 bins = 100)+
  geom_density(aes(`density`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of density')+
  theme(plot.title = element_text(hjust=0.5))
h9 <- ggplot(data = wine)+
  geom_histogram(aes(x=`pH`,
                     y=..density..),col='black',fill='#66B2FF',
                 bins = 100)+
  geom_density(aes(`pH`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of pH')+
  theme(plot.title = element_text(hjust=0.5))
h10 <- ggplot(data = wine)+
  geom_histogram(aes(x=`sulphates`,
                     y=..density..),col='black',fill='#B2FF66',
                 bins = 100)+
  geom_density(aes(`sulphates`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of sulphates')+
  theme(plot.title = element_text(hjust=0.5))
h11 <- ggplot(data = wine)+
  geom_histogram(aes(x=`alcohol`,
                     y=..density..),col='black',fill='#FF99FF',
                 bins = 100)+
  geom_density(aes(`alcohol`),color='black',lwd=1)+
  theme(panel.background = element_rect(fill='azure2'))+
  labs(title = 'Histogram of alcohol')+
  theme(plot.title = element_text(hjust=0.5))
grid.arrange(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,nrow=4)


#Correlation among the features
#Pairplot
pair <- ggpairs(wine[,1:11],
                upper = list(continuous = GGally::wrap(ggally_cor, stars = F)),
                title='Pairplot of the Predictors')
pair


#Partial correlations
partial.cor_new <-  
  corpcor::cor2pcor(cov(wine[,1:11]))
colnames(partial.cor_new)=colnames(wine[,1:11])
rownames(partial.cor_new)=colnames(wine[,1:11])
mel.partial_new = melt(data.matrix(partial.cor_new))
ggplot(mel.partial_new, aes(Var1,Var2))+geom_tile(aes(fill=value)) +
  geom_text(aes(label = round(value, 2)))+
  scale_fill_gradient2(low='blue' ,
                       mid='white',
                       high='red') + 
  labs(title = 'Partial Correlation Heatmap of Predictors')+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust=0.5))

#Fitting Binomial Logistic regression
wine$category[wine$quality <= 5] <- 0
wine$category[wine$quality > 5] <- 1
wine$category <- as.factor(wine$category) 
model_glm <- glm(category~.-quality,
                 wine,
                 family = binomial(link = "logit"))
s <- summary(model_glm);s
a <- tidy(model_glm)
write.csv(a,"C:\\Users\\SAIKAT DATTA\\OneDrive\\Desktop\\wine dissertation\\logistic_results.csv", row.names = FALSE)

#Goodness of fit
R_sq <- 1-(s$deviance/s$null.deviance);R_sq
