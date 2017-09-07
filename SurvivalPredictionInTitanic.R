library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm



train_file_titanic <- read.csv("/Users/stacy/Downloads/train-2.csv",stringsAsFactors = F)
class(train_file_titanic)
View(train_file_titanic)
train_file_titanic$Title <- gsub('(.*, )|(\\..*)','',train_file_titanic$Name)
View(train_file_titanic)
rare_titles <- c("Don","Rev","Major","Lady","Sir","Capt","Col","Jonkheer","the Countess")
#reassigning few titles
train_file_titanic$Title[train_file_titanic$Title=="Mlle"] <- "Miss"
train_file_titanic$Title[train_file_titanic$Title=="Ms"] <- "Miss"
train_file_titanic$Title[train_file_titanic$Title=="Mme"] <- "Mrs"
train_file_titanic$Title[train_file_titanic$Title=="Mlle"] <- "Miss"

#Changes the rare titles into one single title
train_file_titanic$Title[match(rare_titles,train_file_titanic$Title)] <- "Rare Title"

testing <- train_file_titanic$Name
train_file_titanic$SurName <- gsub(",.*$","", testing)
nlevels(factor(train_file_titanic$SurName))

#Total family
train_file_titanic$total_family_size <- train_file_titanic$SibSp + train_file_titanic$Parch +1
train_file_titanic$Testing_family_variable <- paste(train_file_titanic$Title,train_file_titanic$total_family_size,sep='_')
length(train_file_titanic$Title)

# Use ggplot2 to visualize the relationship between family size & survival
#the factor is used to show both the factors(levels 0/1) in the plot. Otherwise only the survived poeple would show up and we would not know how 
#many people survived against the ones who died

ggplot(train_file_titanic[1:891,], aes(x = total_family_size, fill = factor(train_file_titanic$Survived)))+
  scale_x_continuous(breaks=c(1:11))+
  geom_bar(stat='count', position='dodge')+
  labs(x = 'Family Size')+
  theme_few()

#We will find that there is a penalty for people who were singles or who who had more than 4 family members.To further prove our point
#we will categorise the family size into buckets.
train_file_titanic$DiscretFamily <- ifelse(train_file_titanic$total_family_size==1,"Single",ifelse(train_file_titanic$total_family_size<5,"Small","Large"))
mosaicplot(table(train_file_titanic$DiscretFamily, train_file_titanic$Survived), main='Family Size by Survival', shade=TRUE)

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','SurName','Testing_family_variable','DiscretFamily')

factor_vars_testing <- c('PassengerId','Sex')

train_file_titanic[factor_vars_testing]
train_file_titanic[factor_vars] <- lapply(train_file_titanic[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(train_file_titanic[, !names(train_file_titanic) %in% c('PassengerId','Name','Ticket','Cabin','Testing_family_variable','SurName','Survived')], method='rf') 
par(mfrow=c(1,2))
hist(train_file_titanic$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
