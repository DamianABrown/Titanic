rm(list = ls())

#Loading Titanic Dataset
library('titanic')

#Importing Testing and Trainging Datasets
train.data <- titanic_train

#View Summary Data
summary(train.data)

#Changing varaibles from continuos to discrete (factor) for classification in ML models 
train.data$Survived <- as.factor(train.data$Survived) #Survived, train
P.class <- as.factor(train.data$Pclass)
train.data <- subset(train.data, subset = Embarked!='')
summary(train.data)

#Visualize Data
#Load Requipred packages
library('tidyverse')

#Visualize Sex against Age, Fare and Count
GGP <- ggplot(data = train.data)
GGP3 <- GGP + geom_violin(aes(Sex, Age, fill = Survived)) + theme_classic() + labs(x = "Sex", y = "Age")
GGP1 <- GGP + geom_jitter(aes(Sex, Fare, color = Survived)) + theme_classic() + labs(x = "Sex", y = "Ticket Price")
GGP2 <- GGP + geom_boxplot(aes(Sex, Fare, fill = Survived)) + theme_classic() + labs(x = "Sex", y = "Ticket Price")
GGP4 <- GGP + geom_bar(aes(Sex, fill = Survived)) + theme_classic() + labs(x = "Sex", y = "Number of Passengers")
gridExtra::grid.arrange(GGP1, GGP2, ncol = 2)
gridExtra::grid.arrange(GGP3,GGP4, ncol = 2)


#Visualize Pclass against Age, Fare, and Count. Pclass = Ticket Class
GGP3 <- GGP + geom_violin(aes(P.class, Age, fill = Survived)) + theme_classic() + 
  labs(x = "Passenger Ticket Class", y = "Passenger Age")
GGP1 <- GGP + geom_jitter(aes(P.class, Fare, color = Survived)) + theme_classic() +
  labs(x = "Passenger Ticket Class", y = "Ticket Price")
GGP2 <- GGP + geom_boxplot(aes(P.class, Fare, fill = Survived)) + theme_classic() + 
  labs(x = "Passenger Ticket Class", y = "Ticket Price")
GGP4 <- GGP + geom_bar(aes(P.class, fill = Survived)) + theme_classic() + labs(x = "Passenger Ticket Class", y = "Number of Passenger")
gridExtra::grid.arrange(GGP1, GGP2, ncol = 2)
gridExtra::grid.arrange(GGP3,GGP4, ncol = 2)

#Visualize Embarked against Age, Fare, and Count. Embarked = Which port passenger embarked on
GGP + geom_violin(aes(Embarked, Age, fill = Survived)) + theme_classic() + labs(x = "Embarking Port", y = "Passenger Age")
GGP1 <- GGP + geom_jitter(aes(Embarked, Fare, color = Survived)) + theme_classic() + 
  labs(x = "Embarking Port", y = "Ticket Price")
GGP2 <- GGP + geom_boxplot(aes(Embarked, Fare, fill = Survived)) + theme_classic() + 
  labs(x = "Embarking Port", y = "Ticket Price")
GGP4 <- GGP + geom_bar(aes(Embarked, fill = Survived)) + theme_classic() + labs(x = "Embarking Port", y = "Number of Passengers")
gridExtra::grid.arrange(GGP1, GGP2)
gridExtra::grid.arrange(GGP3,GGP4)

#Visualize Parch against Age, Fare, and Count. Parch = Number or parents or children onboard
GGP1 <- GGP + geom_bar(aes(Parch, fill = Survived), position = 'Dodge') + theme_classic() + 
  labs(x = "Number of Parents or Children Onboard", y = "Total Passengers")
GGP2 <- GGP + geom_bar(aes(Parch, fill = Survived), position = "fill") + theme_classic() + 
  labs(x = "Number of Parents or Children Onboard", y = "Total Passengers")
gridExtra::grid.arrange(GGP1, GGP2)

#Visualize SibSp against Age, Fare, and Count. SibSp = Number of Siblings or Spouse onboard
GGP1 <- GGP + geom_bar(aes(SibSp, fill = Survived), position = 'Dodge') + theme_classic() + 
  labs(x = "Number of Siblings or Spouse Onboard", y = "Total Passengers")
GGP2 <- GGP + geom_bar(aes(SibSp, fill = Survived), position = "fill") + theme_classic() + 
  labs(x = "Number of Siblings or Spouse Onboard", y = "Total Passengers")
gridExtra::grid.arrange(GGP1, GGP2)

#Visualize Fare to Pclass and Fare to Embarked
GGP1 <- GGP + geom_boxplot(aes(Pclass, Fare, fill = Embarked, color = Embarked)) + theme_classic() + 
  labs(x = "Passenger Ticket Class", y = "Price of Ticket")
GGP2 <- GGP + geom_bar(aes(Pclass, fill = Embarked), position = 'Dodge') + theme_classic() + 
  labs(x = "Passenger Ticket Class", y = "Number of Passengers")
gridExtra::grid.arrange(GGP1, GGP2)

#Removing new variables created while visualizing
remove(P.class)

#Inputting missing Age values for passengers
age <- train.data$Age
sub.age <- age[!is.na(age)]
ggplot() + geom_density(aes(sub.age), size = 1, color = "Orange", fill = "Orange") + 
  labs(x = "Passenger Age", y = "Age Density") + theme_classic()
new.age <- round(rnorm((length(age)-length(sub.age)), mean = mean(sub.age), sd = sd(sub.age)))
ggplot() + geom_density(aes(sub.age), size = 1, color = "Orange") + geom_density(aes(new.age), size = 1, color = "Blue") + 
  labs(x = "Passenger Age", y = "Age Density") + theme_classic()
for (ii in 1:length(age)){
  rand = round(rnorm(1, mean = mean(sub.age), sd = sd(sub.age)))
  if(rand <= 0) rand = rand^2
  if(rand > 65) rand = round(mean(sub.age))
  if(is.na(age[ii])) (age[ii] = rand)
}
ggplot() + geom_density(aes(sub.age, color = "Origianl Age"), size = 2, color = "Orange") + geom_density(aes(new.age), size = 2, color = "Blue") + 
  geom_density(aes(age, color = "New Age"), size = 2, color = 'Green') +
  labs(x = "Passenger Age", y = "Age Density") + theme_classic()
train.data$Age = age
remove(age, ii, new.age, sub.age, GGP1, GGP2, GGP, GGP3, GGP4, rand)

#Create Subset of important variables removing non essential
training.data <- subset(train.data, select = c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"))
training.data$Sex <- as.factor(training.data$Sex)
training.data$Embarked <- as.factor(training.data$Embarked)
training.data$Pclass <- as.factor(training.data$Pclass)

#Loading required packages for ML models
library('randomForest') #Random Forest
library('e1071') #Support Vector Machine
library('gbm') #Gradient Boost

#Creating Training and Validation Dataset
summary(training.data)
split <- sample(c(TRUE, FALSE), nrow(training.data), replace = TRUE, prob= c(0.7, 0.3))
training.model.data <- training.data[split,]
validate.model.data <- training.data[(split == FALSE),]
dim(training.model.data)
dim(validate.model.data)

#Logistic Regression
logistic.model <- glm(Survived ~ ., data = training.model.data, family = binomial)
Survived <- training.model.data$Survived
ggplot() + geom_point(aes(x = 1:nrow(training.model.data), y = logistic.model$fitted.values, 
                          color = Survived)) + theme_classic() + labs(x = "Observation", y = "Fitted Values")
pred_class <- ifelse(logistic.model$fitted.values >= 0.55, 1, 0)
log.table <- table(training.model.data$Survived, pred_class)
log.table
head(logistic.model$fitted.values, 10)

#Random Forest Classification
forest.model <- randomForest(Survived ~ ., data = training.model.data, ntree = 1500, mtry = 3)
ggplot() + geom_point(aes(x = 1:nrow(training.model.data), y = forest.model$predicted, 
                          color = Survived)) + theme_classic() + labs(x = "Observation", y = "Fitted Values")
print(forest.model[["confusion"]])
(forest.model)


#SVM "Support Vector Machine"
svm.model <- svm(Survived ~ ., data = training.model.data)
ggplot() + geom_point(aes(x = 1:nrow(training.model.data), y = svm.model$decision.values, 
                          color = Survived)) + theme_classic() + labs(x = "Observation", y = "Decision Values")
train.table <- table(training.model.data$Survived, svm.model[['fitted']])
train.table


#Running Model on Validation dataset
logistic.predicted <- predict(logistic.model, validate.model.data, type="response")
forest.predicted <- predict(forest.model, validate.model.data, type="response")
svm.predicted <- predict(svm.model, validate.model.data, type="response")

#Creating confision matrices 
pred_class.log <- ifelse(logistic.predicted >= .55, 1, 0)
log.table <- table(validate.model.data$Survived, pred_class.log)
pred_class.rf <- ifelse(forest.predicted == 1, 1, 0)
forest.table <- table(validate.model.data$Survived, pred_class.rf)
pred_class.svm <- ifelse(svm.predicted == 1, 1, 0)
svm.table <- table(validate.model.data$Survived, pred_class.svm)
pred_class_typeI <- ifelse((pred_class.log+pred_class.rf+pred_class.svm) == 3, 1, 0)
typeI.table <- table(validate.model.data$Survived, pred_class_typeI)
pred_class_typeII <- ifelse((pred_class.log+pred_class.rf+pred_class.svm) >= 1, 1, 0)
typeII.table <- table(validate.model.data$Survived, pred_class_typeII)

#Create dataframe for results inputs
library('gt')
df <- tibble(Model = c("Logistic Regression", "Random Forest", "SVM", "Decrease Type I Error", "Decrease Type II Error"), 
             "Model Accuracy" = c(format(round((log.table[1,1]+log.table[2,2])/sum(log.table), 4), nsmall = 4), 
                                  format(round((forest.table[1,1]+forest.table[2,2])/sum(forest.table), 4), nsmall = 4), 
                                  format(round((svm.table[1,1]+svm.table[2,2])/sum(svm.table), 4), nsmall = 4),
                                  format(round((typeI.table[1,1]+typeI.table[2,2])/sum(typeI.table), 4), nsmall = 4),
                                  format(round((typeII.table[1,1]+typeII.table[2,2])/sum(typeII.table), 4), nsmall = 4)), 
             "Sensitivity/True Positive" = c(format(round(log.table[2,2]/(log.table[2,1]+log.table[2,2]), 4), nsmall = 4), 
                                    format(round(forest.table[2,2]/(forest.table[2,1]+forest.table[2,2]), 4), nsmall = 4), 
                                    format(round(svm.table[2,2]/(svm.table[2,1]+svm.table[2,2]), 4), nsmall = 4),
                                    format(round(typeI.table[2,2]/(typeI.table[2,1]+typeI.table[2,2]), 4), nsmall = 4),
                                    format(round(typeII.table[2,2]/(typeII.table[2,1]+typeII.table[2,2]), 4), nsmall = 4)), 
             "Specificity/True Negative" = c(format(round(log.table[1,1]/(log.table[1,1]+log.table[1,2]), 4), nsmall = 4), 
                                        format(round(forest.table[1,1]/(forest.table[1,1]+forest.table[1,2]), 4), nsmall = 4), 
                                        format(round(svm.table[1,1]/(svm.table[1,1]+svm.table[1,2]), 4), nsmall = 4),
                                        format(round(typeI.table[1,1]/(typeI.table[1,1]+typeI.table[1,2]), 4), nsmall = 4),
                                        format(round(typeII.table[1,1]/(typeII.table[1,1]+typeII.table[1,2]), 4), nsmall = 4)),
             "False Positive/Type I Error" = c(format(round(log.table[1,2]/(log.table[1,1]+log.table[1,2]), 4), nsmall = 4), 
                                             format(round(forest.table[1,2]/(forest.table[1,1]+forest.table[1,2]), 4), nsmall = 4), 
                                             format(round(svm.table[1,2]/(svm.table[1,1]+svm.table[1,2]), 4), nsmall = 4),
                                             format(round(typeI.table[1,2]/(typeI.table[1,1]+typeI.table[1,2]), 4), nsmall = 4),
                                             format(round(typeII.table[1,2]/(typeII.table[1,1]+typeII.table[1,2]), 4), nsmall = 4)), 
             "False Negative/Type II Error" = c(format(round(log.table[2,1]/(log.table[2,1]+log.table[2,2]), 4), nsmall = 4), 
                                             format(round(forest.table[2,1]/(forest.table[2,1]+forest.table[2,2]), 4), nsmall = 4), 
                                             format(round(svm.table[2,1]/(svm.table[2,1]+svm.table[2,2]), 4), nsmall = 4),
                                             format(round(typeI.table[2,1]/(typeI.table[2,1]+typeI.table[2,2]), 4), nsmall = 4),
                                             format(round(typeII.table[2,1]/(typeII.table[2,1]+typeII.table[2,2]), 4), nsmall = 4))
             )
gt(df)
