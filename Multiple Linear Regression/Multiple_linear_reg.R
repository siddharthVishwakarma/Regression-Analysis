#importing the dataset
dataset=read.csv('50_Startups.csv')

#Encoding catiogerical data
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

#spliting dataset into the test sets and traning sets
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(dataset$Profit,SplitRatio = 0.8)
traning_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#feature scalling

#Fitting multiple linear regression to the traning set
regressor = lm(formula = Profit ~ .,
               data = traning_set)

y_pred = predict(regressor, newdata = test_set)


#plotting
ggplot()+
  geom_point(aes(x = traning_set$R.D.Spend, y = traning_set$Profit),
             colour = 'red')+
  geom_line(aes(x = traning_set$R.D.Spend, y = predict(regressor,newdata = traning_set)),
            colour = 'blue')+
  ggtitle('profit calculate')+
  xlab('Research and Devlopment')+
  ylab('Profit')

ggplot()+
  geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit),
             colour = 'red')+
  geom_line(aes(x = traning_set$R.D.Spend, y = predict(regressor,newdata = traning_set)),
            colour = 'blue')+
  ggtitle('profit calculate')+
  xlab('Research and Devlopment')+
  ylab('Profit')

















