#simple linear regreassion

dataset=read.csv('Salary_Data.csv')

#install.packages("caTools")

set.seed(123)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
traning_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#feature scalling

#traning_set[,2:3]=scale(traning_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])

regressor=lm(formula = Salary ~ YearsExperience,
             data = traning_set)

y_pred=predict(regressor,newdata = test_set)


#install.packages("ggplot2")

ggplot()+
  geom_point(aes(x = traning_set$YearsExperience, y = traning_set$Salary),
             colour = 'red')+
  geom_line(aes(x = traning_set$YearsExperience, y = predict(regressor,newdata = traning_set)),
            colour = 'blue')+
  ggtitle('Salary vs Experience (traning set)')+
  xlab('years of experience')+
  ylab('salary')


ggplot()+
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red')+
  geom_line(aes(x = traning_set$YearsExperience, y = predict(regressor,newdata = traning_set)),
            colour = 'blue')+
  ggtitle('Salary vs Experience (test set)')+
  xlab('years of experience')+
  ylab('salary')













