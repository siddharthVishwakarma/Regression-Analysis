#importing dataset
datasets = read.csv('Position_Salaries.csv')
datasets = datasets[1:4]

#fitting linear regression to dataset
lin_reg = lm(formula = Salary~.,
             data = datasets)

#fitting polynomial regression to dataset
datasets$Level2 = datasets$Level^2
datasets$Level3 = datasets$Level^3
datasets$Level4 = datasets$Level^4
poly_reg = lm(formula = Salary~.,
              data = datasets)

#plotting lin_reg
ggplot()+
  geom_point(aes(x = datasets$Level, y = datasets$Salary),
             colour = 'red')+
  geom_line(aes(x = datasets$Level, y = predict(lin_reg,newdata = datasets)),
            colour = 'blue')+
  ggtitle('salary calculate')+
  xlab('Levels')+
  ylab('Salary')

#plotting poly_reg
ggplot()+
  geom_point(aes(x = datasets$Level, y = datasets$Salary),
             colour = 'red')+
  geom_line(aes(x = datasets$Level, y = predict(poly_reg,newdata = datasets)),
            colour = 'blue')+
  ggtitle('salary calculate')+
  xlab('Levels')+
  ylab('Salary')

#predicting a new result with linear regression
y_pred = predict(lin_reg , data.frame(Level = 6.5))

#predicting a new result with polynomial regression
y_pred = predict(poly_reg , data.frame(Level = 6.5,
                                       Level2 = 6.5^2,
                                       Level3 = 6.5^3,
                                       Level4 = 6.5^4))



