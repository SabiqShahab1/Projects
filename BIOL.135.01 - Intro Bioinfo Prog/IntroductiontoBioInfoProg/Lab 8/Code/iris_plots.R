#load ggplot library
library(ggplot2)
#plotting boxplot with petal length as the x axis and y as the species
Boxplot <- ggplot(iris, aes(x=Petal.Length, y=Species)) +
  geom_boxplot()
#displaying plot
Boxplot
#creating scatter plot with petal length on the x axis and sepal length on the y axis
Scatterplot <- ggplot(iris, aes(x=Petal.Length, y=Sepal.Length)) +
#coloring points based on species
  geom_point(aes(color=Species)) 
#displaying plot
Scatterplot
