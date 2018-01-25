
### Setting the work directory and loading in the train data 

```{r}
setwd('F:/Anuj/Study & Work/Sem 2 Courses/INST 737/Final Project')
wineWhites <- read.csv('wineQualityWhites.csv',sep=',') 

```

### Splitting the data into train and test datasets

```{r}
library(caret)
trainIndex <- createDataPartition(wineWhites$quality, p=0.80, list = FALSE)
wine_Train <- wineWhites[ trainIndex,]
wine_Test  <- wineWhites[-trainIndex,] 

```


### Preliminary exploratory analysis

### Summary of the dataset
```{r}
dim(wineWhites)

names(wineWhites)

str(wineWhites)

summary(wineWhites)

```


### Observations from the summary

-> The amount of citric acid in the wine varies mostly between 0 and 1.0.

-> 75% of the wines have residual sugar content less than 9.9 in them
but there are a few outliers whose residual sugar content can go right 
upto 65.8.

-> The ingredients which are used in least amounts are sulphates, chlorides, citric acids and volatile acids.  

-> The quality with the given dataset features mostly hovers between 3 to 8, with the Mean being 6.0.

From the given summary results we have a few quantifiable results but none of them are leading us to any kind of causation yet. In order to surge ahead in that direction, we will need to explore the variables(ingredients) individually in univariate, bi-variate and multi-variate styles.   

### Understanding the distribution of single variables

### Fixed acidity

```{r}

library(ggplot2)
qplot(data = wine_Train, fixed.acidity)

```

Lets just refine our plot a bit

```{r}

qplot( data = wine_Train, 
       fixed.acidity, 
       binwidth = 0.3,
       fill = I('#099DD9'), 
       color = I('black')
) +  
  scale_x_continuous(breaks = seq(0,16,1), limits = c(0,16))  

```

This looks good

```{r}
min(wine_Train$fixed.acidity) 
max(wine_Train$fixed.acidity)

```

Conclusion - Quite big groups of the wine samples have a fixed acidity between 6 to 10.0. There is no sample having a fixed acidity of 0, in fact the least fixed acidity is 3.8 and the highest is 14.2. 

```{r}
max_fa_wine_Train <- subset(wine_Train, 
                            wine_Train$fixed.acidity >= 6.0 &  wine_Train$fixed.acidity <= 10.0)  

nrow(max_fa_wine_Train) 
nrow(wine_Train)

```

About 88% of the samples have a fixed acidity between 6.0 to 10.0.  


Volatile acidity 

```{r}

max(wine_Train$volatile.acidity)
min(wine_Train$volatile.acidity)

qplot(data = wine_Train, volatile.acidity)

qplot(data = wine_Train, 
      volatile.acidity,
      color = I('black'), 
      fill = I('#099DD9'),
      binwidth = 0.03
) +  
  scale_x_continuous(breaks = seq(0,2,0.1)) 

```

Conclusion - Majorityof the wine samples have a volatile acidity between 0.1  to 0.5. There is no sample having a volatile acidity of 0. There are very few samples having volatile acidity above 1. Most of the samples have a volatile acidity less than 1.0.  

Verifying our claims about volatile acidity. 

```{r}
max_va_wine_Train <- subset(wine_Train, 
                            wine_Train$volatile.acidity >= 0.1 &  wine_Train$volatile.acidity <= 0.5) 

nrow(max_va_wine_Train) 
nrow(wine_Train)

```

The above figures verify our claims. About 96% of the wine samples have a volatile acidity between 0.1  to 0.5. 

### Faceting

Analysing the residual sugar content in the wines faceted by quality. This would give us a fair idea about the distribution of residual sugar content across the different quality levels. 

```{r} 
qplot( data = wine_Train,
       x = residual.sugar,
       binwidth = 0.9,
       color = I('black'),
       fill = I('#099DD9')
) +  
  scale_x_continuous(breaks = seq(0,50,5)) +
  facet_wrap(~quality) 

max(wine_Train$residual.sugar)

```

The distribution of residual sugar is prominently seen in the facets showing quality 5 ,6,7. Most of these wines (having quality 5 and 6) have  residual sugar content under 20.   

Exploring free sulphur dioxide.  

```{r}
qplot1 <- qplot( data = wine_Train,
                 x = free.sulfur.dioxide,
                 color = I('black'),
                 fill = I('#099DD9')
) + 
  scale_x_continuous(breaks = seq(0,100,5),limits = c(0,100))+ 
  scale_y_continuous(breaks = seq(0,600,100), limits = c(0,600))  

```

Majority of the samples have a free sulfur dioxide content under 30. 

Now we are seeing a long tail here after free sulfur dioxide goes beyond 40. Lets add a log transformation to our code to address this issue.

```{r}
library(scales)

qplot2 <- qplot( data = wine_Train,
                 x = free.sulfur.dioxide,
                 color = I('black'),
                 fill = I('#099DD9')
) + 
  scale_x_continuous(breaks = seq(1,70,7),trans = log10_trans()) + 
  scale_y_continuous(breaks = seq(0,600,100), limits = c(0,600))
  
```


### Using gridExtra for comparing with and without log transformation

```{r}
install.packages('gridExtra')
library(gridExtra)

grid.arrange(qplot1,qplot2,ncol = 2)

```



Lets try the same variable with frequency polygon

### Frequency polygon

```{r}

qplot( data = wine_Train,
       x = free.sulfur.dioxide,
       geom = 'freqpoly',
) + 
  scale_x_continuous(breaks = seq(0,75,5),limits = c(0,75))

```


### Box Plots

Lets try to check out the quality of the samples against the alcohol content. 

```{r}

qplot(data = wine_Train,
      x = quality,
      y = alcohol,
      geom = 'boxplot',
)

``` 

Here we will need to factor the variable quality first

### Factorisation   

```{r}

wine_Train$qualityfact <- factor(wine_Train$quality,
                                 levels = c('1','2','3','4','5','6','7','8','9','10')
)  

library(RColorBrewer)

qplot(data = wine_Train,
      x = qualityfact,
      y = alcohol,
      geom = 'boxplot',
      binwidth = 0.1,
      fill = factor(quality),
      xlab = "quality"
) + 
  scale_fill_brewer(type = "qual")



```


Conclusion : Median alcohol content is highest for the samples with quality 9  


Impact of density on quality of the samples

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,density, fill = qualityfact)    
) +
  geom_boxplot() +
  xlab("Quality")


```


Impact of sulphate proportion on the quality of the samples

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,sulphates, fill = qualityfact)     
) +
  geom_boxplot() +
  xlab("Quality")

```

What we see here is that as the quality goes on increasing the median content of sulphates goes on increasing. 

### Bivariate analysis using ggplot syntax

Fixed Acidity Vs Quality

```{r}

ggplot( data = wine_Train,
        aes(qualityfact, fixed.acidity, fill = qualityfact),
) +
  geom_boxplot() + 
  xlab("Quality")  

```


From the above box-plots, it is clear that fixed acidity remains fairly constant over all the quality levels.


Volatile acidity VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, volatile.acidity, fill = qualityfact),
) +
  geom_boxplot() + 
  xlab("Quality")

```

As the volatile acidity decreases the quality of the wine goes on increasing. 

Citric acid VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, citric.acid, fill = qualityfact),
) +
  geom_boxplot() + 
  xlab("Quality")  
```

The quality of the redwines tends to have an increasing trend with an increase in citric acid.  

Residual sugar VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, residual.sugar, fill = qualityfact), 
) +
  geom_boxplot() + 
  xlab("Quality") + 
  scale_y_continuous(breaks = seq(0,16,1))

```

Here , we can guess that the residual sugar is more or less at the same level but 
the output is kinda squished because of the large number of outliers. Lets bring the focus on the box plots.  

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, residual.sugar, fill = qualityfact), 
) +
  geom_boxplot() + 
  xlab("Quality") + 
  scale_y_continuous(breaks = seq(0,16,1), limits = c(0,4))

```

The above box-plots show that, the amount of residual sugar remains fairly constant through all the quality levels.  

Chlorides VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, chlorides, fill = qualityfact)
) + 
  geom_boxplot() + 
  xlab("Quality") + 
  scale_y_continuous(breaks = seq(0,0.2,0.01), limits = c(0,0.2))


```

The above box-plots show that, the amount of chlorides remains fairly constant through all the quality levels.  

Free sulphur dioxide VS Quality 

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,free.sulfur.dioxide, fill = qualityfact)
) +  
  geom_boxplot() + 
  xlab("Quality")

```

The amount of free sulphur dioxide varies across different quality levels.  

Total Sulphur dioxide VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,total.sulfur.dioxide, fill = qualityfact)
) +  
  geom_boxplot() +  
  xlab("Quality")


```

The trend of total sulphur dioxide is very similar to that of free sulphur dioxide w.r.t quality.  


Density VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,density, fill = qualityfact)
) +  
  geom_boxplot() +  
  xlab("Quality") 

```

The density gradually decreases, as the quality goes on increasing. 

pH vs Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,pH, fill = qualityfact)
) +  
  geom_boxplot() +  
  xlab("Quality") 


```

sulphates VS Quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact, sulphates, fill = qualityfact)
) +  
  geom_boxplot() +  
  xlab("Quality")   
```

With a steady increase in quality, increase in sulphates.   

alcohol VS quality 

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,alcohol,fill = qualityfact)
) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0,16,1)) + 
  xlab("Quality") 

```

Volatile Acidity VS quality

```{r}
ggplot( data = wine_Train,
        aes(qualityfact,volatile.acidity,fill = qualityfact)
) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0,16,1)) + 
  xlab("Quality") 

```


We will have to test the interdependency between the factors affecting the quality of the redwine. A correlation coefficient matrix would come in handy for that purpose.  

```{r}
install.packages('GGally')
library(GGally)
set.seed(1000)
good_quality_subset <- good_quality[,c(2:12)]  

ggpairs(wine_Train[sample.int(nrow(wine_Train), ), ], 
        title = "Interdependency between the ingredients"
)   

```

The following pairs of ingredients have a relatively strong correlation 
- Density and total.sulfur.dioxide (+ve)
- Residual sugar and Density (+ve)
- Alcohol and Density(-ve)
- Free Sulfur dioxide and Total Sulfur dioxide(+ve) 

```{r}
library(ggplot2)
plot1 <- ggplot( data = wine_Train,
                 aes(density, total.sulfur.dioxide, color = qualityfact),
) + 
  geom_point()

```

```{r}
linearModel1  <- lm(wine_Train$quality ~ wine_Train$density + wine_Train$total.sulfur.dioxide)

firstplot <- ggplot( data = wine_Train,
                     aes(x = density, y = total.sulfur.dioxide)
) + 
  geom_point()

p1 <- firstplot + geom_smooth(method = "lm", formula = y~x) + ggtitle('Linear Model for Total Sulfur dioxide VS Density')

summary(linearModel1)

```


Here again, we see a similar pattern of linear dependency between fixed acidity and density.

```{r}
linearModel2 <- lm(wine_Train$quality ~ wine_Train$density + wine_Train$residual.sugar)

secondplot <- ggplot( data = wine_Train,
                      aes(x = residual.sugar, y = density)
) + 
  geom_point()

p2 <- secondplot + geom_smooth(method = "lm", formula = y~x) + ggtitle('Linear Model for Fixed acidity VS Density')

summary(linearModel2)

```


Alcohol VS Density

```{r}
plot2 <- ggplot(data = wine_Train,
                aes(alcohol, density, color = qualityfact)
) + 
  geom_point()

```

Now here is another interesting trend. We see a linear dependency between alcohol and density. But it is a negative linear dependency.

```{r}

linearModel3 <- lm(wine_Train$quality ~ wine_Train$alcohol + wine_Train$density)

thirdplot <- ggplot( data = wine_Train,
                     aes(x = alcohol, y = density)
) + 
  geom_point()

p3 <- thirdplot + geom_smooth(method = "lm", formula = y~x) + ggtitle('Linear Model for Alcohol VS density')

summary(linearModel3)

```


Free SO2 vs Total SO2

```{r}

plot3 <- ggplot( data = wine_Train,
                 aes(total.sulfur.dioxide,free.sulfur.dioxide,color = qualityfact)
) + 
  geom_point()

```


```{r}
linearModel4 <- lm(wine_Train$quality ~ wine_Train$total.sulfur.dioxide + wine_Train$free.sulfur.dioxide) 

fourthplot <- ggplot( data = wine_Train,
                      aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)
) + 
  geom_point()

p4 <- fourthplot + geom_smooth(method = "lm", formula = y~x) + ggtitle('Linear Model for Free Sulfur dioxide VS Total Sulfur dioxide')


summary(linearModel4)
```


### Linear Model Analysis

```{r}
library(gridExtra)
grid.arrange(p1,p2,p3,p4,ncol = 2) 

```







