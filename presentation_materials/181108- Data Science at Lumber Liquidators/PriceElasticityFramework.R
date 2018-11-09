###########Code to run linear regression for Price Elasticity for each article in a list of articles#####

library(tidyverse)
library(data.table)
#Create a list of fake Article names. This is the same as a SKU number
sales_df<-data.frame(
 article=paste("A",seq(from=10000,to= 10100),sep="")
)
#Create a list of generated weeks in a year
weeks_df<-data.frame(
 week=paste("2017-",sprintf('%0.2d', 1:52),sep="") 
)
#Merge the articles with the weeks to create a new dataframe for modeling
sales_df<-merge(sales_df,weeks_df,all=TRUE)
#Generate a random number for the number of units sold each week
sales_df$units.sold<-  runif(nrow(sales_df), min=1000, max=10000)
#Generate a random sales price for each week
sales_df$price<-  runif(nrow(sales_df), min=1, max=5)
#My approach uses the z score to detect promotional events or outliers. This will calculate the z score (distance from mean) by week for each individual article
sales_df<-sales_df%>%group_by(article)%>%mutate(z.score=scale(units.sold, center = TRUE, scale = TRUE))
#If it is more than 1.5 standard deviations above the mean, consider it on promotion
sales_df$on.promo<-ifelse(sales_df$z.score>1.5,1,0)
#If it is more or less than 3 standard deviations from the mean, consider it an outlier
sales_df$outlier<-ifelse(sales_df$z.score>3 | sales_df$z.score  < -3,1,0)

#For each article in our dataframe, run a linear regression with Units Sold as the dependent variable, and our price and on.promo boolean as independent variables. We will also write certain parts of the model out to a dataframe for use later
models_promo_df<-sales_df%>%
  filter(outlier!=1)%>%
  group_by(article)%>%
  summarise(mod=list(lm(units.sold~price+on.promo)),
            intercept=lm(units.sold~price+on.promo)$coefficients["(Intercept)"],
            coeff.price=lm(units.sold~price+on.promo)$coefficients["price"],
            coeff.promo=lm(units.sold~price+on.promo)$coefficients["on.promo"],
            adj.r.squared=summary(lm(units.sold~price+on.promo))$adj.r.squared,
            r.squared=summary(lm(units.sold~price+on.promo))$r.squared,
            f.stat=summary(lm(units.sold~price+on.promo))$fstatistic["value"],
            df=summary(lm(units.sold~price+on.promo))$df[2],
            avg.price=round(mean(price),2),
            avg.units=mean(units.sold),
            wks.data=n_distinct(week),
            sd=sd(units.sold)
  )%>%#Use the coefficient for price, avg price, and avg units to calculate a Price Elasticity ratio. Calculate the lift to expect when something goes on promotion.
  mutate(PE=coeff.price*(avg.price/avg.units),lift.prom=coeff.promo/avg.units)

#Write everything in the new models dataframe to a csv file. Do not write the mod attribute, it will cause your R session to terminate!
fwrite(subset(models_promo_df, select=-c(mod)),paste("C:/Users/bkelley/Desktop/PriceElasticityModeling",Sys.Date(),".csv",sep=""))
