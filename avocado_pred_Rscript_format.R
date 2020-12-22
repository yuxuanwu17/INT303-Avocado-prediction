---
  title: "INT303 Final project"
author: "Yuxuan Wu 1716309"
date: "December 1, 2020"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Import the libraries
```{r，include=False}
library(tidyr)
library(skimr)
library(GGally)
library(viridis)
library(caret)
library(e1071)
library(rpart)
library(xgboost)
library(forecast)
library(corrplot)
library(corrgram)
library(ggplot2)
library(ggthemes)
library(psych)
library(scales)
library(treemap)
library(repr)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(plotrix)
library(ggrepel)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(tibbletime)
library(reshape2)
library(prophet)

```
### Load the data and return the head of data
```{r}
df <- read.csv("/Users/yuxuan/Desktop/INT303-Avocado-prediction/avocado-updated-2020.csv")
head(df)
colnames(df)
```
### Check whether the dataset contains the missing value
```{r}
sum(is.na(df))
```
The overall dataset do not contain any missing value

### Explore the data and some clarification

#### Explain the features
- date \- The date of the observation
- average_price \- The average price of a single
- total_volume \- Total number of avocados sold
- year \- The year
- type \- conventional or organic
- geography \- The city or region of the observation

#### X4046, X4225, X4770 stands for the PLU code

- Small/Medium Hass Avocado (~3-5oz avocado) | #4046 <br>
  - Large Hass Avocado (~8-10oz avocado) | #4225 <br>
  - Extra Large Hass Avocado (~10-15oz avocado) | #4770 <br>
  
  ### Exploratory Data Analysis
  
  #### Density plot of the difference between two avocados.
  ```{r}
levels(df$type)
```
```{r}
library(ggplot2)
options(repr.plot.width = 8, repr.plot.height = 4)
density_plot <- ggplot(df, aes(x=average_price, fill=type))+
  geom_density()+
  facet_wrap(~type)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  labs(title = "Avocado Price by type")+
  scale_fill_brewer(palette = "Set2")
density_plot
```

### Create a matrix to demonstrate the volume of conventional and organic avocados
```{r}
library(dplyr)
vol_type <- df %>% group_by(type) %>% summarise(average_volume = round(mean(total_volume),3),average_price = round(mean(average_price),3)) %>% mutate(volume_percent= round(prop.table(average_volume)*100,3))
vol_type

#plu_conv <- df %>% select(type,total_volume,X4046,X4225,X4770) %>% group_by(type) %>% summarise(average_volume = round(mean(total_volume),3),x4046 = sum(X4046),x4225 = sum(X4225),x4770 = sum(X4770)) %>% mutate(volume_percent= round(prop.table(average_volume)*100,3))
#plu_conv

#cor(df$average_price,df$total_volume)
```
**As can be seen from the density plot and the table in avocados.** <br>
  - there are two types of avocado: organic and conventional  <br>
  - organic avocado share a small percent (3.2%) of volume but has a high price (1.62)  <br>
  - conventional avocado share a large percent (96.8) of volume but has a relative low price (1.16) <br>
  
  ### Compare the volume of each avocado
  ```{r}
x4770 <- df$X4770 %>% sum()
x4046 <- df$X4046 %>% sum()
x4225 <- df$X4225 %>% sum()
total_types <- x4770+x4046+x4225
total_types
df$total_volume %>% sum()

```

The reason causing this difference is that there are different kinds of avocados, this dataset only consider the plu code of Hass avocados, it is obvious that it will cause the difference

### Avocado price with the Date

```{r ggplot}
library(ggplot2)
## Change the Date column from factor to the date format
df$date <- as.Date(df$date, "%Y-%m-%d")

## Sort the dates and order the datesets in date
df <- df[order(df$date),]

## Make the plot
comparision_plot <- df %>% select(date, average_price, type) %>%
  ggplot(aes(x=date,y=average_price))+
  geom_area(aes(color=type,fill=type),alpha=0.3,position=position_dodge(0.8))+
  theme_bw()+
  scale_color_manual(values = c("#ED7921","#62BE51"))+
  scale_fill_manual(values = c("#FD833E","#B8FC5F")
  )

comparision_plot
#plot_grid(density_plot,comparision,ncol = 2)

```

```{r}
ggplot(data=df, aes(x=date, y=average_price,col=type))+
  geom_line()+
  facet_wrap(~ type)+
  theme_bw()+
  theme(legend.position = "position")
```

### Relationship between Prices and Total on either conventional or organic avocados

#### Filter the data into two categories, conventional or organic

```{r}
organic <- df %>% select(type,average_price,total_volume,date) %>% filter(type=="organic")
#head(organic)
conventional <- df %>% select(type,average_price,total_volume,date) %>% filter(type=="conventional")
#head(conventional)
```

```{r tribble_tabel}
library(tibbletime)
organic <- as_tbl_time(organic,index = date) %>% as_period('1 month')
conventional <- as_tbl_time(conventional,index = date) %>% as_period('monthly')

```

#### Monthly avocados price in either conventional or organic avocados
```{r cowplot, fig.width = 10, fig.height = 8, fig.align = "center"}

library(ggplot2)
library(ggthemes)
library(cowplot)

options(repr.plot.width=8, repr.plot.height=6)

## average-price with time series
conventional_monthly <- conventional %>%
  ggplot(aes(x=date,y=average_price))+
  geom_line(color="#5C374C")+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill = "#D5D8DC"))+
  labs(title = "Conventional Avocados")+
  geom_hline(yintercept = max(conventional$average_price),linetype="dashed",color = "red")+
  geom_hline(yintercept = min(conventional$average_price),linetype="dashed",color = "blue")

organic_monthly <- organic %>%
  ggplot(aes(x=date,y=average_price))+
  geom_line(color="#58D68D")+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill = "#D5D8DC"))+
  labs(title = "Organic Avocados")+
  geom_hline(yintercept = max(organic$average_price),linetype="dashed",color = "red")+
  geom_hline(yintercept = min(organic$average_price),linetype="dashed",color = "blue")

## create a volume chart
conventional_volume <- conventional %>%
  ggplot(aes(x=date,y=total_volume))+
  geom_bar(stat = 'identity',fill="#7FB3D5",color="black")+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill = "#D5D8DC"))+
  geom_smooth(method = "loess",color="red")

organic_volume <- organic %>%
  ggplot(aes(x=date,y=total_volume))+
  geom_bar(stat = 'identity',fill='#58D68D',color="black")+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill = "#D5D8DC"))+
  geom_smooth(method = "loess",color ="red")

plot_grid(conventional_monthly,organic_monthly,conventional_volume,organic_volume,nrow = 2,ncol = 2,labels = c("A","B","C","D"))

#plot_grid(conventional_monthly,conventional_volume,nrow = 2)
```

- In order to find the seasonal patterns, I used the average price and volume in each month to conduct data analysis
- Figure A,B stands for the average price in each month (monthly)
- Blue dashed line stands for the minimum value while the red dashed value stands for the max value
- The most expensive conventional avocados in one month could 1.8 \$, the cheapest month of conventional avocados can be 0.82 \$
  - The most expensive organic avocados in one month could 2.1 \$, the cheapest month of conventional avocados can be 1.21 \$
  - Figure C,D stands for the volume in each month, the red line stands for the trend
- The love of American people are consistent, the sold volume continues to grow (conventional avocado)
- Regarding with the organic avocados, in 2019-2020, possibly owing to the decline of economic situation and the covid 19 pandemic after 2020
- The volume patterns could follow some seasonal patterns and need further analysis

### Patterns among the years in each month (Autoplot library)

```{r fig.height = 8, fig.width = 10, fig.align = "center"}
## Process the data into year and month format
library(forecast)
seasonal_df <- read.csv("/Users/yuxuan/Desktop/INT303-Avocado-prediction/avocado-updated-2020.csv")
seasonal_df$month_year <- format(as.Date(seasonal_df$date),"%Y-%m")
seasonal_df$month <- format(as.Date(seasonal_df$date),"%m")

## Change the month from a Date format into a numerical foramt, then convert to the three letter format
seasonal_df$monthabb <- sapply(seasonal_df$month, function (x) month.abb[as.numeric(x)])
seasonal_df$monthabb <- factor(seasonal_df$monthabb,levels=month.abb)
seasonal_df$monthabb <- factor(seasonal_df$monthabb)

## Set the figure size
options(repr.plot.width=10,repr.plot.height=8)

## Analyze the price by month

conv_price <- seasonal_df %>% select(type,year,monthabb,average_price) %>% filter(type=="conventional") %>% group_by(year,monthabb) %>% summarise(avg=mean(average_price))

org_price <- seasonal_df %>% select(type,year,monthabb,average_price) %>% filter(type=="organic") %>% group_by(year,monthabb) %>% summarise(avg=mean(average_price))

conv_price <- ts(conv_price$avg,start = 2015,frequency = 12)
org_price <- ts(org_price$avg,start = 2015,frequency = 12)

## Analyze the volume by month
conv_volume <- seasonal_df %>% select(type,year,monthabb,total_volume) %>% filter(type=="conventional") %>% group_by(year,monthabb) %>% summarise(avg=mean(total_volume))

org_volume <- seasonal_df %>% select(type,year,monthabb,total_volume) %>% filter(type=="organic") %>% group_by(year,monthabb) %>% summarise(avg=mean(total_volume))

conv_volume <- ts(conv_volume$avg,start = 2015,frequency = 12)
org_volume <- ts(org_volume$avg,start = 2015,frequency = 12)

byyearplot_price_conv <- ggseasonplot(conv_price,year.labels = TRUE,year.labels.left = TRUE)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Average conventional Avocados price \n by year for each month", y="Average Price")+
  scale_fill_manual(values = c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676"))

byyearplot_price_org <- ggseasonplot(org_price,year.labels = TRUE,year.labels.left = TRUE)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Average organic Avocados price \n by year for each month", y="Average Price")+
  scale_fill_manual(values = c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676"))

byyearplot_volume_conv <- ggseasonplot(conv_volume,year.labels = TRUE,year.labels.left = TRUE)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Average conventional Avocados volume \n by year for each month", y="Average volume")+
  scale_fill_manual(values = c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676"))

byyearplot_volume_org <- ggseasonplot(org_volume,year.labels = TRUE,year.labels.left = TRUE)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Average organic Avocados volume by year \n for each month", y="Average volume")+
  scale_fill_manual(values = c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676"))

plot_grid(byyearplot_price_conv,byyearplot_price_org,byyearplot_volume_conv,byyearplot_volume_org,nrow = 2,ncol = 2,labels = c("A","B","C","D"))
```

### Seasonal patterns analysis
```{r}
ggplot(seasonal_df,aes(x=average_price,fill=as.factor(year)))+
  geom_density(alpha=0.5)+
  theme_economist()+
  facet_wrap(~year)+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  guides(fill=FALSE)+
  labs(title = "Distribution of Prices by year",x='Average Price',y='Density')+
  scale_fill_manual(values = c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1"))

```

```{r}
library(ggplot2)
ggplot(seasonal_df,aes(x=total_volume,fill=as.factor(year)))+
  geom_density(alpha=0.5)+
  theme_economist()+
  facet_wrap(~year)+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  guides(fill=FALSE)+
  labs(title = "Distribution of Prices by year",x='Average Price',y='Density')+
  scale_fill_manual(values = c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1"))
```

### Seasonality patterns

#### Monthly analysis
```{r seaonal patterns, fig.align = "center", fig.width = 8, fig.height = 10}

options(repr.plot.width=10,repr.plot.height=8)
conv_patterns <- seasonal_df %>% select(monthabb,average_price,type) %>% filter(type=="conventional") %>% group_by(monthabb) %>% summarise(avg=mean(average_price)) %>%
  ggplot(aes(x=monthabb, y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Conventional Avocados",x="Month",y="Average Price")

organic_patterns <- seasonal_df %>% select(monthabb,average_price,type) %>% filter(type=="organic") %>% group_by(monthabb) %>% summarise(avg=mean(average_price)) %>%
  ggplot(aes(x=monthabb,y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Organic Avocados",x="Month",y="Average Price")

whole_patterns <- seasonal_df %>% select(monthabb,average_price,type) %>% group_by(monthabb) %>% summarise(avg=mean(average_price)) %>%
  ggplot(aes(x=monthabb,y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "All Avocados",x="Month",y="Average Price")


plot_grid(conv_patterns,organic_patterns,whole_patterns,nrow = 3)
#conv_patterns
```

```{r}
conv_patterns_vol <- seasonal_df %>% select(monthabb,total_volume,type) %>% filter(type=="conventional") %>% group_by(monthabb) %>% summarise(avg=mean(total_volume)) %>%
  ggplot(aes(x=monthabb, y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Conventional Avocados volume",x="Month",y="Average Price")

organic_patterns_vol <- seasonal_df %>% select(monthabb,total_volume,type) %>% filter(type=="organic") %>% group_by(monthabb) %>% summarise(avg=mean(total_volume)) %>%
  ggplot(aes(x=monthabb,y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "Organic Avocados volume",x="Month",y="Average Price")

whole_patterns_vol <- seasonal_df %>% select(monthabb,total_volume,type) %>% group_by(monthabb) %>% summarise(avg=mean(total_volume)) %>%
  ggplot(aes(x=monthabb,y=avg))+
  geom_point(color="#F35D5D",aes(size=avg))+
  geom_line(group=0)+
  theme_economist()+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#D5D8DC"))+
  labs(title = "All Avocados volume",x="Month",y="Average Price")


plot_grid(conv_patterns_vol,organic_patterns_vol,whole_patterns_vol,nrow = 3)

```

- Overall, the avocado price could reach the peak of the year during Sep and Oct, and Feb could be the lowest price
- Regarding with the volume avocado sold, American people like to buy avocado at Feb and May, but not at Nov
- Based on the research, avocado tend to ripe at Aug and Sep, plus the time in transportation and packaging, it is unavoidable that it could have some delay
- From the plot we could see that the trend in Aug are positive
- We could see that the sold volume and price demonstrated some negative correlation at some time, which obeys our common sense, people like buying staffs when their price are low
- Meanwhile, some positive correlation could be discovered between price and volumes, the hypothesis is that the newly harvest avocados are definitely in high quality and the previous stored avocados could be consumed significantly, which means there is a gap between demand and output
- The volumes are increasing with the time which possibly because of the widely broadcasting.



#### Seasonal patterns
```{r fig.height = 8, fig.width = 10, fig.align = "center" }
options(repr.plot.width=10,repr.plot.height=8)

## seperate the month into four seasons
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03","04","05"),"Spring",
                             ifelse(seasonal_df$month %in% c("06","07","08"),"Summer",
                                    ifelse(seasonal_df$month %in% c("09","10","11"),"Autumn","Winter")))

## Prepare to analyze the results
seasonality_plot_conventional_price <- seasonal_df %>% select(season,year,average_price,type) %>% filter(type =="conventional") %>% group_by(season,year) %>% summarise(avg=mean(average_price)) %>%
  ggplot(aes(x=season,y=avg,color=season))+
  geom_segment(aes(x=season,xend=season,y=0,yend=avg),show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~as.factor(year))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Conventional Avocados average price by Season",x="",y="Average price")+
  geom_text(aes(x=season,y=0.01,label=paste0("$ ",round(avg,2))),hjust=-0.5,vjust=-0.5,size=4,color="black",fontface='italic',angle=360)

seasonality_plot_conventional_volume <- seasonal_df %>% select(season,year,total_volume,type) %>% filter(type=="conventional") %>% group_by(season,year) %>% summarise(avg=round(mean(total_volume/1000000),2)) %>%
  ggplot(aes(x=season,y=avg,color=season))+
  geom_segment(aes(x=season,xend=season,y=0,yend=avg),show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~as.factor(year))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Conventional Avocados total volume by Season",x="",y="Average volume")+
  geom_text(aes(x=season,y=0.01,label=paste0(avg," m")),hjust=-0.5,vjust=-0.5,size=4,color="black",fontface='italic',angle=360)

#plot_grid(seasonality_plot_conventional_price,seasonality_plot_conventional_volume,nrow = 2)

seasonality_plot_organic_price <- seasonal_df %>% select(season,year,average_price,type) %>% filter(type =="organic") %>% group_by(season,year) %>% summarise(avg=mean(average_price)) %>%
  ggplot(aes(x=season,y=avg,color=season))+
  geom_segment(aes(x=season,xend=season,y=0,yend=avg),show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~as.factor(year))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Organic Avocados average price by Season",x="",y="Average price")+
  geom_text(aes(x=season,y=0.01,label=paste0("$ ",round(avg,2))),hjust=-0.5,vjust=-0.5,size=4,color="black",fontface='italic',angle=360)

seasonality_plot_organic_volume <- seasonal_df %>% select(season,year,total_volume,type) %>% filter(type=="organic") %>% group_by(season,year) %>% summarise(avg=round(mean(total_volume/1000000),2)) %>%
  ggplot(aes(x=season,y=avg,color=season))+
  geom_segment(aes(x=season,xend=season,y=0,yend=avg),show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~as.factor(year))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Organic Avocados total volume by Season",x="",y="Average volume")+
  geom_text(aes(x=season,y=0.01,label=paste0(avg," m")),hjust=-0.5,vjust=-0.5,size=4,color="black",fontface='italic',angle=360)

plot_grid(seasonality_plot_conventional_price,seasonality_plot_organic_price,seasonality_plot_conventional_volume,seasonality_plot_organic_volume,nrow = 2,ncol = 2,labels = c("A","B","C","D"))

```

- Spring (3-5), Summer (6-8), Autumn (9-11), Winter (12-2)
- Figure A, B stands for the average price of avocados for either conventional or organic
- Figure C, D stands for the volume sold for either conventional or organic, the unit is million (m)
- In summary:
  - it is relative cheaper to avocado in Spring or Winter
- avocado most sold in Spring and Summer since it is close to the next round of ripen in avocado and the market are in great supply of avocados


### Find the city where avocado's price is lowest
```{r}
library(forcats)
geo_conv_price <- seasonal_df %>% select(geography,average_price,type) %>% filter(type=="conventional") %>% group_by(geography)%>%  summarise(avg=round(mean(average_price),2)) %>% arrange(avg) %>% slice(1:6)  %>% mutate(geography=fct_reorder(geography,desc(avg))) %>%
  ggplot(aes(x=geography,y=avg))+
  geom_bar(stat = "identity",position = "dodge",alpha=.6,width =.4,show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label=avg),vjust=1.5,color='black',size=5)+
  theme_minimal()+
  theme(title = element_text(hjust = 0.6),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Conventional Avocados average price by geography",x="Geography place",y="Average Price")

geo_org_price <- seasonal_df %>% select(geography,average_price,type) %>% filter(type=="organic") %>% group_by(geography)%>%  summarise(avg=round(mean(average_price),2)) %>% arrange(avg) %>% slice(1:6) %>% mutate(geography=fct_reorder(geography,desc(avg))) %>%
  ggplot(aes(x=geography,y=avg))+
  geom_bar(stat = "identity",position = "dodge",alpha=.6,width =.4,show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label=avg),vjust=1.5,color='black',size=5)+
  theme_minimal()+
  theme(title = element_text(hjust = 0.6),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Organic Avocados average price by geography",x="Geography place",y="Average Price")

plot_grid(geo_conv_price,geo_org_price,nrow = 2)

```

- Use the calculation to return the six cities or regions whether the average price for avocados is lowest
- Based on the research from map, Top 5 is close to the Mexico, where the avocado originated
- These places are possibly the region to supply avocado


### The future price prediction
```{r}
library(prophet)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(devtools)
## Select and filter the data and fit the model
price_predict <- df %>% select(date,average_price) %>% group_by(date) %>% summarise(avg=mean(average_price))
colnames(price_predict) <- c('ds','y')
model <- prophet(price_predict,daily.seasonality = TRUE)

## make prediction or forecast the results
future <- make_future_dataframe(model,periods = 365)
forecast <- predict(model,future)
plot(model,forecast)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect(fill="#F4F6F7"))+
  labs(title = "Avocados price prediction in next year",x="year",y="Average Price")

trend <- prophet_plot_components(model,forecast)

year_trend <- trend[1]
month_trend <- trend[3]
week_trend <- trend[2]
time_trend <- trend[4]

overall_trend <- c(year_trend,month_trend,week_trend,time_trend)
ggarrange(plotlist = overall_trend, labels = c('A', 'B','C','D'))

```

- Figure A is analyze based on year: Since 2015, the average price of avocados continues to grow and 2017 was a crazy year, they grow in a speed higher than 1.3; After 2018, the average price tend to be stable, but still has a rate about 1.28; Regarding to the future prediction, the range is about [1.20-1.34]. But considering the pandemic in North America, the possible rate could be 1.20
- Figure B is the analysis based on month: we could see that during May, the price tend to grow in a positive way and this trend continues to grow to Oct; After Oct, the price starts to decrease, which follows the previous analysis
- Figure C is the analysis based on week. The average price follow the rule that ordinary family shopping habits, they prefer to buy things during weekends, so the price during weekends could be higher in other days
































