#script for MeasureCamp Bucharest, R-Ladies Bucharest, 2/11/2019

library(gtrendsR)
library(ggplot2)
#define the keywords
keywords=c("Simona Halep", "Mihaela Buzarnescu", "Bianca Alexandrescu")
#set the geographic area: 
country=c('RO')
#set the time window
time=("2019-10-01 2019-11-01")
#set channels 
channel='web'

trends = gtrends(keywords, gprop =channel,geo=country, time = time )
#select only interst over time 
time_trend=trends$interest_over_time
head(time_trend)


plot<-ggplot(data=time_trend, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ 
  theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+
  ggtitle("Google Search Volume")
plot

