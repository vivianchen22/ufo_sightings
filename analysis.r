#imoport data set
ufo <- read.csv("complete.csv")

#expolre data set
library("tidyverse")
library("lubridate")
install.packages("maps")
install.packages("mapdata")
head(ufo)
glimpse(ufo)
problems(ufo)

#fixed the data format
ufo$datetime <- mdy_hm(ufo$datetime)
ufo$date.posted <- mdy(ufo$date.posted)
ufo$duration..seconds. <- as.numeric(ufo$duration..seconds.)
ufo$country <-as.factor(ufo$country)
levels(ufo$country) <- c("No Record","Australia","Canada","Germany","United Kingdom","United State")
ufo$latitude <- as.double(ufo$latitude)

#UFO by Country.found that US are most likely to have UFO. 
ufo %>% ggplot(aes(country,fill=country))+geom_bar()+labs(title='UFO count by Country')+theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

#UFO by Shape. found that the most common type is Light.followed by Cicle and Triangle.
ufo %>% ggplot(aes(shape,fill=shape))+geom_bar()+labs(title='UFO count by Shape')+theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

#UFO map.
map <- borders("world", colour="black", fill="white") 
ufo_map <- ggplot(ufo) + map 
print(ufo_map + geom_point(aes(longitude,latitude,color=shape),shape=18) +
        theme(legend.position = "top")+
        ggtitle("UFOs"))

#UFP by duration. the mean is way larger than median. very right screwed. 
summary(ufo$duration..seconds.)
ufo %>% filter(duration..seconds.<8000) %>% ggplot(aes(duration..seconds.))+geom_density(fill="skyblue")+scale_x_log10()

#Time difference between time spotted and time posted.
diff_s <- as.numeric(difftime(ufo$date.posted,ufo$datetime,units="secs"))
summary(diff_s)
#trimed off the extreme value
diff_s <-summary(diff_s[diff_s>0 & diff_s <3.033e+08])
duration <- duration(num=as.numeric(diff_s),units="seconds")
cbind(diff_s, as.character(duration))
#in average, the difference between time posted & time spotted is 29 wks.

#UFO by year. in 2012, more UFO spotted event happened. 
ufo %>% ggplot(aes(year(datetime)))+geom_bar(fill="skyblue")+ggtitle("UFO by year spotted")+theme_bw()
#UFO by month.more UFO spotted event happened in Jul.
ufo %>% filter(!is.na(datetime)) %>% ggplot(aes(month(datetime,label = TRUE)))+geom_bar(fill="skyblue")+ labs(title="UFO by month spotted",x="Month")+theme_bw()
#UFO by weekday.more UFO spotted event happened in Sat. Followed by Sun.
ufo %>% filter(!is.na(datetime)) %>% ggplot(aes(wday(datetime,label = TRUE)))+geom_bar(fill="skyblue")+ggtitle("UFO by weekday spotted")+labs(x="Weekday")+theme(text=element_text(family="黑體-繁 中黑", size=14))

