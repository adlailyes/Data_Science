library('remotes')
remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE)
library('nCov2019')
require(dplyr)
x <- get_nCov2019(lang = 'en')
y <- load_nCov2019()
x
y
summary(x)
x['global',]
library(ggplot2)
require(ggrepel)
library(ggrepel)
plot(x)
d <- summary(x)
ggplot(d,aes(as.Date(date, "%m.%d"), as.numeric(confirm))) +geom_col(fill = 'firebrick') +theme_minimal(base_size = 14) +xlab(NULL) + ylab(NULL) +scale_x_date(date_labels = "%Y/%m/%d") +labs(caption = paste("accessed date:", time(x)))

install.packages("DataExplorer")
library(DataExplorer)
plot_str(x)
plot_str(y)
summary(x['global',])
d <- y['global']
d <- d[d$country == "Algeria",]
d
ggplot(filter(d,  d$time > "2020/02/15"),aes(time, cum_confirm, color=country)) +geom_line() +geom_text_repel(aes(label=country),function(d) d[d$time == time(y),]) +theme_minimal(base_size=14) 
ggplot(filter(d,  d$time > "2020/02/15"),aes(time, cum_dead, color=country)) +geom_line() +geom_text_repel(aes(label=country),function(d) d[d$time == time(y),]) +theme_minimal(base_size=14) 

d <- y['global']
d
n <- d %>% filter(time == time(y)) %>% top_n(10, cum_confirm) %>% arrange(desc(cum_confirm))
n
ggplot(filter(d, country %in% n$country, d$time > "2020/02/15"),aes(time, cum_confirm, color=country)) +geom_line() +geom_text_repel(aes(label=country),function(d) d[d$time == time(y),]) +theme_minimal(base_size=14) 



usdata <- d %>% filter(d$country == "United States" & d$cum_confirm>100) %>% select(time,cum_confirm)
install.packages("forecast")
library(forecast)
case <- ts(usdata[,2], start=1,frequency = 1)
fit <- tslm(log(case) ~ trend)
summary(fit)
fc <- forecast(fit, h=10)
plot(fc)

#le cas de l'algerie
d <- y['global']
aldata <- d %>% filter(d$country == "Algeria" & d$cum_confirm>100) %>% select(time,cum_confirm)
case <- ts(usdata[,2], start=1,frequency = 1)
fit <- tslm(log(case) ~ trend)
summary(fit)
fc <- forecast(fit, h=10)
plot(fc)

#la question sur l'age

library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(lubridate)

#reéxecuter les library si les commandes suivantes ne marchent pas
data2 <- read.csv(file.choose(),header = TRUE)
data2$age <- as.numeric(data2$age)
data2 <- data2[between(data2$age, 2, 100),]
data2 <- data2[,c(1,2,3,6)]



data2$Group <- 0
data2$Group[data2$country=="Austria"]<-"Europe"
data2$Group[data2$Country=="Albania"]<-"Europe"
data2$Group[data2$Country=="Armenia"]<-"Europe"
data2$Group[data2$country=="Belgium"]<-"Europe"
data2$Group[data2$country=="Bulgaria"]<-"Europe"
data2$Group[data2$country=="Croatia"]<-"Europe"
data2$Group[data2$country=="Czech Republic"]<-"Europe"
data2$Group[data2$country=="Denmark"]<-"Europe"
data2$Group[data2$country=="Estonia"]<-"Europe"
data2$Group[data2$country=="Finland"]<-"Europe"
data2$Group[data2$country=="France"]<-"Europe"
data2$Group[data2$country=="Germany"]<-"Europe"
data2$Group[data2$country=="Greece"]<-"Europe"
data2$Group[data2$country=="Hungary"]<-"Europe"
data2$Group[data2$Country=="Iceland"]<-"Europe"
data2$Group[data2$country=="Ireland"]<-"Europe"
data2$Group[data2$country=="Italy"]<-"Europe"
data2$Group[data2$country=="Lativia"]<-"Europe"
data2$Group[data2$country=="Lithuania"]<-"Europe"
data2$Group[data2$country=="Netherlands"]<-"Europe"
data2$Group[data2$country=="Poland"]<-"Europe"
data2$Group[data2$country=="Portugal"]<-"Europe"
data2$Group[data2$country=="Romania"]<-"Europe"
data2$Group[data2$country=="Slovakia"]<-"Europe"
data2$Group[data2$country=="Slovenia"]<-"Europe"
data2$Group[data2$country=="Spain"]<-"Europe"
data2$Group[data2$country=="Sweden"]<-"Europe"
data2$Group[data2$country=="UK"]<-"Europe"
data2$Group[data2$country=="Russia"]<-"Europe"
data2$Group[data2$country=="Switzerland"]<-"Europe"
data2$Group[data2$country=="Ukraine"]<-"Europe"
data2$Group[data2$country=="Belarus"]<-"Europe"
data2$Group[data2$country=="Norway"]<-"Europe"
data2$Group[data2$Country=="San Marino"]<-"Europe"
data2$Group[data2$Country=="Moldova"]<-"Europe"
data2$Group[data2$Country=="Malta"]<-"Europe"
data2$Group[data2$Country=="Liechtenstein"]<-"Europe"
data2$Group[data2$Country=="Luxembourg"]<-"Europe"
data2$Group[data2$country=="China"]<-"East Asia"
data2$Group[data2$country=="Mainland China"]<-"East Asia"
data2$Group[data2$country=="Japan"]<-"East Asia"
data2$Group[data2$country=="South Korea"]<-"East Asia"
data2$Group[data2$country=="Mongolia"]<-"East Asia"

data2$Group[data2$Group==0] <- "Rest of World"
head2 <- data2[sample(1:nrow(data2),5), ]
head2 <- head2[order(head2$ID),]
head2
ggplot(data2, aes(age, fill = Group))+ geom_density(alpha = 0.75, position = "stack", size = 0.8, show.legend = T, col = "gray25")+ scale_x_continuous(limits = c(0,110), breaks = seq(0,110,15))+ scale_fill_brewer(palette = "Set1")+ labs(title = "Age of infected", subtitle = "by East Asia, Europe and Rest of World", y = "Density", x = "Age", fill = "")+ theme_fivethirtyeight()+ theme(legend.position="bottom", legend.direction="horizontal", axis.text = element_text(size = 14), axis.title = element_text(size = 15), legend.text = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"), plot.background = element_rect(fill = "#C8EDAF"), legend.background = element_rect(fill = "#C8EDAF"))

to_tile <- data2 %>% group_by(sex, Group) %>% summarise(Count = n())
to_tile <- to_tile[c(4:6,8:10),]
ggplot(to_tile, aes(sex, Group)) +geom_tile(aes(fill = Count), colour = "black", alpha = 0.9)+geom_text(aes(label = paste0(round(Count/sum(to_tile$Count) * 100,1), "%")), size = 6.8)+scale_fill_distiller(palette = "Spectral")+labs(title = "Gender of infected (only for cases where is known!)", subtitle = "by East Asia, Europe and Rest of World", y = "", x = "", fill = "")+theme_fivethirtyeight()+theme(legend.position="none", axis.text = element_text(size = 14), axis.title = element_text(size = 15),legend.text = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"), title = element_text(size = 10),plot.background = element_rect(fill = "#C8EDAF"), legend.background = element_rect(fill = "#C8EDAF"))
