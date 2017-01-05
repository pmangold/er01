nominal <- read.csv("nominal19.csv", header=TRUE, sep=",")
ppp <- read.csv("PPP19.csv", header=TRUE, sep=",")

library("ggplot2")
library("calibrate")

pppmoy<-(ppp[5]+ppp[6]+ppp[7])/3
nominalmoy<-(nominal[5]+nominal[6]+nominal[7])/3
name<-ppp[4]

df<-data.frame(x=pppmoy, y=nominalmoy)

colnames(df)[1] <- "x"
colnames(df)[2] <- "y"

ggplot(df,aes(x=x, y=y))+geom_point()+scale_x_log10()+scale_y_log10()+geom_text(aes(label=name,vjust=1.5))+geom_smooth(method='lm',formula=y~x)+xlab("PIB-Parité de pouvoir d'achat")+ylab("PIB Nominal")+labs(title="PIB Nominal = f(PIB PPA)")
ggsave("nominalppa.png")

upmoy=(ppp[2,5:15]+ppp[4,5:15]+ppp[6,5:15]+ppp[7,5:15]+ppp[10,5:15]+ppp[11,5:15]+ppp[18,5:15]+ppp[19,5:15])/(nominal[2,5:15]+nominal[4,5:15]+nominal[6,5:15]+nominal[7,5:15]+nominal[10,5:15]+nominal[11,5:15]+nominal[18,5:15]+nominal[19,5:15])
downmoy=(ppp[5,5:15]+ppp[8,5:15]+ppp[9,5:15]+ppp[13,5:15]+ppp[14,5:15])/(nominal[5,5:15]+nominal[8,5:15]+nominal[9,5:15]+nominal[13,5:15]+nominal[14,5:15])
time=data.frame(1990,2000,2007,2008,2009,2010,2011,2012,2013,2014,2015)

time = data.frame(t(time))
upmoy = data.frame(t(upmoy))
downmoy = data.frame(t(downmoy))

upmoy = 1/upmoy
downmoy = 1/downmoy

colnames(time)[1] <- "time"
colnames(upmoy)[1] <- "val"
colnames(downmoy)[1] <- "val"

ggplot() + geom_line(data = downmoy, aes(x=time$time, y=upmoy$val)) + xlab("Year") + ylab("Taux Nominal/PPA")
ggsave("upnominalppa.png")

ggplot() + geom_line(data = downmoy, aes(x=time$time, y=downmoy$val)) + xlab("Year") + ylab("Taux Nominal/PPA")
ggsave("downnominalppa.png")
