install.packages("NPS")
library(dplyr)
library(NPS)
library(ggplot2)


x <- paper$c
y <- paper$n
p<-plot(y~x, main="Default Plot")
p<-plot(y~x, xaxt = "n", xlab='Some Letters')
axis(2, at=1:10, labels=letters[1:10])

p + scale_x_discrete(labels=c("-2" = "Dose 0.5", "2" = "Dose 1",
                              "8" = "Dose 2"))
ggplot(paper, aes(x=n, y=c)) + 
  geom_point() +
  geom_smooth(method=lm, color="black") +
  scale_x_continuous(labels = c("1000",
                                "2000",
                                "4000",
                                "8000",
                                "16000",
                                "32000",
                                "64000",
                                "125000",
                                "250000",
                                "500000",
                                "1000000",
                                "2000000",
                                "4000000"),
                     breaks = c(9.965784285,
                                10.96578428,
                                11.96578428,
                                12.96578428,
                                13.96578428,
                                14.96578428,
                                15.96578428,
                                16.93156857,
                                17.93156857,
                                18.93156857,
                                19.93156857,
                                20.93156857,
                                21.93156857)) +
  scale_y_continuous(labels = c("1", "2", "4", "8", "16","32","64","128" ,"256", "512","1024"),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(title="T vs N",
       x="N", y = "Time(ms)")+
  theme_classic()  
# Load the sheet
canarydata<-canaryreport...Sheet
canarydata2<-Canaryreport3...Sheet1

#convert into the  factor level
canarydata2$Grade.level.my.child.is.enrolled<- as.factor(canarydata2$Grade.level.my.child.is.enrolled)

#Group and arrange accordingly
dat<-df %>%
  group_by(canarydata$Grade.level.my.child.is.enrolled) %>%
  summarise(Tot=avg(canarydata$My.child.is.able.to.understand.the.concepts.taught.in.English, na.rm=TRUE))  %>%
  arrange(desc(Tot))

#Declare the variables and calculate NPS

df <- mutate_at(canarydata, vars(My.child.is.able.to.understand.the.concepts.taught.in.English), as.factor)
dat2<-aggregate(canarydata2[, 2:46], list(canarydata2$Grade.level.my.child.is.enrolled), nps)

nps(canarydata$My.child.is.able.to.understand.the.concepts.taught.in.English, breaks = list(0:6, 7:8, 9:10))
na.rm(canarydata$My.child.is.able.to.understand.the.concepts.taught.in.English)
drop_na(canarydata2)
canarydata2[is.na(x = canarydata2)] <- 8
getwd()

#mention the output file
write.csv(dat2, "/Users/vineeth/report2.csv")

early <- aggregate(nps$Score, list(nps$year), FUN = nps, nps$Score)

ggplot(dat) + 
  geom_col(aes(x=as.factor(canarydata$Grade.level.my.child.is.enrolled), y=x, fill=as.factor(Group.1)), width=0.5) +
  xlab("Year") + 
  ylab("NPS") + 
  geom_text(aes(x=as.factor(Group.1), y=x, label=round(x,2)),  vjust = 1, hjust = 1)  +
  coord_flip()  + 
  scale_fill_brewer("Year", palette = "Set1")
