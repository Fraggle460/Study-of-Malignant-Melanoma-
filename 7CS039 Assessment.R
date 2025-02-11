attach(melanoma)
library(melanoma)
melanoma
mean(melanoma$time)
mean(melanoma$status)
mean(melanoma$sex)
mean(melanoma$age)
mean(melanoma$year)
mean(melanoma$thickness)
mean(melanoma$ulcer)
summary(melanoma)


help(hist)
melanoma$sex=factor(melanoma$sex)
melanoma$ulcer=factor(melanoma$ulcer)
melanoma$status=factor(melanoma$status)
melanoma$year=factor(melanoma$year)
hist(melanoma$time,main="Time")
xlim(-100,100)

hist(melanoma$status)
hist(melanoma$sex)

hist(melanoma$age,col="red")


hist(melanoma$time,col="lightyellow")
hist(melanoma$age)
hist(melanoma$year,col="green")
hist(melanoma$thickness,col="green")
par(mfrow=c(1,1))
attach(melanoma)
cor(time,thickness,method="pearson")
plot(time,thickness,main="Scatterplot:Time & Thickness")
cor(time,age,method="pearson")
plot(time,age)
cor(thickness,age,method="pearson")
plot(thickness,age)
plot(time,age,main="Scatterplot:Time & Age")
plot(thickness,age,main="Scatterplot:Thickness & Age")
my_model=lm(formula=time~thickness)
summary(my_model)
my_model=lm(formula=time~age)
summary(my_model)
my_model
summary(my_model)
my_model=lm(formula=thickness~age)
linreg<-lm(time~age)
abline(linreg,col="green",lwd=2)
summary(linreg)
abline(linreg,col="blue",lwd=2)


library(melanoma)
head(melanoma)
library(tidyverse)
melanoma<-as_tibble(MASS::melanoma)
melanoma<-melanoma%>%
  mutate(status=recode_factor(status,'1'="melanoma death",'2'="alive",'3'="unrelated death"))%>%
  mutate(sex=recode_factor(sex,'0'="female",'1'="male"))%>%
  mutate(ulcer=recode_factor(ulcer,'0'="absent",'1'="present"))
  
qplot(x=sex,y=time,
      geom="boxplot",data=melanoma,
      xlab="Gender", 
      ylab="Survival Time (in Days) since operation",
      fill=I("green"))
qplot(x=sex,y=thickness,
      geom="boxplot",data=melanoma,
      xlab="Gender", 
      ylab="Tumor thickness (mm)",
      fill=I("pink"))
qplot(x=sex,y=age,
      geom="boxplot",data=melanoma,
      xlab="Gender", 
      ylab="Age in years at the time of operation",
      fill=I("yellow"))

head(melanoma)
yes<-filter(melanoma,sex=='yes')
yes
mean(sex$age)
melanoma%>%
  group_by(sex)%>%
  summarize(mean_age=mean(age),
            sd_age=sd(age))
melanoma%>%
  group_by(sex)%>%
  summarize(num.obs=n(),
            mean_age=round(mean(age),0),
            sd_age=round(sd(age),0),
            se_age=round(sd(age)/sqrt(num.obs),0))


age_t_test<-t.test(age~sex,data=melanoma)
age_t_test

p_age<-ggplot(data=melanoma,aes(sample=age))
p_age+stat_qq()+stat_qq_line()
p_age+stat_qq()+stat_qq_line()+facet_grid(.~age)+facet_wrap(sex)

thickness_t_test<-t.test(thickness~sex,data=melanoma)
thickness_t_test

time_t_test<-t.test(time~sex,data=melanoma)
time_t_test

p_time<-ggplot(data=melanoma,aes(sample=time))
p_time+stat_qq()+stat_qq_line()
p_time+stat_qq()+stat_qq_line()+facet_grid(.~time)+facet_wrap(sex)
melanoma%>%
  group_by(sex)%>%
  summarize(num.obs=n(),
            mean_thickness=round(mean(thickness),0),
            sd_thickness=round(sd(thickness),0),
            se_thickness=round(sd(thickness)/sqrt(num.obs),0))

melanoma%>%
  group_by(sex)%>%
  summarize(num.obs=n(),
            mean_time=round(mean(time),0),
            sd_time=round(sd(time),0),
            se_time=round(sd(time)/sqrt(num.obs),0))

p_thickness<-ggplot(data=melanoma,aes(sample=thickness))
p_thickness+stat_qq()+stat_qq_line()
p_thickness+stat_qq()+stat_qq_line()+facet_grid(.~thickness)+facet_wrap(sex)
data("breastcancer")
data<-breastcancer
set.seed(12)
install.packages("ggplot2")
qqplot()
library(ggplot2)


q_age<-ggplot(data=melanoma,aes(sample=sex))
q_age+stat_qq()+stat_qq_line()
q_age+stat_qq()+stat_qq_line()+facet_wrap(.~sex)
                    
head(melanoma)

attach(melanoma)
cor(age,time,method="pearson")
plot(age,time,main="Scatterplot: Age ~ Time")
my_model=lm(formula=time~age)
linreg<-lm(time~age)
summary(my_model)
plot(time,age)
plot(age,time,main="Time v Age Scatterplot with Regression Line",xlab="Age",ylab="Time")
abline(linreg,col="blue")

attach(melanoma)
cor(time,thickness,method="pearson")
plot(time,thickness,main="Scatterplot: Time ~ Thickness")
my_model=lm(formula=thickness~time)
linreg<-lm(thickness~time)
summary(my_model)
plot(thickness,time)
plot(time,thickness,main="Time V Thickness Scatterplot with Regression Line",xlab="Time",ylab="Thickness")
abline(linreg,col="green")

cor(age,thickness,method="pearson")
plot(age,thickness,main="Scatterplot: Age ~ Thickness")
my_model=lm(formula=thickness~age)
linreg<-lm(thickness~age)
summary(my_model)
plot(thickness,age)
plot(age,thickness,main="Thickness v Time Scatterplot with Regression Line",xlab="Age",ylab="Thickness")
abline(linreg,col="blue")


sex<-c("Female","Male")
data<-c(126,79)
pie(data,labels=sex,main="Pie Chart:Sex")

data<-c(126,79)
labels<-c("Female(61%)","Male(39%)")
colors<-c("orange","lightgreen")
pie(data,
    labels=labels,
    col=colors,
    main="Pie Chart: Sex",
    border="black")

data<-c(126,79)
labels<-c("Female","Male")
colors<-c("lightblue","lightgreen")
barchart(data,labels=labels,
       col =colours,
       main="Bar Chart (Sex)")

categories <- c("Female", "Male")
values <- c(126, 79)
barplot(values, names.arg = categories, col = "skyblue", main = "Bar Chart (Sex)", ylab = "Values")


categories<-c("Melanoma death","Alive","Unrelated")
values<-c(57,134,14)
barplot(values,names.arg=categories,col="red",main="Bar Chart (Status)",ylab="Values")


data<-c(57,134,14)
labels<-c("Melanoma death(28%)","Alive(65%)","Unrelated(7%)")
colors<-c("grey","lightgreen","lightblue")
pie(data,
    labels=labels,
    col=colors,
    main="Patient's Status at Study End",
    border="black")

categories<-c("Absent","Present")
values<-c(115,90)
barplot(values,names.arg=categories,col="orange",main="Bar Chart (Ulcer)",ylab="Values")


data<-c(115,90)
labels<-c("Absent(56%)","Present(44%)")
colors<-c("blue","cyan")
pie(data,
    labels=labels,
    col=colors,
    main="Patient Ulceration",
    border="black")
