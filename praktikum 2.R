library('BSDA')
#==================================================
#1.
Orang.ke=c(seq(1:9))
SO.sebelum =c(78,75,67,77,70,72,78,74,77)
SO.sesudah =c(100,95,70,90,90,90,89,90,100)
data = data.frame(Orang.ke,SO.sebelum,SO.sesudah)

#1a
selisih = c(data$SO.sesudah-data$SO.sebelum)

sd(selisih)

#1b
t.test(y=data$SO.sebelum, x=data$SO.sesudah)

#1c
t.test(x=data$SO.sebelum, y=data$SO.sesudah,
       alternative = "two.sided",paired = TRUE,
       conf.level = 0.95)

#================================================
#2
zsum.test(mean.x=23500, sigma.x=3900,n.x=100,
          alternative="greater")

#================================================
#3
tsum.test(mean.x = 3.64, mean.y = 2.79,
          s.x = 1.67, s.y = 1.32,
          n.x = 19, n.y = 27,
          alternative = "two.sided",
          var.equal = TRUE,
          conf.level = 0.95,)

#=================================================
#4
library(dplyr)
my_data <- read.delim("C:/Users/ASUS/Documents/onewayanova.txt")

#4a
grup1 <- filter(my_data, Group==1)
grup2 <- filter(my_data, Group==2)
grup3 <- filter(my_data, Group==3)
#View(grup1)

qqnorm(grup1$Length, main = "kuantil Normal Grup 1")
qqnorm(grup2$Length, main = "kuantil Normal Grup 2")
qqnorm(grup3$Length, main = "kuantil Normal Grup 3")

library("ggpubr")
ggboxplot(my_data, x = "Group", y = "Length",
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Length", xlab = "Group")

#4b
bartlett.test(Length ~ Group, data = my_data)

#4c
model1<- aov(Length ~ Group, data = my_data)
summary(model1)

#4e
model2<-aov(Length ~ factor(Group), data = my_data)
TukeyHSD(model2)

#4f
data = data.frame(my_data)
ggplot(data, aes(y=Length,x=Group,
           color=Group))+
  geom_point()
           

#=======================================================
#5

#5a
data2 <- read.csv("C:/Users/ASUS/Documents/GTL.csv")
View(data2)

ggplot(data2, aes(y=Temp,x=Light,
                 color=Glass))+
  geom_point()

str(data2)

library("ggpubr")
ggboxplot(data2, y = "Light", x = "Temp", color = "Glass",
          palette = c("#00AFBB", "#E7B800","#669999"))

ggline(data2, x = "Temp", y = "Light", color = "Glass",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800","#669999"))


#5b
interaction.plot(x.factor = data2$Temp, trace.factor =
                   data2$Glass,
                 response = data2$Light, fun = mean,
                 type = "b", legend = TRUE,
                 xlab = "Temp", ylab="Light",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800", "#669999"))

res.aov2 <- aov(Light ~ Temp + Glass, data = data2)
summary(res.aov2)

#5c
require(dplyr)
group_by(data2, data2$Glass, data2$Temp) %>%
  summarise(
    count = n(),
    mean = mean(data2$Light, na.rm = TRUE),
    sd = sd(data2$Light, na.rm = TRUE)
  )

#5d
anova2 <- aov(Light ~ factor(Temp) + factor(Glass), data = data2)
tukey <- TukeyHSD(anova2)

#5e
library(multcompView)
cld <- multcompLetters4(anova2,tukey)
cld
