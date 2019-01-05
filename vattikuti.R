file <- read.csv('cleanData.csv')

summary(file)

#Corelation Analysis

cor(file$N1, file$A02650, use="complete.obs")

agi1 <- file[which(file$agi_stub=='1'),]
agi2 <- file[which(file$agi_stub=='2'),]
agi3 <- file[which(file$agi_stub=='3'),]
agi4 <- file[which(file$agi_stub=='4'),]
agi5 <- file[which(file$agi_stub=='5'),]
agi6 <- file[which(file$agi_stub=='6'),]

cor (agi1$N1, agi1$A02650, use = "complete.obs")
cor (agi2$N1, agi2$A02650, use = "complete.obs")
cor (agi3$N1, agi3$A02650, use = "complete.obs")
cor (agi4$N1, agi4$A02650, use = "complete.obs")
cor (agi5$N1, agi5$A02650, use = "complete.obs")
cor (agi6$N1, agi6$A02650, use = "complete.obs")

cor(file$N2, file$SCHF, use="complete.obs")
cor(file$N1, file$NUMDEP, use="complete.obs")
cor(file$N2, file$ELDERLY, use="complete.obs")

#Regression Analysis

linearMod1<-lm(formula= N1~A02650, agi1)
linearMod2<-lm(formula= N1~A02650, agi2)
linearMod3<-lm(formula= N1~A02650, agi3)
linearMod4<-lm(formula= N1~A02650, agi4)
linearMod5<-lm(formula= N1~A02650, agi5)
linearMod6<-lm(formula= N1~A02650, agi6)

print(linearMod1)
print(linearMod2)
print(linearMod3)
print(linearMod4)
print(linearMod5)
print(linearMod6)

summary(linearMod1)
summary(linearMod2)
summary(linearMod3)
summary(linearMod4)
summary(linearMod5)
summary(linearMod6)

#Hypothesis Tests

t.test(agi1$N1, agi1$A02650,alternative="greater",conf.level=0.95)

wilcox.test(agi2$N1, agi2$A02650,alternative = "greater",conf.level=0.95)
wilcox.test(agi3$N1, agi3$A02650,alternative = "greater",conf.level=0.95)
wilcox.test(agi3$N1, agi4$A02650,alternative = "greater",conf.level=0.95)
wilcox.test(agi5$N1, agi5$A02650,alternative = "greater",conf.level=0.95)
wilcox.test(agi6$N1, agi6$A02650,alternative = "greater",conf.level=0.95)

wilcox.test(file$N1, file$agi_stub,alternative = "greater",conf.level=0.95)
wilcox.test(file$N1, file$NUMDEP,alternative = "greater",conf.level=0.95)
wilcox.test(file$N2, file$ELDERLY,alternative = "greater",conf.level=0.95)







