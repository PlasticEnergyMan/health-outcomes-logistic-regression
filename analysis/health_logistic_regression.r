# =========================
# Logistic Regression Analysis of Health Outcomes
# =========================

# -------------------------
# Libraries & Setup
# -------------------------
library(xtable)
library(MASS)
library(caret)
library(boot)
library(lattice)
library(DAAG)
library(pROC)
library(Epi)
library(data.table)

#importing dataset, already clean
heart <- read.csv("https://raw.githubusercontent.com/PlasticEnergyMan/MATH-449-Final-Project/refs/heads/main/heart_failure_clinical_records_dataset.csv", 
              header=TRUE)
head(heart)

#First fit, overfitting. 
fit1 <- glm(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes +
              ejection_fraction + high_blood_pressure + platelets + serum_creatinine +
              serum_sodium + sex + smoking + time, family = binomial, data = heart)

summary(fit1)


x1 <- xtable(summary(fit1))


#Backwards Step

step(fit1)
stepAIC(fit1, direction="backward")

fit2 <- glm(DEATH_EVENT ~ serum_sodium + age + serum_creatinine 
            + ejection_fraction + time, family = binomial, data = heart)

summary(fit2)

drop1(fit2, test="LRT")

fit3 <- glm(DEATH_EVENT ~ age + serum_creatinine + ejection_fraction + time,
            family = binomial, data = heart)

summary(fit3)

drop1(fit3, test="LRT")

table(heart$DEATH_EVENT)

x3 <- xtable(summary(fit3), caption = "A spanning several pages")

drop1(fit4, test="LRT")

fit0 <- glm(DEATH_EVENT ~ 1, family = binomial, data = heart)


#comparison with the full model
anova(fit4, fit1, test = "Chisq")

full.tbl <- data.frame(Names = c("Resid. Df", "Resid. Dev", "Df", "Deviance", "P-val"),
                       Full = c(294, 226.30, "NA", "NA", "NA"),
                       Reduced = c(286, 219.55, 8, 6.747, 0.56))

transpose(full.tbl) 

lrt1 <- xtable(transpose(full.tbl), caption = "Table 3: LRT comparing final model to full model")

#Comparison with the null model
anova(fit0, fit3, test = "Chisq")

full.tbl0 <- data.frame(Names = c("Resid. Df", "Resid. Dev", "Df", "Deviance", "P-val"),
                       Full = c(298, 375.35, "NA", "NA", "NA"),
                       Reduced = c(294, 226.30, 4, 149.05, 2.2e-16))

transpose(full.tbl0) 


lrt2 <- xtable(transpose(full.tbl0), caption = "LRT comparing final model to null model")
#residuals
res1=data.frame(rstandard(fit3,type="pearson"), residuals(fit3,type="pearson"),
                residuals(fit3,type="deviance"), rstandard(fit3,type="deviance"))
names(res1)=cbind("standardized", "Pearson", "deviance", "std. dev")
res1

boxplot(res1, ylim=c(-5,5))

#Confidence Interval
conf=confint(fit3)

con.tbl <-data.frame(Names = c("Beta", "Lower Beta", "Upper Beta", "e Beta", "Lower e Beta", "Upper e Beta"),
                      age = c(0.043, 0.015, 0.073, 1.044, 1.01, 1.07),
                      serum_creatinine = c(0.719, 0.387, 1.096, 1.052,1.473, 2.992),
                      ejection_fraction = c(-0.075, -1.06, -0.04, 0.928, 0.346, 0.960),
                      time = c(-0.021, -0.027, -0.015,0.979 , 0.973, 0.985))

con.tbl1 <- xtable(transpose(con.tbl), caption = "Coefficient and multiplicative effect for 95% confidence level")

#Creating the Confusion Matrix
heart$prob <- predict(fit3, type="response")
Predicted1 <- ifelse(heart$prob > 0.5, 1, 0)
expected_value1 <- factor(heart$DEATH_EVENT)
predicted_value1 <- factor(Predicted1)
conmat1 <- confusionMatrix(data=predicted_value1, reference = expected_value1)
conmat1

Predicted2 <- ifelse(heart1$prob > 0.25, 1, 0)
expected_value2 <- factor(heart1$DEATH_EVENT)
predicted_value2 <- factor(Predicted2)
conmat2 <- confusionMatrix(data=predicted_value2, reference = expected_value2)
conmat2

Predicted3 <- ifelse(heart1$prob > 0.3, 1, 0)
expected_value3 <- factor(heart1$DEATH_EVENT)
predicted_value3 <- factor(Predicted3)
conmat3 <- confusionMatrix(data=predicted_value3, reference = expected_value3)
conmat3

Predicted4 <- ifelse(heart1$prob > 0.4, 1, 0)
expected_value4 <- factor(heart1$DEATH_EVENT)
predicted_value4 <- factor(Predicted1)
conmat4 <- confusionMatrix(data=predicted_value4, reference = expected_value4)
conmat4

Predicted5 <- ifelse(heart1$prob > 0.6, 1, 0)
expected_value5 <- factor(heart1$DEATH_EVENT)
predicted_value5 <- factor(Predicted5)
conmat5 <- confusionMatrix(data=predicted_value5, reference = expected_value5)
conmat5

Predicted6 <- ifelse(heart1$prob > 0.75, 1, 0)
expected_value6 <- factor(heart1$DEATH_EVENT)
predicted_value6 <- factor(Predicted6)
conmat6 <- confusionMatrix(data=predicted_value6, reference = expected_value6)
conmat6

Predicted7 <- ifelse(heart$prob > 0.85, 1, 0)
expected_value7 <- factor(heart$DEATH_EVENT)
predicted_value7 <- factor(Predicted7)
conmat7 <- confusionMatrix(data=predicted_value7, reference = expected_value7)
conmat7

conmat <- table(Predicted = Predicted, Actual = heart$DEATH_EVENT)
sens <- round(conmat[4]/(conmat[4]+conmat[2]),3)
spec <- round(conmat[1]/(conmat[3]+conmat[1]),3)
acc <- round((conmat[4]+conmat[1])/(sum(conmat)),3)

Predicted1 <- ifelse(heart$prob > 0.2, 1, 0)
conmat1 <- table(Predicted = Predicted1, Actual = heart$DEATH_EVENT)
sens1 <- round(conmat1[4]/(conmat1[4]+conmat1[2]),3)
spec1 <- round(conmat1[1]/(conmat1[3]+conmat1[1]),3)
acc1 <- round((conmat1[4]+conmat1[1])/(sum(conmat1)),3)

Predicted2 <- ifelse(heart$prob > 0.3, 1, 0)
conmat2 <- table(Predicted = Predicted2, Actual = heart$DEATH_EVENT)
sens2 <- round(conmat2[4]/(conmat2[4]+conmat2[2]),3)
spec2 <- round(conmat2[1]/(conmat2[3]+conmat2[1]),3)
acc2 <- round((conmat2[4]+conmat2[1])/(sum(conmat2)),3)

Predicted3 <- ifelse(heart$prob > 0.688, 1, 0)
conmat3 <- table(Predicted = Predicted3, Actual = heart$DEATH_EVENT)
sens3 <- round(conmat3[4]/(conmat3[4]+conmat3[2]),3)
spec3 <- round(conmat3[1]/(conmat3[3]+conmat3[1]),3)
acc3 <- round((conmat3[4]+conmat3[1])/(sum(conmat3)),3)

Predicted4 <- ifelse(heart$prob > 0.75, 1, 0)
conmat4 <- table(Predicted = Predicted4, Actual = heart$DEATH_EVENT)
sens4 <- round(conmat4[4]/(conmat4[4]+conmat4[2]),3)
spec4 <- round(conmat4[1]/(conmat4[3]+conmat4[1]),3)
acc4 <- round((conmat4[4]+conmat4[1])/(sum(conmat4)),3)

Predicted5 <- ifelse(heart$prob > 0.85, 1, 0)
conmat5 <- table(Predicted = Predicted5, Actual = heart$DEATH_EVENT)
sens5 <- round(conmat5[4]/(conmat5[4]+conmat5[2]),3)
spec5 <- round(conmat5[1]/(conmat5[3]+conmat5[1]),3)
acc5 <- round((conmat5[4]+conmat5[1])/(sum(conmat5)),3)

Predicted6 <- ifelse(heart$prob > 0.9, 1, 0)
conmat6 <- table(Predicted = Predicted6, Actual = heart$DEATH_EVENT)
sens6 <- round(conmat6[4]/(conmat6[4]+conmat6[2]),3)
spec6 <- round(conmat6[1]/(conmat6[3]+conmat6[1]),3)
acc6 <- round((conmat6[4]+conmat6[1])/(sum(conmat6)),3)

comptbl<-data.frame(Names = c("Cutoff","sensitivity","specificity","accuracy"),
                    First = c("0.2",sens1,spec1,acc1),
                    Second = c("0.3",sens2,spec2,acc2),
                    Third = c("0.5",sens,spec,acc),
                    Fourth = c("0.6",sens3,spec3,acc3),
                    Fifth = c("0.75",sens4,spec4,acc4),
                    Sixth = c("0.85",sens5,spec5,acc5),
                    Seventh = c("0.9",sens6,spec6,acc6))

transpose(comptbl)

coeftbl <- xtable(transpose(comptbl), caption = "ahhhh")

#ROC

ROC(form = DEATH_EVENT ~ age + serum_creatinine + ejection_fraction + time, plot="ROC", data = heart)


rocplot0 <- roc(DEATH_EVENT ~ fitted(fit3), data=heart)
plot.roc(rocplot0, legacy.axes=TRUE)

#trying probit and identity link 
fit4 <- glm(DEATH_EVENT ~ age + serum_creatinine + ejection_fraction + time, 
            family=binomial(link="probit"), data = heart)

summary(fit4)

heart$prob2 <- predict(fit4, type="response")
Predicted7 <- ifelse(heart$prob2 > 0.5, 1, 0)
conmat7 <- table(Predicted = Predicted7, Actual = heart$DEATH_EVENT)
sens7 <- round(conmat7[4]/(conmat7[4]+conmat7[2]),4)
spec7 <- round(conmat7[1]/(conmat7[3]+conmat7[1]),4)
acc7 <- round((conmat7[4]+conmat7[1])/(sum(conmat7)),4)

Predicted8 <- ifelse(heart$prob2 > 0.312, 1, 0)
expected_value8 <- factor(heart$DEATH_EVENT)
predicted_value8 <- factor(Predicted8)
conmat8 <- confusionMatrix(data=predicted_value8, reference = expected_value8)
conmat8

comptbl2 <- data.frame(Names = c("Cutoff","sensitivity","specificity","accuracy"),
                    First = c("logit",sens,spec,acc),
                    Second = c("probit",sens7,spec7,acc7))

transpose(comptbl2)


#cross validation
out0=cv.glm(data = heart, glmfit = fit3)
cost<-function(r,pi=0) {
  mean(abs(r-pi)>0.312)}
out1=cv.glm(data = heart, glmfit = fit3, cost, K=10)
cv.binary(fit3)
out0$delta
out1$delta

#makes sense due to unbalanced data


ggpredict(fit2, terms = c("age")) |> plot()

ggpredict(fit2, terms = "serum_creatinine[all]") |> plot()

ggpredict(fit2, terms = "ejection_fraction[all]") |> plot()

ggpredict(fit2, terms = "time[all]") |> plot()
