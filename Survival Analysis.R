library(survival)

survival1 <- survival_analysis[-c(2,37,50,61,73,146,151,195,202,384,405,420,424,435,513,514,525,528,554,565,568,572,575,593,600,611),]
fit1 <- coxph(Surv(os_time , os_event) ~ as.factor(trt) + as.factor(B_SYM), data = survival1)
summary(fit1)

fit2 <- coxph(Surv(os_time , os_event) ~ as.factor(trt), data = survival1)
summary(fit2)

1-pchisq(2*(fit1$loglik[2] - fit2$loglik[2]), 1)
survival2 <- survival_analysis[-c(66,74,117,140,235,236,238,379,380,439,440,486,607,616,316,345,359,360,361,367,368,369,434,510,614),]

fit3 <- coxph(Surv(os_time , os_event) ~ as.factor(trt) + as.factor(RACE), data = survival2)
summary(fit3)

fit4 <- coxph(Surv(os_time , os_event)~as.factor(trt), data = survival2)
summary(fit4)

1-pchisq(2*(fit3$loglik[2] - fit4$loglik[2]), 3)
fit0 <- coxph(Surv(os_time , os_event)~as.factor(PERF_STA)+AGE+as.factor(STAGE)+as.factor(SEX)+as.factor(PR_RAD)+as.factor(B_SYM)+as.factor(r_score)+as.factor(pr_resp)+as.factor(pr_drug)+as.factor(trt), data = survival_analysis)
summary(fit0)

fit6 <- coxph(Surv(os_time , os_event)~as.factor(PERF_STA)+as.factor(STAGE)+as.factor(SEX)+as.factor(PR_RAD)+as.factor(B_SYM)+as.factor(r_score)+as.factor(pr_resp)+as.factor(pr_drug), data = survival_analysis)
summary(fit6)

1-pchisq(2*(fit0$loglik[2] - fit6$loglik[2]), 16-14)
fit7 <- coxph(Surv(os_time , os_event)~as.factor(PERF_STA)+as.factor(PR_RAD)+as.factor(B_SYM)+as.factor(r_score)+as.factor(pr_resp)+as.factor(pr_drug), data = survival_analysis)
summary(fit7)

1-pchisq(2*(fit6$loglik[2] - fit7$loglik[2]), 4)
fit8 <- coxph(Surv(os_time , os_event)~as.factor(PERF_STA)+as.factor(PR_RAD)+as.factor(B_SYM)+as.factor(r_score)+as.factor(pr_resp), data = survival_analysis)
summary(fit8)

1-pchisq(2*(fit7$loglik[2] - fit8$loglik[2]), 1)
fit9 <- coxph(Surv(os_time , os_event)~as.factor(PERF_STA)+as.factor(B_SYM)+as.factor(r_score)+as.factor(pr_resp), data = survival_analysis)
summary(fit9)


1-pchisq(2*(fit8$loglik[2] - fit9$loglik[2]),1)
fit.km <- survfit(Surv(os_time , os_event)~trt, data= survival_analysis)

plot(fit.km,xlab="Time", ylab="Estimated SurvivalProbability")

fit.km1 <- survfit(Surv(os_time, os_event)~ PERF_STA, data = survival_analysis)
fit.km2 <- survfit(Surv(os_time, os_event)~ PR_RAD, data = survival_analysis)
fit.km3 <- survfit(Surv(os_time, os_event)~ B_SYM, data = survival_analysis)
fit.km4 <- survfit(Surv(os_time, os_event)~ r_score, data = survival_analysis)
fit.km5 <- survfit(Surv(os_time, os_event)~ pr_resp, data = survival_analysis)

plot(fit.km1, xlab= "Time", ylab= "Estimated SurvivalProbability")
plot(fit.km2, xlab= "Time", ylab= "Estimated SurvivalProbability")
plot(fit.km3, xlab= "Time", ylab= "Estimated SurvivalProbability")

text(100,0.2,"Y")
text(100,0.5,"N")
plot(fit.km4, xlab= "Time", ylab= "Estimated SurvivalProbability")

text(100,0.15, ">=3")
text(100,0.4, "2")
text(100,0.6, "0, 1")
plot(fit.km5, xlab= "Time", ylab= "Estimated SurvivalProbability")

text(100,0.15, "SDPD")
text(100,0.35, "DU<1")
text(100,0.6, "DU>1")

logrk0 <- survdiff(Surv(os_time, os_event)~as.factor(trt), data = survival_analysis)
logrk0

logrk1 <- survdiff(Surv(os_time, os_event)~as.factor(PERF_STA), data = survival_analysis)
logrk1

logrk2 <- survdiff(Surv(os_time, os_event)~as.factor(PR_RAD), data = survival_analysis)
logrk2

logrk3<-survdiff(Surv(os_time, os_event)~as.factor(B_SYM), data = survival_analysis)
logrk3

logrk4<-survdiff(Surv(os_time, os_event)~as.factor(r_score), data = survival_analysis)
logrk4

logrk5<-survdiff(Surv(os_time, os_event)~as.factor(pr_resp), data = survival_analysis)
logrk5

delta <- survival_analysis$os_event
x <- survival_analysis$os_time
PS <- survival_analysis$PERF_STA
PR <- survival_analysis$PR_RAD
BS <- survival_analysis$B_SYM
rs <- survival_analysis$r_score
pr_resp <- survival_analysis$pr_resp
PERF_STA <- survival_analysis$PERF_STA
AGE <- survival_analysis$AGE
STAGE <- survival_analysis$STAGE
SEX <- survival_analysis$SEX
B_SYM <- survival_analysis$B_SYM
r_score <- survival_analysis$r_score
PR_RAD <- survival_analysis$PR_RAD
pr_drug <- survival_analysis$pr_drug
pr_resp <- survival_analysis$pr_resp
trt <- survival_analysis$trt

plot(PS[delta==1], log(x[delta==1]), pch=4, xlab="performance status: 0, 1, 2,3", ylab="log time")
points(PS[delta==0], log(x[delta==0]), pch=1)

plot(AGE[delta==1], log(x[delta==1]), pch=4, xlab="age in year", ylab="log time")
points(AGE[delta==0], log(x[delta==0]), pch=1)

plot(STAGE[delta==1], log(x[delta==1]), pch=4, xlab="cancer stage; I, II, III", ylab="log time")
points(STAGE[delta==0], log(x[delta==0]), pch=1)

plot(SEX[delta==1], log(x[delta==1]), pch=4, xlab="Female vs male", ylab="log time")
plot(SEX[delta==1], log(x[delta==1]), pch=4, xlab="cancer stage; I, II, III", ylab="log time")
points(SEX[delta==0], log(x[delta==0]), pch=1)

plot(SEX[delta==1], log(x[delta==1]), pch=4, xlab="Female vs male", ylab="log time")
points(SEX[delta==0], log(x[delta==0]), pch=1)

plot(B_SYM[delta==1], log(x[delta==1]), pch=4, xlab="B-symptom, Yes vs No", ylab="log time")
points(B_SYM[delta==0], log(x[delta==0]), pch=1)

plot(r_score[delta==1], log(x[delta==1]), pch=4, xlab="International rIPI score: 0 or 1 vs 2 vs 3 or more", ylab="log time")
points(r_score[delta==0], log(x[delta==0]), pch=1)

plot(PR_RAD[delta==1], log(x[delta==1]), pch=4, xlab="Received radiotherapy before randomization", ylab="log time")
points(PR_RAD[delta==0], log(x[delta==0]), pch=1)

plot(PR_RAD[delta==1], log(x[delta==1]), xlab="Received radiotherapy before randomization", ylab="log time")
plot(pr_drug[delta==1], log(x[delta==1]), pch=4, xlab="Received other drug before randomization", ylab="log time")
points(pr_drug[delta==0], log(x[delta==0]), pch=1)

plot(pr_resp[delta==1], log(x[delta==1]), pch=4, xlab="Response to other treatments before randomization", ylab="log time")
points(pr_resp[delta==0], log(x[delta==0]), pch=1)

plot(trt[delta==1], log(x[delta==1]), pch=4, xlab="treatment vs control", ylab="log time")
points(trt[delta==0], log(x[delta==0]), pch=1)














