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






















