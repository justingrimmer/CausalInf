library(foreign)

exp <- read.dta("~/Desktop/CausalInf/Homeworks/HW3/nsw_exper.dta")
nexp <- read.dta("~/Desktop/CausalInf/Homeworks/HW3/nsw_psid_withtreated.dta")

set.seed(1)

# a

SATE <- mean(exp$re78[exp$nsw==1]) - mean(exp$re78[exp$nsw==0])

SE <- sqrt(var(exp$re78[exp$nsw==1])/sum(exp$nsw==1) + 
             var(exp$re78[exp$nsw==0])/sum(exp$nsw==0))

ci1 <- SATE - 1.96*(SE/sqrt(sum(exp$nsw)))
ci2 <- SATE + 1.96*(SE/sqrt(sum(exp$nsw)))

# b 

ATEage <- summary(lm(re78 ~ nsw + age + educ + black + hisp + married + re74
                     + re75 + u74 + u75, data = exp))
ATEage

# c 


