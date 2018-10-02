####                      Info:                                      ####
####                                                                  ###
####   Authors: Mandelkern, M. & Phillips, J.                         ###
####                                                                  ###
####   Title: Sticky situations: Force and quantifier domains         ###
####   Contact: phillips01@g.harvard.edu                              ###

#### Working directory, packages and data ####

rm(list=ls())
setwd("C:/Users/Jonat/Dropbox/Force_Mandelkern/materials")

# packages
library(lsr)
library(tidyverse)
library(lme4)
blackGreyPalette <- c("#2C3539", "#999999") 

## Force domain expansion #####
d1 <- read.csv("data/study1.csv")

#demographics
length(d1$ResponseId)
table(d1$gender,exclude=NULL)
mean(d1$age,na.rm=T)
sd(d1$age,na.rm=T)

d1$time <- rowSums(d1[,c(20,25,30,34,39,44,48,53,58,62,67,72)],na.rm=T)
#hist(d1$time[d1$time<100],breaks=300) ## if you wanted to exclude based on reading time, it looks like 20 or so might be reasonable cutoff

## Figure ####

d1.graph <- d1 %>% select(c(ResponseId,22,27,36,41,50,55,64,69,81,82)) %>%
  gather(question,response,-c(1,10:11)) %>%
  filter(response!="") %>%
  #filter(time>20) %>%
  mutate(question= factor(question),
         morality = factor(c(rep(c(rep("Cargo",2),rep("Passengers",2)),2))[question]),
         morality = factor(morality,levels=c("Passengers","Cargo")),
         sentence = factor(rep(c("Active","Passive"),4)[question]),
         order = factor(c(rep(c("Judged first","Judged second"),2),rep(c("Judged second","Judged first"),2))[question]),
         condition = factor(c(rep("Active then passive",4),rep("Passive then active",4))[question])
         ) %>%
  group_by(morality,order,condition) %>%
  summarize(N  = length(response),
            mean = mean(response, na.rm=TRUE),
            sd = sd(response, na.rm=TRUE),
            se   = sd/ sqrt(N)
  ) %>%
  ggplot(aes(x=condition, y=mean, fill=order)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  facet_wrap(~morality) +
  ylab("Agreement Rating") +
  xlab("") +
  coord_cartesian(ylim=c(1,100)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.125,.9)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
    ,plot.title = element_text(face="bold",vjust=.75,size=rel(1.75))
  )

#ggsave(d1.graph, file="figures/Fig1.jpg")

### pre-registered analyses ###

d1l <- d1 %>% select(c(ResponseId,22,27,36,41,50,55,64,69,81,82)) %>%
  gather(question,response,-c(1,10:11)) %>%
  filter(response!="") %>%
  #filter(time>20) %>%
  mutate(question= factor(question),
         morality = factor(c(rep(c(rep("Cargo",2),rep("Passengers",2)),2))[question]),
         morality = factor(morality,levels=c("Passengers","Cargo")),
         sentence = factor(rep(c("Active","Passive"),4)[question]),
         order = factor(c(rep(c("Judged first","Judged second"),2),rep(c("Judged second","Judged first"),2))[question]),
         condition = factor(c(rep("Active then passive",4),rep("Passive then active",4))[question])
  )

## three-way interaction:
  
lm1.0 <- lmer(response ~ morality * condition * order + (1|ResponseId), data=d1l)
lm1.1 <- lmer(response ~ (morality * condition) + (condition * order) + (order * morality) + (1|ResponseId), data=d1l)

anova(lm1.0,lm1.1) # 31.154      1  2.384e-08

## condition * order interaction for passengers 

d1l.p <- d1l %>% filter(morality=="Passengers")
  
lm1.2 <- lmer(response ~ condition * order + (1|ResponseId), data=d1l.p)
lm1.3 <- lmer(response ~ condition + order + (1|ResponseId), data=d1l.p)

anova(lm1.2,lm1.3) #28.997      1   7.25e-08

## planned comparisons:

d1l.p.sum <- d1l.p %>% group_by(condition,order) %>%
                      summarize(N  = length(response),
                                 mean = mean(response, na.rm=TRUE),
                                 sd = sd(response, na.rm=TRUE),
                                 se   = sd/ sqrt(N)
                                 )

## Passive then active sentence judgments:
var.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
         d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Passive then active"])
t.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
       d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Passive then active"],var.equal = T)
cohensD(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
        d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Passive then active"])

## Active then passive sentence judgments:
var.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Active then passive"],
         d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"])
t.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Active then passive"],
       d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"],var.equal = T)
cohensD(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Active then passive"],
        d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"])

## condition * order interaction for cargo 

d1l.c <- d1l %>% filter(morality=="Cargo")

lm1.4 <- lmer(response ~ condition * order + (1|ResponseId), data=d1l.c)
lm1.5 <- lmer(response ~ condition + order + (1|ResponseId), data=d1l.c)

anova(lm1.4,lm1.5) #4.0929      1    0.04306 



## exploratory analyses ####

d1l.c.sum <- d1l.c %>% group_by(condition,order) %>%
  summarize(N  = length(response),
            mean = mean(response, na.rm=TRUE),
            sd = sd(response, na.rm=TRUE),
            se   = sd/ sqrt(N)
  )

## Passive then active sentence judgments:
var.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
         d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"])
t.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
       d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"],var.equal = T)
cohensD(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
        d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"])

#Active then passive judgments
var.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"],
         d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"])
t.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"],
       d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"],var.equal = T)
cohensD(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"],
        d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"])


## Directionality comparisons for the passive sentence before/after reading active

# passengers
var.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
         d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"])
t.test(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
       d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"],var.equal = T)
cohensD(d1l.p$response[d1l.p$order=="Judged first" & d1l.p$condition=="Passive then active"],
        d1l.p$response[d1l.p$order=="Judged second" & d1l.p$condition=="Active then passive"])

#cargo
var.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
         d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"])
t.test(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
       d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"])
cohensD(d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Passive then active"],
        d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Active then passive"])


#overall does active agreement depend on order?

anova(lm(response ~ order * morality, data=d1l[d1l$sentence=="Active",]))
etaSquared(lm(response ~ order * morality, data=d1l[d1l$sentence=="Active",]))

#Matt's question about cargo cases: active vs. active
var.test(d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"],
         d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"])
t.test(d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"],
       d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"],var.equal = T)
cohensD(d1l.c$response[d1l.c$order=="Judged second" & d1l.c$condition=="Passive then active"],
        d1l.c$response[d1l.c$order=="Judged first" & d1l.c$condition=="Active then passive"])

