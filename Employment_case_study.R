library(dplyr)
library(data.table)
library(magrittr)
library(rpart)
library(ggplot2)
library(randomForest)
library(bit64)
library(psych)
library(ROCR)
library(glmnet)
library(rpart.plot)

# load data
dat = fread('link.csv', data.table = FALSE)

# quick summary
str(dat)
summary(dat)

#################### Data Cleaning and Transformation ##########################
# remove the rows with null values
dat[profiles_viewed == "NULL"] %>% View
dat <- dat[profiles_viewed != "NULL"] 
# reason for null value? talk to software engineer

# transform column types
char.vars <- names(dat)[lapply(dat, class) == "character"]
dat[char.vars] <- lapply(dat[char.vars], factor)


# check for duplicates
dat %>% unique %>% nrow
dat %>% nrow


# create new variables
dat[, job_views := job_views_desktop + job_views_mobile]
dat[, job_applies := job_applies_desktop + job_applies_mobile]

dat[, apply_to_view := job_applies/job_views]
dat[, apply_to_view_mobile := job_applies_mobile/job_views_mobile]
dat[, apply_to_view_desktop := job_applies_desktop/job_views_desktop]

dat[job_views == 0, apply_to_view := -1]
dat[job_views_mobile == 0, apply_to_view_mobile := -1]
dat[job_views_desktop == 0, apply_to_view_desktop := -1]




########### exploratory analysis
pattern <- dat[,.(pct = (.N/nrow(dat)) %>% round(3)),by=list(profile_view = as.numeric(profiles_viewed > 0)
                                                             , job_view = as.numeric(job_views > 0)
                                                             , apply = as.numeric(job_applies > 0)
                                                             , hire)][order(profile_view, job_view, apply, hire),]
# hire_rate
dat[,.(hire_rate = mean(hire==1))]

# seniority
dat_sen = dat[,.(hire_rate = mean(hire==1)), by=seniority]
ggplot(aes(x=seniority, y=hire_rate), data=dat_sen) + geom_bar(stat='identity', aes(fill=seniority)) +
  xlab("Seniority") + ylab("Hire Rate") + ggtitle('Hire Rate by Member Seniority') + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=40, vjust=.8, hjust=1.01))

dat$seniority <- factor(dat$seniority)
dat$seniority <- reorder(dat$seniority, new.order = c('unknown', 'Unpaid', 'Training',
                                                      'Entry', 'Senior', 'Manager',
                                                      'Director', 'VP','CXO', 'Partner',
                                                      'Owner'))
sen = table(seniority = dat$seniority) %>% prop.table() %>% round(3) %>% as.data.frame

ggplot(data=sen, aes(x=seniority, y=Freq)) + geom_bar(stat='identity', aes(fill=seniority)) + 
  xlab("Seniority") + ylab("Percent of Population") + ggtitle('Distribution of Member Seniority') +
  theme(text = element_text(size=16), axis.text.x = element_text(angle=40, vjust=.8, hjust=1.01))


table(dat$seniority, as.numeric(dat$job_views > 0)) %>% prop.table(1) %>% round(3)
table(dat$seniority, as.numeric(dat$job_applies > 0)) %>% prop.table(1) %>% round(3)
table(dat$seniority, as.numeric(dat$profiles_viewed > 0)) %>% prop.table(1) %>% round(3)
dat[,.(avg_prof_views = mean(profiles_viewed)), by=seniority][order(-avg_prof_views),]
dat[,.(avg_prof_score = mean(profile_completeness_score)), by=seniority][order(-avg_prof_score),]
dat[job_views > 0,.(avg_job_views = mean(job_views)), by=seniority][order(-avg_job_views),]
dat[,.(avg_job_views = mean(job_views)), by=seniority][order(-avg_job_views),]
dat[,.(avg_applies = mean(job_applies)), by=seniority][order(-avg_applies),]
dat[job_views != 0,.(apply_to_view %>% mean), by=seniority][order(-V1),]

# profile completeness -> very interesting patterns
dat_pro_com = dat[,.(hire_rate = mean(hire==1)), by=profile_completeness_score]
ggplot(aes(profile_completeness_score, hire_rate), data=dat_pro_com) + geom_point() + 
  geom_smooth() + xlab("Profile Completeness Score") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Profile Completeness Score') + theme(text = element_text(size=16))

dat[profile_completeness_score > 75] %>% View
qplot(as.factor(hire), profile_completeness_score,data=dat, geom='boxplot')

ggplot(data=dat, aes(profile_completeness_score)) + geom_density() +
  xlab("Profile Completeness Score") + ylab("Density") + 
  ggtitle('Distribution of Profile Completeness Score') + theme(text = element_text(size=16))

# connections
summary(dat$connections)
qplot(as.factor(hire), connections, data=dat, geom='boxplot')
quantile(dat$connections, 0.95)
quantile(dat$connections, 0.96)

dat$binned_conn = cut(dat$connections, breaks=c(-1, seq(100,1000,100), Inf), 
                  labels = c(seq(0,900,100), '1000+'))
dat_conn = dat[,.(hire_rate = mean(hire==1)), by=binned_conn][order(binned_conn),]
ggplot(data=dat_conn, aes(x=binned_conn, y=hire_rate)) + geom_bar(stat = 'identity', colour = '#0089dd', fill='#0089dd') + 
  xlab("Number of Connections") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Number of Connections') + theme(text = element_text(size=16))

ggplot(data=dat, aes(connections)) + geom_density() +
  xlab("Number of Connections") + ylab("Density") + 
  ggtitle('Distribution of Number of Connections') + theme(text = element_text(size=16))


# is subscriber
dat$is_subscriber = reorder(dat$is_subscriber, new.order = c('Y','N'))
dat[,.(mean(is_subscriber=='Y'))]
dat_sub = dat[,.(hire_rate = mean(hire==1)), by=is_subscriber]
ggplot(data=dat_sub, aes(x=is_subscriber, y=hire_rate)) + geom_bar(stat='identity', aes(fill=is_subscriber)) +
  scale_fill_manual(values=c('#0089dd', '#666666')) +
  xlab("Is Subscriber") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Subscription') + theme(text = element_text(size=16))
ggplot(data=dat, aes(x=is_subscriber)) + geom_boxplot(aes(y=profile_completeness_score))
ggplot(data=dat, aes(x=is_subscriber)) + geom_boxplot(aes(y=connections))
ggplot(data=dat, aes(x=is_subscriber)) + geom_boxplot(aes(y=profiles_viewed))
plot_sub = table(sub = dat$is_subscriber, seniority = dat$seniority) %>% prop.table(2) %>% round(3) %>% as.data.frame()
ggplot(data=plot_sub, aes(x=seniority, y=Freq, fill=sub)) + geom_bar(stat='identity')


# profiles_viewed
summary(dat$profiles_viewed)
qplot(profiles_viewed, data=dat, geom='density')
quantile(dat$profiles_viewed, 0.95)

dat$binned_views = cut(dat$profiles_viewed, breaks=c(-1, seq(5,100,5), Inf), 
                      labels = c(seq(0,95,5), '100+'))
dat_views = dat[,.(hire_rate = mean(hire==1)), by=binned_views][order(binned_views),]

ggplot(data=dat_views, aes(x=binned_views, y=hire_rate)) + geom_bar(stat = 'identity') +
  xlab("Number of Profiles Viewed") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Number of Profiles Viewed') + theme(text = element_text(size=16))


ggplot(data=dat, aes(profiles_viewed)) + geom_density() +
  xlab("Number of Profiles Viewed") + ylab("Density") + 
  ggtitle('Distribution of Profile Views') + theme(text = element_text(size=16))

# inmails_received
summary(dat$inmails_received)
quantile(dat$inmails_received, 0.99)
table(dat$inmails_received)
dat$binned_inmails = cut(dat$inmails_received, breaks=c(-1, seq(1,5,1), Inf),
                         labels = c(seq(0,4,1),'5+'))

dat_inmails = dat[,.(hire_rate = mean(hire==1)), by=binned_inmails]
ggplot(data=dat_inmails, aes(x=binned_inmails, y=hire_rate)) + geom_bar(stat = 'identity') +
  xlab("Number of InMails Received") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Number of InMails Received') + theme(text = element_text(size=16))

# desktop vs mobile
dat[,.(sum_job_views_mobile = sum(job_views_mobile),
       sum_job_views_desktop = sum(job_views_desktop))] %>% prop.table() %>% round(3)

dat[,.(sum_job_applies_mobile = sum(job_applies_mobile),
       sum_job_applies_desktop = sum(job_applies_desktop))] %>% prop.table() %>% round(3)
# job_views_desktop
table(dat$job_views_desktop)
dat_view_d = table(dat$hire, as.numeric(dat$job_views_desktop > 0)) %>% prop.table(2)
barplot(1 - dat_view_d)

# job_views_mobile
table(dat$job_views_mobile)
dat_view_m = table(dat$hire, as.numeric(dat$job_views_mobile > 0)) %>% prop.table(2)
barplot(1 - dat_view_m)

# job_applies_desktop
table(dat$job_applies_desktop)
dat_apply_d = table(dat$hire, as.numeric(dat$job_applies_desktop > 0)) %>% prop.table(2)
barplot(1 - dat_apply_d)

# job_applies_mobile
table(dat$job_applies_mobile)
dat_apply_m = table(dat$hire, as.numeric(dat$job_applies_mobile > 0)) %>% prop.table(2)
barplot(1 - dat_apply_m)

# job_views
table(dat$job_views)
dat_view = table(hire = dat$hire, job_views = as.numeric(dat$job_views > 0)) %>% prop.table(2) %>% 
  round(3) %>% data.frame

ggplot(data=dat_view[dat_view$hire=='1',], aes(x=job_views, y=Freq)) + geom_bar(stat = 'identity') +
  xlab("Number of Job Views") + ylab("Hire Rate") + scale_x_discrete(labels=c('0','1+')) +
  ggtitle('Hire Rate by Job Views') + theme(text = element_text(size=16))



# job_applies
table(dat$job_applies)
dat_apply = table(hire = dat$hire, job_applies = as.numeric(dat$job_applies > 0)) %>% prop.table(2) %>% 
  round(3) %>% data.frame

ggplot(data=dat_apply[dat_apply$hire=='1',], aes(x=job_applies, y=Freq)) + geom_bar(stat = 'identity') +
  xlab("Number of Job Applies") + ylab("Hire Rate") + scale_x_discrete(labels=c('0','1+')) +
  ggtitle('Hire Rate by Job Applies') + theme(text = element_text(size=16))

barplot(1 - dat_apply)

quantile(dat$job_applies, 0.95)
dat$binned_job_applies = cut(dat$job_applies, breaks=c(-1, seq(10,70,10), Inf),
                           labels = c(seq(0,60,10),'70+'))

dat_job_views = dat[,.(hire_rate = mean(hire==1)), by=binned_job_views]
ggplot(data=dat_job_views, aes(x=binned_job_views, y=hire_rate)) + geom_bar(stat = 'identity') +
  xlab("Number of Job Views") + ylab("Hire Rate") + 
  ggtitle('Hire Rate by Job Views') + theme(text = element_text(size=16))



# apply_to_view
ggplot(data=dat, aes(apply_to_view)) + geom_density()
table(apply = as.numeric(dat$job_applies > 0), view = as.numeric(dat$job_views > 0))
dat[job_views != 0,.(apply_to_view %>% mean)] #one application per 10 job views



######## checking correlations
source("panelfxns.R")
cont.vars<-dat[,num_cols, with=FALSE]
pairs(cont.vars,upper.panel=panel.smooth, lower.panel=panel.cor)

num_cols <- c('profile_completeness_score', 'connections', 'profiles_viewed',
              'inmails_received', 'job_views_desktop', 'job_views_mobile', 
              'job_applies_desktop', 'job_applies_mobile')
psych::pairs.panels(dat[,num_cols, with=FALSE])

# check proportion of mobile vs desktop
sum(dat$job_applies_mobile)/sum(dat$job_applies)
sum(dat$job_views_mobile)/sum(dat$job_views)







########### modeling
#regression tree
dat[,memberid := NULL]
tree <- rpart(hire~., data=dat, control = rpart.control(cp = 0.0001))
tree

printcp(tree)
0.399*0.65
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)

conf.matrix <- table(dat$hire, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)


tree_sub <-rpart(is_subscriber~.,data=dat)
tree_sub


# random forest
samp = sample(nrow(dat), nrow(dat)*0.66)
train_dat = dat[samp,]
test_dat = dat[-samp,]
rf = randomForest(y=train_dat$hire, x = train_dat[, -c('hire', 'binned_views', 'binned_conn'), with=FALSE],
                  ytest = test_dat$hire, xtest = test_dat[, -c('hire', 'binned_views', 'binned_conn'), with=FALSE],
                  ntree = 100, mtry = 5, keep.forest = TRUE)
rf
varImpPlot(rf)
#roc and auc 
rf.pr=as.vector(rf$votes[,2])
rf.pred = prediction(rf.pr, train_dat$hire)
perf_AUC=performance(rf.pred,"auc")
AUC=perf_AUC@y.values[[1]]


#performance in terms of true and false positive rates
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")


partialPlot(rf, train_dat, profile_completeness_score, 1)
partialPlot(rf, train_dat, seniority, 1)
partialPlot(rf, train_dat, connections, 1)
partialPlot(rf, train_dat, profiles_viewed, 1)
partialPlot(rf, train_dat, job_views, 1)
partialPlot(rf, train_dat, is_subscriber, 1)
partialPlot(rf, train_dat, inmails_received, 1)
partialPlot(rf, train_dat, apply_to_view, 1)

#logistic reg
fit <- glm(hire~., data=dat, family = 'binomial'(link='logit'))
summary(fit)

predicted <- predict(fit,dat, type = 'response')
table(response = dat$hire, predicted = as.numeric(predicted>=0.5))
1-mean(dat$hire != as.numeric(predicted>=0.5))



