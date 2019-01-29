# libraries
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# Files generated with sacct command
setwd("/network/lustre/iss01/home/maxime.kermarquer/statistics/data/2018/parsable/collapse")
mars = read.table('03_mars.txt', header = TRUE, sep = "|")
avril = read.table('04_avril.txt', header = TRUE, sep = "|")
mai = read.table('05_mai.txt', header = TRUE, sep = "|")
juin = read.table('06_juin.txt', header = TRUE, sep = "|")
juillet = read.table('07_juillet.txt', header = TRUE, sep = "|")
aout = read.table('08_aout.txt', header = TRUE, sep = "|")
septembre = read.table('09_septembre.txt', header = TRUE, sep = "|")
octobre = read.table('10_octobre.txt', header = TRUE, sep = "|")
novembre = read.table('11_novembre.txt', header = TRUE, sep = "|")
decembre = read.table('12_decembre.txt', header = TRUE, sep = "|")

# Correct cancel states
mars$State = as.character(mars$State)
mars[str_detect(as.character(mars$State), "CANCEL"),]$State = "CANCELLED"
mars$month = "mars"
avril$State = as.character(avril$State)
avril[str_detect(as.character(avril$State), "CANCEL"),]$State = "CANCELLED"
avril$month = "avril"
mai$State = as.character(mai$State)
mai[str_detect(as.character(mai$State), "CANCEL"),]$State = "CANCELLED"
mai$month = "mai"
juin$State = as.character(juin$State)
juin[str_detect(as.character(juin$State), "CANCEL"),]$State = "CANCELLED"
juin$month = "juin"
juillet$State = as.character(juillet$State)
juillet[str_detect(as.character(juillet$State), "CANCEL"),]$State = "CANCELLED"
juillet$month = "juillet"
aout$State = as.character(aout$State)
aout[str_detect(as.character(aout$State), "CANCEL"),]$State = "CANCELLED"
aout$month = "aout"
septembre$State = as.character(septembre$State)
septembre[str_detect(as.character(septembre$State), "CANCEL"),]$State = "CANCELLED"
septembre$month = "septembre"
octobre$State = as.character(octobre$State)
octobre[str_detect(as.character(octobre$State), "CANCEL"),]$State = "CANCELLED"
octobre$month = "octobre"
novembre$State = as.character(novembre$State)
novembre[str_detect(as.character(novembre$State), "CANCEL"),]$State = "CANCELLED"
novembre$month = "novembre"
decembre$State = as.character(decembre$State)
decembre[str_detect(as.character(decembre$State), "CANCEL"),]$State = "CANCELLED"
decembre$month = "decembre"
mois = c("mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre")

year2018_months = c(mars,avril,mai,juin,juillet,aout,septembre,octobre,novembre,decembre)
year2018 = rbind(mars,avril,mai,juin,juillet,aout,septembre,octobre,novembre,decembre)
year2018$State = as.factor(year2018$State)


# 
summary(year2018)

# Extract data
users = unique (year2018[,"User"])
users_job_count = count (year2018[,"User"])

### Jobs
jobs_mars = count(mars[,"State"])
jobs_avril = count(avril[,"State"])
jobs_mai = count(mai[,"State"])
jobs_juin = count(juin[,"State"])
jobs_juillet = count(juillet[,"State"])
jobs_aout = count(aout[,"State"])
jobs_septembre = count(septembre[,"State"])
jobs_octobre = count(octobre[,"State"])
jobs_novembre = count(novembre[,"State"])
jobs_decembre = count(decembre[,"State"])

## completed
jobs_mars_compl = jobs_mars[jobs_mars[,1] == 'COMPLETED' , 2 ]
jobs_avril_compl = jobs_avril[jobs_avril[,1] == 'COMPLETED' , 2 ]
jobs_mai_compl = jobs_mai[jobs_mai[,1] == 'COMPLETED' , 2 ]
jobs_juin_compl = jobs_juin[jobs_juin[,1] == 'COMPLETED' , 2 ]
jobs_juillet_compl = jobs_juillet[jobs_juillet[,1] == 'COMPLETED' , 2 ]
jobs_septembre_compl = jobs_septembre[jobs_septembre[,1] == 'COMPLETED' , 2 ]
jobs_octobre_compl = jobs_octobre[jobs_octobre[,1] == 'COMPLETED' , 2 ]
jobs_novembre_compl = jobs_novembre[jobs_novembre[,1] == 'COMPLETED' , 2 ]
jobs_decembre_compl = jobs_decembre[jobs_decembre[,1] == 'COMPLETED' , 2 ]
jobs_aout_compl = jobs_aout[jobs_aout[,1] == 'COMPLETED' , 2 ]

completed_jobs = c(jobs_mars_compl, jobs_avril_compl, jobs_mai_compl, jobs_juin_compl, jobs_juillet_compl, jobs_aout_compl, 
                   jobs_septembre_compl, jobs_octobre_compl, jobs_novembre_compl, jobs_decembre_compl)

## cancelled
jobs_mars_cancel = jobs_mars[jobs_mars[,1] == 'CANCELLED' , 2 ]
jobs_avril_cancel = jobs_avril[jobs_avril[,1] == 'CANCELLED' , 2 ]
jobs_mai_cancel = jobs_mai[jobs_mai[,1] == 'CANCELLED' , 2 ]
jobs_juin_cancel = jobs_juin[jobs_juin[,1] == 'CANCELLED' , 2 ]
jobs_juillet_cancel = jobs_juillet[jobs_juillet[,1] == 'CANCELLED' , 2 ]
jobs_septembre_cancel = jobs_septembre[jobs_septembre[,1] == 'CANCELLED' , 2 ]
jobs_octobre_cancel = jobs_octobre[jobs_octobre[,1] == 'CANCELLED' , 2 ]
jobs_novembre_cancel = jobs_novembre[jobs_novembre[,1] == 'CANCELLED' , 2 ]
jobs_decembre_cancel = jobs_decembre[jobs_decembre[,1] == 'CANCELLED' , 2 ]
jobs_aout_cancel = jobs_aout[jobs_aout[,1] == 'CANCELLED' , 2 ]

cancelled_jobs = c(jobs_mars_cancel, jobs_avril_cancel, jobs_mai_cancel, jobs_juin_cancel, jobs_juillet_cancel, jobs_aout_cancel, 
                   jobs_septembre_cancel, jobs_octobre_cancel, jobs_novembre_cancel, jobs_decembre_cancel)

## failed
jobs_mars_fail = jobs_mars[jobs_mars[,1] == 'FAILED' , 2 ]
jobs_avril_fail = jobs_avril[jobs_avril[,1] == 'FAILED' , 2 ]
jobs_mai_fail = jobs_mai[jobs_mai[,1] == 'FAILED' , 2 ]
jobs_juin_fail = jobs_juin[jobs_juin[,1] == 'FAILED' , 2 ]
jobs_juillet_fail = jobs_juillet[jobs_juillet[,1] == 'FAILED' , 2 ]
jobs_septembre_fail = jobs_septembre[jobs_septembre[,1] == 'FAILED' , 2 ]
jobs_octobre_fail = jobs_octobre[jobs_octobre[,1] == 'FAILED' , 2 ]
jobs_novembre_fail = jobs_novembre[jobs_novembre[,1] == 'FAILED' , 2 ]
jobs_decembre_fail = jobs_decembre[jobs_decembre[,1] == 'FAILED' , 2 ]
jobs_aout_fail = jobs_aout[jobs_aout[,1] == 'FAILED' , 2 ]

failed_jobs = c(jobs_mars_fail, jobs_avril_fail, jobs_mai_fail, jobs_juin_fail, jobs_juillet_fail, jobs_aout_fail, 
                   jobs_septembre_fail, jobs_octobre_fail, jobs_novembre_fail, jobs_decembre_fail)

## out_of_memory
jobs_mars_oom = jobs_mars[jobs_mars[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_avril_oom = jobs_avril[jobs_avril[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_mai_oom = jobs_mai[jobs_mai[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_juin_oom = jobs_juin[jobs_juin[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_juillet_oom = jobs_juillet[jobs_juillet[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_septembre_oom = jobs_septembre[jobs_septembre[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_octobre_oom = jobs_octobre[jobs_octobre[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_novembre_oom = jobs_novembre[jobs_novembre[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_decembre_oom = jobs_decembre[jobs_decembre[,1] == 'OUT_OF_MEMORY' , 2 ]
jobs_aout_oom = jobs_aout[jobs_aout[,1] == 'OUT_OF_MEMORY' , 2 ]

out_of_memory_jobs = c(jobs_mars_oom, jobs_avril_oom, jobs_mai_oom, jobs_juin_oom, jobs_juillet_oom, jobs_aout_oom, 
                   jobs_septembre_oom, jobs_octobre_oom, jobs_novembre_oom, jobs_decembre_oom)

## timeout
jobs_mars_timeout = jobs_mars[jobs_mars[,1] == 'TIMEOUT' , 2 ]
jobs_avril_timeout = jobs_avril[jobs_avril[,1] == 'TIMEOUT' , 2 ]
jobs_mai_timeout = jobs_mai[jobs_mai[,1] == 'TIMEOUT' , 2 ]
jobs_juin_timeout = jobs_juin[jobs_juin[,1] == 'TIMEOUT' , 2 ]
jobs_juillet_timeout = jobs_juillet[jobs_juillet[,1] == 'TIMEOUT' , 2 ]
jobs_septembre_timeout = jobs_septembre[jobs_septembre[,1] == 'TIMEOUT' , 2 ]
jobs_octobre_timeout = jobs_octobre[jobs_octobre[,1] == 'TIMEOUT' , 2 ]
jobs_novembre_timeout = jobs_novembre[jobs_novembre[,1] == 'TIMEOUT' , 2 ]
jobs_decembre_timeout = jobs_decembre[jobs_decembre[,1] == 'TIMEOUT' , 2 ]
jobs_aout_timeout = jobs_aout[jobs_aout[,1] == 'TIMEOUT' , 2 ]

timeout_jobs = c(jobs_mars_timeout, jobs_avril_timeout, jobs_mai_timeout, jobs_juin_timeout, jobs_juillet_timeout, jobs_aout_timeout, 
                   jobs_septembre_timeout, jobs_octobre_timeout, jobs_novembre_timeout, jobs_decembre_timeout)


## Total jobs
total_jobs = c(nrow(mars), nrow(avril), nrow(mai), nrow(juin), nrow(juillet), nrow(aout), nrow(septembre) , nrow(octobre) , nrow(novembre) , nrow(decembre))





### Plot
# Completed jobs
ggplot(data.frame(completed_jobs),aes(seq_along(completed_jobs),completed_jobs, fill = seq_along(completed_jobs)))+
  geom_bar(stat="identity",show.legend = FALSE)+
  labs(y = "Jobs complétés", x = "2018")+
  scale_x_discrete(limits=seq_along(completed_jobs), labels=mois)+
  theme_bw()  







# combined graph job
# cancelled_jobs$name = "cancelled"
# completed_jobs$name = "completed"
# failed_jobs$name = "failed"
# out_of_memory_jobs$name = "out of memory"
# timeout_jobs$name = "timeout"

data_job = cbind(cancelled_jobs, completed_jobs, failed_jobs, out_of_memory_jobs, timeout_jobs)
colnames(data_job) = c("CANCELLED" , "COMPLETED" , "FAILED" , "OUT_OF_MEMORY" , "TIMEOUT" )
data_job = data.frame(data_job)
data_job$MONTH = mois;

ggplot(data_job, aes(1:10, fill = Status))+
  geom_bar(aes(1:10, COMPLETED, fill = "COMPLETED"), stat = "identity")+
  geom_bar(aes(1:10, FAILED, fill = "FAILED"), stat = "identity")+
  geom_bar(aes(1:10, CANCELLED, fill = "CANCELLED"), stat = "identity")+
  geom_bar(aes(1:10, OUT_OF_MEMORY, fill = "OUT_OF_MEMORY"), stat = "identity")+
  geom_bar(aes(1:10, TIMEOUT, fill = "TIMEOUT"), stat = "identity")+
  scale_fill_brewer(palette = "Set1")+
  scale_x_discrete(limits=data_job$MONTH)+
  labs(y = "Job count", x = "2018")+
  theme_bw()  





# Total jobs
# ggplot(data.frame(total_jobs),aes(seq_along(total_jobs),total_jobs, fill = seq_along(total_jobs)))+
#   geom_bar(stat="identity",show.legend = FALSE)+
#   labs(y = "Jobs", x = "2018")+
#   scale_x_discrete(limits=seq_along(total_jobs), labels=mois)+
#   theme_bw()  


# test
# ggplot(data.frame(total_jobs),aes(seq_along(total_jobs),total_jobs, fill = seq_along(total_jobs)))+
#   geom_bar(stat="identity",show.legend = FALSE)+
#   labs(y = "Jobs", x = "2018")+
#   scale_x_discrete(limits=seq_along(total_jobs), labels=mois)+
#   theme_bw() 