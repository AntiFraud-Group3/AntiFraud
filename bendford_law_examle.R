# load libraries
p_load(gtrendsR)
p_load(reshape2)
p_load(dplyr)
p_load(benford.analysis)
# read data
data = read.csv("data/co-est2019-alldata.csv", header = T)
# filter out columns
data_filt = data %>% filter(COUNTY != 0) %>% select(c(STNAME, CTYNAME, CENSUS2010POP))
# perform benford analysis
trends = benford(data_filt$CENSUS2010POP, number.of.digits = 1, discrete = T, sign = "positive") 
trends
# plot results
plot(trends)
