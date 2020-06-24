expedia=read.csv("D:\\Pengantar Data Mining\\test.csv\\test.csv")

dim(expedia)
str(expedia)
summary(expedia)
expedia[1000000,]
expedia$srch_adults_cnt
hist(expedia$srch_adults_cnt)
hist(expedia$srch_children_cnt)
hist(expedia$srch_rm_cnt)
hist(expedia$user_location_country)

table(expedia$srch_adults_cnt,expedia$srch_children_cnt)
