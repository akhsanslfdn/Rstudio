library(MASS)
painters

#frequensi dist
summary(painters$School)

school=painters$School
school
school.freq=table(school)
school.freq
cbind(school.freq)

#relative freq dist
nrow(painters)
school.relfreq=school.freq/nrow(painters)
school.relfreq
cbind(school.freq, school.relfreq)

#barchart
barplot(school.freq)
barplot(school.freq, col=c("green","red","blue","black","grey","purple","violet","brown"), main = "Bar Chart of School Painters")

#pie chart
pie(school.freq ,col=c("green","red","blue","black","grey","purple","violet","brown"), main = "Bar Chart of School Painters")

#category statistics
a_school=school=="A"
a_school
a_painters=painters[a_school,]
a_painters
mean(a_painters$Composition)
mean.all=tapply(painters$Composition, painters$School, mean)
mean.all
cbind(mean.all)

