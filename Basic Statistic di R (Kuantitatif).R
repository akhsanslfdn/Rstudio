faithful
head(faithful)

#freq dist
duration=faithful$eruptions
duration
range(duration)
breaks=seq(1.5, 5.5, by=0.5) # barisan 1,5 sampai 5,5 dengan interval 0,5
breaks
duration.cut=cut(duration, breaks, right=FALSE)
duration.cut
duration.freq=table(duration.cut)
duration.freq
cbind(duration.freq)

#histogram
hist(duration)
hist(duration, ylim=c(0, 80))

barplot(duration.freq)

#relatif freq.dist
nrow(faithful)
duration.relfreq=duration.freq/nrow(faithful)
duration.relfreq
cbind(duration.freq, duration.relfreq)

#cumulative freq.dist
duration.cumfreq=cumsum(duration.freq)
duration.cumfreq
cbind(duration.freq, duration.relfreq, duration.cumfreq)

#ogive
cumfreq0=c(0, duration.cumfreq)
cumfreq0
plot(breaks, cumfreq0)
lines(breaks, cumfreq0)

#scatterplot
waiting=faithful$waiting
plot(duration, waiting)

#stem & leafplot
stem(duration)

