
###Histogram

library(RColorBrewer)

dhoni <- read.csv("Dhoni_ODI_runs.csv")
View(dhoni)
dhoni_runs <- dhoni[,2]
dhoni_runs1 <- dhoni[1:100, 2]
hist(dhoni_runs)

hist(dhoni_runs, 
     main="Dhoni ODI Runs", 
     xlab="Runs Range",
     ylab = "Freq",
     border="blue", 
     col="green",
     xlim=c(0,200),
     ylim = c(0,120),
     las=2, 
     breaks=20)

####Line Chart
plot(dhoni_runs1, type = "o")

plot(dhoni_runs, 
     type = "o",
     col="blue",
     xlab = "Innings",
     ylab = "Runs")


### BoxPlot
boxplot(dhoni_runs,
        xlab = "Innings",
        ylab = "Runs", 
        main = "Dhoni",
        notch = TRUE, 
        varwidth = TRUE, 
        col = c("green","yellow","purple"))

#### Scatterplot

plot(dhoni$innings, dhoni$Runs,
     xlab = "Innings",
     ylab = "Runs")


