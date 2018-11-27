View(mtcars)
mtcars[,1:2]

dotchart(mtcars$mpg,labels=row.names(mtcars),
         cex=.9, color = "blue",gcolor = "green",
         main="Gas Milage by Cars", xlab="Miles/Gallon")

#dot plot by group
myCars <- mtcars[order(mtcars$mpg),] # ordering the
myCars$cyl <- factor(myCars$cyl) # making cyl into
myCars$color[myCars$cyl==4] <- "red" # assigning
myCars$color[myCars$cyl==6] <- "blue" # assigning
myCars$color[myCars$cyl==8] <- "purple" # assigning
View(myCars)
dotchart(myCars$mpg,labels=row.names(myCars),
         cex=.9, groups= myCars$cyl,
         main="Gas Milage by Cars",
         xlab="MPG", color=myCars$color)


counts <- table(mtcars$gear)
class(counts)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")

counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))

counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by
        Gears and V/S",
        xlab="Number of Gears",
        col=c("blue","orange"),
        legend = rownames(counts))

counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears
        and V/S",
        xlab="Number of Gears",
        col=c("blue","pink"),
        
        legend = rownames(counts),
        beside = T)

slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany",
          "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany",
          "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to
lbls <- paste(lbls,"%",sep="") # add % to
pie(slices,labels = lbls,
    col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

library(plotrix)
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany",
          "France")
pie3D(slices,labels=lbls,explode=0.05,
      main="Pie Chart of Countries ")

hist(mtcars$mpg)
