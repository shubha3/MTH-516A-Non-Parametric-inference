########30simulation0.2contamination#########
install.packages('ggplot2') #To Install
library(ggplot2)
library(lattice)
par(mfrow=c(1,2)) 
# Create data
gfg <- data.frame(x = c(98.4,46.4,10.76,84.90,97.4,97.4,11.43,84.93), 
                   Bootstrap = rep(c("Coverage-Intercept", "Length-Intercept","Coverage-Slope", "Length-Slope"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
ggplot(gfg,aes(x = Bootstrap, y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge",width=0.5)+labs(title="Comparison of coverage and length for n=30 and epsilon=0.2")

gfgg <- data.frame(Time = c(8.688,856.2), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
p <- ggplot(gfgg,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p + labs(title="Computation time(in seconds) for n=30 and epsilon=0.20")
#########################30simulation0.2contamination#########
# Create data
gfg1 <- data.frame(x = c(97.8,97.8,90.6,90.2,97.4,97.8,101,92.3), 
                   Bootstrap = rep(c("Coverage-Intercept", "Length-Intercept","Coverage-Slope", "Length-Slope"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))

# Create grouped barplot using ggplot2
ggplot(gfg1,aes(x = Bootstrap , y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge")+labs(title="Comparison of coverage and length for n=30 and epsilon=0.00")

gfgg1 <- data.frame(Time = c(11.65,14.34*60), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
p1 <- ggplot(gfgg1,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p1 + labs(title="Computation time(in seconds) for n=30 and epsilon=0.00")


############100simulation0.2contamination###############
gfg3 <- data.frame(x = c(99.1,0,5.8,46.6,98.2,98.2,5.9,46.6), 
                   grp = rep(c("Coverage-Intercept", "Length-Intercept","Coverage-Slope", "Length-Slope"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 
# Create grouped barplot using ggplot2
ggplot(gfg3,aes(x = grp, y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge")+labs(title="Comparison of coverage and length for n=100 and ep=0.2")

gfgg3 <- data.frame(Time = c(19.26,18.24*60), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 

p2 <- ggplot(gfgg3,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p2 + labs(title="Computation time(in seconds) for n=100 and epsilon=0.20")


############100simulation0contamination##############
gfg4 <- data.frame(x = c(98.2,99,52,51,98.7,98.8,53,51), 
                   grp = rep(c("Coverage-Intercept", "Length-Intercept","Coverage-Slope", "Length-Slope"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))

# Create grouped barplot using ggplot2
ggplot(gfg4,aes(x = grp, y =x, fill = subgroup)) +
geom_bar(stat = "identity", position = "dodge")+labs(title="Comparison of coverage and length n=100 and ep=0.00")


gfgg4 <- data.frame(Time = c(1.24*60,14.23*60), 
                   Bootstrap = rep(c("Computation Time"),each =2),
                   subgroup = c("Fast Bootstrap","Classical Bootstrap"))
 

p2 <- ggplot(gfgg4,aes(x = Bootstrap, y =Time, fill = subgroup)) + geom_bar(stat = "identity", position = "dodge",width=0.5)
p2 + labs(title="Computation time(in seconds) for n=100 and epsilon=0.00")



