
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
# https://www.datanovia.com/en/fr/blog/exemples-de-ggplot/
# http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization
#
# https://www.data-to-art.com/
# https://towardsdatascience.com/themes-to-spice-up-visualizations-with-ggplot2-3e275038dafa
# https://github.com/erikgahner/awesome-ggplot2

options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")
# midwest <- read.csv("http://goo.gl/G1K41K")

# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal)) 
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()+ geom_line()
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()+ geom_smooth(method="lm") 

g+ xlim(c(0, 0.1)) + ylim(c(0, 1000000))
g

g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
g1

g1 + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# or

g1 + ggtitle("Area Vs Population", subtitle="From midwest dataset") + xlab("Area") + ylab("Population")


ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue", size=3) +   # Set static color and size for points
  geom_smooth(method="lm", col="firebrick") +  # change the color of line
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")


gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)


gg + theme(legend.position="None")  # remove legend
gg + scale_colour_brewer(palette = "Set1")  # change color palette
