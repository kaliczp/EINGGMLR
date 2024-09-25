library(sf)

# studpos 0:
studpos <- 30
width <- 15
length <-80
p1 <- rbind(c(0,0), c(width,0), c(width,length), c(0,length), c(0,0))
pol1 <- st_polygon(list(p1))
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
pol1tr <- pol1*rot(studpos * pi/40) + + c(460000, 165800)
BuildNew(pol1tr)
