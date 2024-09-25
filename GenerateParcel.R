library(sf)

studpos <- 30
parcelwidth <- sample(seq(14,20,by=0.1),1)
parcellength <- sample(seq(75,90,by=0.1),1)
p1 <- rbind(c(0,0), c(parcelwidth,0),
            c(parcelwidth,parcellength), c(0,parcellength), c(0,0))
pol1 <- st_polygon(list(p1))
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
pol1tr <- pol1*rot(studpos * pi/40) + + c(460000, 165800)
BuildNew(pol1tr, hrsz = sample(11:280,1))
