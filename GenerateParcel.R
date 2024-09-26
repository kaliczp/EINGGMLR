library(sf)

students <- read.table("export.csv", sep = ";")

for(studentnr in 1:nrow(students)){
studpos <- studentnr + 10
parcelwidth <- sample(seq(14,20,by=0.1),1)
parcellength <- sample(seq(75,90,by=0.1),1)
p1 <- rbind(c(0,0), c(parcelwidth,0),
            c(parcelwidth,parcellength), c(0,parcellength), c(0,0))
pol1 <- st_polygon(list(p1))
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
pol1tr <- pol1*rot(studpos * pi/40) + + c(460000, 165800)
aktfilename <- paste0(gsub(" ", "", students[studentnr,]), ".gml")
BuildNew(pol1tr, file = aktfilename, hrsz = sample(11:280,1))
}
