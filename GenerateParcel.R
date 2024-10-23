library(sf)

students <- read.table("export.csv", sep = ";")

for(studentnr in 1:nrow(students)){
studpos <- studentnr + 10
parcelwidth <- sample(seq(14,20,by=0.1),1)
parcellength <- sample(seq(75,90,by=0.1),1)
## First row of parcels
p1 <- rbind(c(0,0), c(parcelwidth,0),
            c(parcelwidth,parcellength), c(0,parcellength), c(0,0))
pol1 <- st_polygon(list(p1))
polmult <- st_sfc(pol1,
                  pol1 + rep(c(parcelwidth, 0), 5),
                  pol1 +  2 * rep(c(parcelwidth, 0), 5) +
                  c(0,0,round(rnorm(1,sd = 0.1),2),0,round(rnorm(1, sd = 0.1),2),0,0,0,0,0))
polmult.df <- st_sf(data.frame(Selected = c(F,T,F), geom=polmult))
## Street gen
streetcoords <- st_coordinates(polmult)
streetcoords <- unique(streetcoords[streetcoords[,"Y"] < 1,c("X", "Y")])
endParcel <- streetcoords[nrow(streetcoords), "X"]
streetLeftRight <- sample(3:50,2)
streetOtherSide <- sum(c(length(polmult), streetLeftRight))
streetcoords <- rbind(matrix(rep(0, 2*streetLeftRight[1]), nr = streetLeftRight[1]),
                      streetcoords,
                      matrix(rep(0, 2*streetLeftRight[2]), nr = streetLeftRight[2])
                      )
## Left replacement
streetcoords[1:streetLeftRight[1],"X"] <- seq(-streetLeftRight[1] * parcelwidth,
                                              -parcelwidth, parcelwidth)
## Right replacement
firstRight <- streetLeftRight[1] + length(polmult) + 2 # plus one side and the next
streetcoords[firstRight:nrow(streetcoords),"X"] <- seq(endParcel + parcelwidth,
                                                       by = parcelwidth,
                                                       length = nrow(streetcoords) - firstRight + 1)
streetcoordsOtherSide <- streetcoords[nrow(streetcoords):1,]
streetcoordsOtherSide[,"Y"] <- sample(seq(-25,-15,by=0.1),1)
streetcoordsOK <- rbind(streetcoords, streetcoordsOtherSide, streetcoords[1,])
streetPol <- st_sfc(st_polygon(list(streetcoordsOK)))
streetPol.df <- st_sf(data.frame(Selected = F, geom = polmult + c(0, parcellength)))
## Neighboring parcels second row
nostreetPol.df <- st_sf(data.frame(Selected = F, geom = polmult + c(0, parcellength)))
polmultnostreet.df <- rbind(polmult.df, nostreetPol.df)
## Add street
polmult <- c(polmultnostreet, streetPol)
polmult.df <- rbind(polmultnostreet.df, st_sf(data.frame(Selected = F, geom = streetPol)))
polmult.df <- cbind(polmult.df, OBJ_FELS = c(rep("BD01", nrow(polmult.df)-1), "BC01"))
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
polmulttr <- polmult*rot(studpos * pi/40) + c(864000, 100000)
polmult.df$geometry<-polmult.df$geometry*rot(studpos * pi/40) + c(864000, 100000)
st_crs(polmult.df) <- 23700
szovegszog <- studpos*180/40 - 90
szovegszog <- ifelse(szovegszog < 0, szovegszog + 360, szovegszog)
polmult.df <- cbind(polmult.df, IRANY = szovegszog)
streetangle <- szovegszog + 270
streetangle <- ifelse(streetangle > 360, streetangle - 360, streetangle)
polmult.df[grep("BC", polmult.df[, "OBJ_FELS", drop = TRUE]), "IRANY"] <- streetangle
aktfilename <- paste0(gsub(" ", "", students[studentnr,]), ".gml")
BuildNew(polmulttr, currpoly = 2, street = 7, file = aktfilename, hrsz = sample(21:380,1),
         angle = szovegszog)
}
