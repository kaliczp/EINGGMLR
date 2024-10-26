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
## Neighboring parcels second row
nostreetPol.df <- st_sf(data.frame(Selected = F, geom = polmult + c(0, parcellength)))
polmultnostreet.df <- rbind(polmult.df, nostreetPol.df)
## Add street
polmult.df <- rbind(polmultnostreet.df, st_sf(data.frame(Selected = F, geom = streetPol)))
polmult.df <- cbind(polmult.df, OBJ_FELS = c(rep("BD01", nrow(polmult.df)-1), "BC01"))
### Buildings generation
## Selected parcel width
selected.coord <- st_coordinates(polmult.df[which(polmult.df$Selected),]$geometry)[,"X"]
buildleft <- min(selected.coord) + sample(seq(0.5,2,by=0.2),1)
buildwidth <- 9.5 + sample(c(0, 0.5, 1), 1)
buildlength <- 10 + sample(c(0, 0.5, 1, 1.5, 2, 2.5), 1)
## Building polys
## First building
b1 <- rbind(c(buildleft,0),
            c(buildleft + buildwidth,0),
            c(buildleft + buildwidth, buildlength),
            c(buildleft, buildlength),
            c(buildleft,0))
buildpol1 <- st_polygon(list(b1))
## Second building
buildlow <- buildlength + sample(1:10,1)
buildwidth <- 5.5 + sample(c(0, 0.5, 1), 1)
buildlength <- sample(8:15,1)
b2 <- rbind(c(buildleft, buildlow),
            c(buildleft + buildwidth, buildlow),
            c(buildleft + buildwidth, buildlow + buildlength),
            c(buildleft, buildlow + buildlength),
            c(buildleft, buildlow))
buildpol2 <- st_polygon(list(b2))
## Put into one geometry
builpolmult <- st_sfc(buildpol1, buildpol2)
polmult.df <- rbind(polmult.df, st_sf(data.frame(Selected = T, OBJ_FELS = c("CA01", "CA06")), geometry = builpolmult))
## Rotate polys
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
polmult.df$geometry<-polmult.df$geometry*rot(studpos * pi/40) + c(864000, 100000)
## Add CRS
st_crs(polmult.df) <- 23700
## Text rotation angle
szovegszog <- studpos*180/40 - 90
szovegszog <- ifelse(szovegszog < 0, szovegszog + 360, szovegszog)
polmult.df <- cbind(polmult.df, IRANY = szovegszog)
streetangle <- szovegszog + 270
streetangle <- ifelse(streetangle > 360, streetangle - 360, streetangle)
polmult.df[grep("BC", polmult.df[, "OBJ_FELS", drop = TRUE]), "IRANY"] <- streetangle
aktfilename <- paste0(gsub(" ", "", students[studentnr,]), ".gml")
BuildNew(polmult.df, file = aktfilename, hrsz = sample(21:380,1))
}
