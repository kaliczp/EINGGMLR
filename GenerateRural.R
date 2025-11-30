library(sf)

students <- read.table("export.csv", sep = ";", head = TRUE)

studentnr <- 20


{
    parcelwidth <- sample(seq(140,200,by=0.1),1)
    parcellength <- sample(seq(750,900,by=0.1),1)
    ## First row of parcels
    p1 <- rbind(c(0,0), c(parcelwidth,0),
                c(parcelwidth,parcellength), c(0,parcellength), c(0,0))
    pol1 <- st_polygon(list(p1))
    if(megoszt) {
        pol2 <- pol1 + c(parcelwidth, 0,
                         2 * parcelwidth, 0,
                         2 * parcelwidth, 0,
                         parcelwidth, 0,
                         parcelwidth, 0)
    } else {
        pol2 <- pol1 + rep(c(parcelwidth, 0), 5)
    }
        if(megoszt){
        pol3 <- pol2 +  2 * rep(c(parcelwidth, 0), 5)
    } else {
        pol3 <- pol2 +  rep(c(parcelwidth, 0), 5)
    }
    polmult <- st_sfc(pol1,
                      pol2,
                      pol3 +
                  c(0,0,round(rnorm(1,sd = 0.1),2),0,round(rnorm(1, sd = 0.1),2),0,0,0,0,0))
    polmult.df <- st_sf(data.frame(Selected = c(F,T,F), geom=polmult))
    ## Parcel ID
    hrsz <- sample(60:580,1)
    hrsz[2:3] <- hrsz[1] + 1:2
    if(megoszt)
        hrsz[1] <- paste0(hrsz[1], "/2")
    polmult.df[, "HRSZ"] <- hrsz
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
    neighhrsz <- as.numeric(hrsz[2]) + sample(streetLeftRight[2]:(streetLeftRight[2]+100),1)
    neighhrsz[2:3] <- neighhrsz[1] + 1:2
    if(megoszt)
        neighhrsz[1] <- paste0(neighhrsz[1], "/2")
    nostreetPol.df <- st_sf(data.frame(Selected = F, HRSZ = neighhrsz, geom = polmult + c(0, parcellength)))
    polmultnostreet.df <- rbind(polmult.df, nostreetPol.df)
    ## Add street
    strhrsz <- as.numeric(hrsz[2]) - streetLeftRight[1] - 1
    polmult.df <- rbind(polmultnostreet.df, st_sf(data.frame(Selected = F, HRSZ = strhrsz, geom = streetPol)))
    polmult.df <- cbind(polmult.df, OBJ_FELS = c(rep("BD01", nrow(polmult.df)-1), "BC01"))
    ### Rotate polys
    ## In case of measure line
    if(MeasureLine) {
        st_cast(polmult.df, "MULTIPOINT")
    }
    polmult.df$geometry<-polmult.df$geometry*rot(studpos * pi/40) + c(864000, 100000)
### Text rotation angle
    szovegszog <- studpos*180/40 - 90
    szovegszog <- ifelse(szovegszog < 0, szovegszog + 360, szovegszog)
    polmult.df <- cbind(polmult.df, IRANY = szovegszog)
    streetangle <- szovegszog + 270
    streetangle <- ifelse(streetangle > 360, streetangle - 360, streetangle)
    polmult.df[kozterLine, "IRANY"] <- streetangle
    ## Add CRS
    st_crs(polmult.df) <- 23700
    aktfilename <- paste0("Telkek/",gsub(" ", "", students[studentnr,]))
### Export gml
    BuildNew(polmult.df, file = paste0(aktfilename, ".gml"))
}
