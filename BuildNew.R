BuildNew <- function(fulldf, file = NULL, adminarea = NULL) {
    require(XML)
    require(sf)
    ## To prevent scientific notation
    options(scipen=999)
    ## Get DAT code and class
    DATcode <- as.vector(fulldf[, "OBJ_FELS", drop = TRUE])
    DATclass <- substr(DATcode, start = 1, stop = 1 )
    ## Building?
    if(any(DATclass == "C")) {
        epuletpoly <- fulldf[DATclass == "C",]
        poly <- fulldf[!DATclass == "C",]
        modDATclass <- DATclass[!DATclass == "C"]
    }
    ## Text?
    if(any(DATclass == "T")) {
        texts <- fulldf[DATclass == "T",]
        poly <- poly[!modDATclass == "T",]
    }
    ## Selected poly
    currpoly <- which(poly$Selected)
    ## CRS
    srsName <- paste0("urn:x-ogc:def:crs:",st_crs(poly)$input)
    ## Number of polys
    nrpoly <- nrow(poly)
    ## Generate fids
    allfid <- round(abs(rnorm(1))*10^14) +
        round(abs(rnorm(nrpoly, sd = 0.01)*10^4))
    currfid <- allfid[currpoly]
    doc <- newXMLDoc()
    ## Meta data creation
    ns <- c(eing = "eing.foldhivatal.hu",
            gml = "http://www.opengis.net/gml",
            xlink = "http://www.w3.org/1999/xlink",
            xs="http://www.w3.org/2001/XMLSchema")
    newgml <- newXMLNode("FeatureCollection", namespaceDefinitions = ns,
                         namespace = "gml", doc = doc)
    ## Create MetaData chidren
    metaprop <-  newXMLNode("metaDataProperty", parent = newgml, namespace = "gml")
    genericmeta <-  newXMLNode("GenericMetaData", parent = metaprop, namespace = "gml")
    metalist <-  newXMLNode("MetaDataList", parent = genericmeta)
    newXMLNode("gmlID", "691da01c-7911-45a7-b831-bc594bfaca16", parent = metalist)
    newXMLNode("gmlExportDate", round(as.numeric(Sys.time())*1000), parent = metalist)
    newXMLNode("gmlGeobjIds", currfid, parent = metalist)
    newXMLNode("xsdVersion", "2.3", parent = metalist)
### Data processing
    ## First create featrue Members node
    metadataNode <- newXMLNode("featureMembers", parent = newgml, namespace = "gml")
    ## Only two cases OK; 1 or more than 3
    if(nrpoly > 2) {
        ## Selected poly last in the order because point generation
        orderedpoly <- c(1:(currpoly-1), (currpoly+1):nrpoly, currpoly)
    } else {
        orderedpoly <- currpoly
    }
    for(actualpoly in orderedpoly) {
        ## Coordinates prepcocessing
        coords.matrix <- round(st_coordinates(poly[actualpoly,])[, c("X","Y")], 2)
        coords <- as.numeric(t(coords.matrix))
        ## Remove duplicated points
        coords.matrix <- coords.matrix[!duplicated(coords.matrix),]
        ## Poly area calcualtion
        if(is.null(adminarea)) {
            ## Without error
            adminareagen <- round(st_area(poly[actualpoly,]))
        }
### Create features
        ## Create a parcel node
        parcelNode = newXMLNode("FOLDRESZLETEK", parent=metadataNode, namespace = "eing")
        addAttributes(parcelNode, "gml:id" = paste0("fid-", allfid[actualpoly]))
        parcelBounded <- newXMLNode("boundedBy", parent=parcelNode, namespace = "gml")
        parcelEnvelope <- newXMLNode("Envelope", parent=parcelBounded, namespace = "gml")
        addAttributes(parcelEnvelope, srsDimension = 2, srsName = srsName) 
        addChildren(parcelEnvelope, newXMLNode("lowerCorner", paste(min(coords.matrix[,1]), min(coords.matrix[,2])), namespace = "gml"))
        addChildren(parcelEnvelope, newXMLNode("upperCorner", paste(max(coords.matrix[,1]), max(coords.matrix[,2])), namespace = "gml"))
        addChildren(parcelNode, newXMLNode("GEOBJ_ID", allfid[actualpoly], namespace = "eing"))
        actDATcode <- as.character(poly[actualpoly,"OBJ_FELS", drop = TRUE])
        addChildren(parcelNode, newXMLNode("OBJ_FELS", actDATcode, namespace = "eing"))
        addChildren(parcelNode, newXMLNode("RETEG_ID", 20, namespace = "eing"))
        addChildren(parcelNode, newXMLNode("RETEG_NEV", "Földrészletek" , namespace = "eing"))
        addChildren(parcelNode, newXMLNode("TELEPULES_ID", 3400, namespace = "eing"))
        addChildren(parcelNode, newXMLNode("FEKVES", 3719, namespace = "eing")) # Belter
        if(actDATcode %in% c("BC01", "BC02")) {
            streethrsz <- st_drop_geometry(poly[actualpoly, "HRSZ", drop = TRUE])
            addChildren(parcelNode, newXMLNode("HRSZ", streethrsz, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("FELIRAT", paste0(
                                                              "(",
                                                              streethrsz,
                                                              ")"),
                                               namespace = "eing"))
        } else {
            parcelhrsz <- st_drop_geometry(poly[actualpoly, "HRSZ", drop = TRUE])
            addChildren(parcelNode, newXMLNode("HRSZ", parcelhrsz, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("FELIRAT", parcelhrsz, namespace = "eing"))
        }
        addChildren(parcelNode, newXMLNode("SZINT", 0, namespace = "eing"))
        ## Text angle
        textangle <- as.character(poly[actualpoly,"IRANY", drop = TRUE])
        addChildren(parcelNode, newXMLNode("IRANY", textangle, namespace = "eing"))
        addChildren(parcelNode, newXMLNode("MUVEL_AG", 4557, namespace = "eing")) # Kivett
        addChildren(parcelNode, newXMLNode("JOGI_TERULET", adminareagen, namespace = "eing"))
        parcelGeometry <- newXMLNode("geometry", parent=parcelNode, namespace = "eing")
        parcelPolygon <- newXMLNode("Polygon", parent=parcelGeometry, namespace = "gml")
        addAttributes(parcelPolygon, srsDimension = 2, srsName = srsName) 
        parcelExterior <- newXMLNode("exterior", parent=parcelPolygon, namespace = "gml")
        parcelRing <- newXMLNode("LinearRing", parent=parcelExterior, namespace = "gml")
        addAttributes(parcelRing, srsDimension = 2)
        addChildren(parcelRing, newXMLNode("posList", paste(coords, collapse = " "), namespace = "gml"))
    }
### Building
    if(any(DATclass == "C")) {
        warning("Buildings exist!")
        buildnum <- nrow(epuletpoly)
        buildfid <- currfid + round(abs(rnorm(buildnum))*10)
        for(actbuildingpoly in 1:buildnum) {
            ## Coordinates prepcocessing
            buildcoords.matrix <- round(st_coordinates(epuletpoly[actbuildingpoly,])[, c("X","Y")], 2)
            buildcoords <- as.numeric(t(buildcoords.matrix))
            ## Remove duplicated points
            buildcoords.matrix <- buildcoords.matrix[!duplicated(buildcoords.matrix),]
            ## Poly area calcualtion
            if(is.null(adminarea)) {
                ## Without error
                adminareagen <- round(st_area(epuletpoly[actbuildingpoly,]))
            }
            ## Create a parcel node
            parcelNode = newXMLNode("EPULETEK", parent=metadataNode, namespace = "eing")
            addAttributes(parcelNode, "gml:id" = paste0("fid-", buildfid[actbuildingpoly]))
            parcelBounded <- newXMLNode("boundedBy", parent=parcelNode, namespace = "gml")
            parcelEnvelope <- newXMLNode("Envelope", parent=parcelBounded, namespace = "gml")
            addAttributes(parcelEnvelope, srsDimension = 2, srsName = srsName) 
            addChildren(parcelEnvelope, newXMLNode("lowerCorner",
                                                   paste(min(buildcoords.matrix[,1]),
                                                         min(buildcoords.matrix[,2])),
                                                   namespace = "gml"))
            addChildren(parcelEnvelope, newXMLNode("upperCorner",
                                                   paste(max(buildcoords.matrix[,1]),
                                                         max(buildcoords.matrix[,2])),
                                                   namespace = "gml"))
            addChildren(parcelNode, newXMLNode("GEOBJ_ID", buildfid[actbuildingpoly],
                                               namespace = "eing"))
            actDATcode <- as.character(epuletpoly[actbuildingpoly,"OBJ_FELS", drop = TRUE])
            addChildren(parcelNode, newXMLNode("OBJ_FELS", actDATcode, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("RETEG_ID", 8, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("RETEG_NEV", "Épület" , namespace = "eing"))
            addChildren(parcelNode, newXMLNode("TELEPULES_ID", 3400, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("FEKVES", 3719, namespace = "eing")) # Belter
            buildhrsz <- st_drop_geometry(epuletpoly[actbuildingpoly, "HRSZ", drop = TRUE])
            addChildren(parcelNode, newXMLNode("HRSZ", buildhrsz, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("FELIRAT", paste(actbuildingpoly,"ép."),
                                               namespace = "eing"))
            addChildren(parcelNode, newXMLNode("SZINT", 0, namespace = "eing"))
            ## Text angle
            textangle <- as.character(poly[actualpoly,"IRANY", drop = TRUE])
            addChildren(parcelNode, newXMLNode("IRANY", textangle, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("SORSZAM", actbuildingpoly, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("JOGI_TERULET", adminareagen, namespace = "eing"))
            addChildren(parcelNode, newXMLNode("FRSZ_ID", allfid[currpoly], namespace = "eing"))
            parcelGeometry <- newXMLNode("geometry", parent=parcelNode, namespace = "eing")
            parcelPolygon <- newXMLNode("Polygon", parent=parcelGeometry, namespace = "gml")
            addAttributes(parcelPolygon, srsDimension = 2, srsName = srsName) 
            parcelExterior <- newXMLNode("exterior", parent=parcelPolygon, namespace = "gml")
            parcelRing <- newXMLNode("LinearRing", parent=parcelExterior, namespace = "gml")
            addAttributes(parcelRing, srsDimension = 2)
            addChildren(parcelRing, newXMLNode("posList", paste(buildcoords, collapse = " "), namespace = "gml"))
            ## Add buildcoords.matrix coords.matrix
            coords.matrix <- rbind(coords.matrix, buildcoords.matrix)
        }

    }
### Points
    ## Random point geneeration related to original
    currfidother <- currfid + round(abs(rnorm(1))*10^4)
    ## Address coordinate if residential area non-public
    if(DATcode[currpoly] == "BD01") {
        suppressWarnings( # Prevent known warning about attributes
            currcentroid <- st_centroid(poly[currpoly,])
        )
        addresscoordpoint <- round(st_coordinates(currcentroid))
        pointNode <- newXMLNode("CIMKOORDINATA", parent=metadataNode, namespace = "eing")
        addAttributes(pointNode, "gml:id" = paste0("fid-", currfidother))
        pointBounded <- newXMLNode("boundedBy", parent=pointNode, namespace = "gml")
        pointEnvelope <- newXMLNode("Envelope", parent=pointBounded, namespace = "gml")
        addAttributes(pointEnvelope, srsDimension = 2, srsName = srsName)
        addChildren(pointEnvelope, newXMLNode("lowerCorner", paste(addresscoordpoint, collapse = " "), namespace = "gml"))
        addChildren(pointEnvelope, newXMLNode("upperCorner", paste(addresscoordpoint, collapse = " "), namespace = "gml"))
        addChildren(pointNode, newXMLNode("GEOBJ_ID", currfidother, namespace = "eing"))
        addChildren(pointNode, newXMLNode("OBJ_FELS", "AD01", namespace = "eing"))
        addChildren(pointNode, newXMLNode("RETEG_ID", 52, namespace = "eing"))
        addChildren(pointNode, newXMLNode("RETEG_NEV", "Címkoordináták" , namespace = "eing"))
        addChildren(pointNode, newXMLNode("TELEPULES_ID", 3400, namespace = "eing"))
        addChildren(pointNode, newXMLNode("HRSZ", parcelhrsz, namespace = "eing"))
        addChildren(pointNode, newXMLNode("FELIRAT", 1, namespace = "eing"))
        addChildren(pointNode, newXMLNode("SZINT", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("IRANY", textangle, namespace = "eing"))
        addChildren(pointNode, newXMLNode("PONTSZAM", 1, namespace = "eing"))
        addChildren(pointNode, newXMLNode("PONTKOD", 5411, namespace = "eing"))
        addChildren(pointNode, newXMLNode("JELKULCS", 36, namespace = "eing"))
        addChildren(pointNode, newXMLNode("FRSZ_ID", currfid, namespace = "eing"))
        pointGeometry <- newXMLNode("geometry", parent=pointNode, namespace = "eing")
        pointPoint <- newXMLNode("Point", parent=pointGeometry, namespace = "gml")
        addAttributes(pointPoint, srsDimension = 2, srsName = srsName)
        addChildren(pointPoint, newXMLNode("pos", paste(addresscoordpoint, collapse = " "), namespace = "gml"))
    }
### Points generation
    ## Initial point id
    pontszam <- 52421
    ## Points assigned to the polygon
    for(actualpoints in 1:nrow(coords.matrix)) {
        currfidother <- currfid + sample(1:5, 1)
        actualpoint <- coords.matrix[actualpoints,]
        pointNode <- newXMLNode("RESZLETPONTOK", parent=metadataNode, namespace = "eing")
        addAttributes(pointNode, "gml:id" = paste0("fid-", currfidother))
        pointBounded <- newXMLNode("boundedBy", parent=pointNode, namespace = "gml")
        pointEnvelope <- newXMLNode("Envelope", parent=pointBounded, namespace = "gml")
        addAttributes(pointEnvelope, srsDimension = 2, srsName = srsName)
        addChildren(pointEnvelope, newXMLNode("lowerCorner", paste(actualpoint, collapse = " "), namespace = "gml"))
        addChildren(pointEnvelope, newXMLNode("upperCorner", paste(actualpoint, collapse = " "), namespace = "gml"))
        addChildren(pointNode, newXMLNode("GEOBJ_ID", currfidother, namespace = "eing"))
        if(actualpoints < 3) {
            addChildren(pointNode, newXMLNode("OBJ_FELS", "AC01", namespace = "eing"))
        } else {
            addChildren(pointNode, newXMLNode("OBJ_FELS", "AC02", namespace = "eing"))
        }
        addChildren(pointNode, newXMLNode("RETEG_ID", 6, namespace = "eing"))
        addChildren(pointNode, newXMLNode("RETEG_NEV", "Részletpontok" , namespace = "eing"))
        addChildren(pointNode, newXMLNode("TELEPULES_ID", 3400, namespace = "eing"))
        addChildren(pointNode, newXMLNode("FEKVES", 3719, namespace = "eing"))
        addChildren(pointNode, newXMLNode("HRSZ", namespace = "eing"))
        addChildren(pointNode, newXMLNode("FELIRAT", pontszam, namespace = "eing"))
        addChildren(pointNode, newXMLNode("SZINT", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("IRANY", textangle, namespace = "eing"))
        addChildren(pointNode, newXMLNode("MAGASSAG", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("PONTSZAM", pontszam, namespace = "eing"))
        if(actualpoints < 3) {
            addChildren(pointNode, newXMLNode("PONTKOD", 4195, namespace = "eing"))
        } else {
            if(actualpoints < 5) {
                addChildren(pointNode, newXMLNode("PONTKOD", 4295, namespace = "eing"))
            } else {
                addChildren(pointNode, newXMLNode("PONTKOD", 4236, namespace = "eing"))
            }
        }
        addChildren(pointNode, newXMLNode("JELKULCS", 0, namespace = "eing"))
        pointGeometry <- newXMLNode("geometry", parent=pointNode, namespace = "eing")
        pointPoint <- newXMLNode("Point", parent=pointGeometry, namespace = "gml")
        addAttributes(pointPoint, srsDimension = 2, srsName = srsName)
        addChildren(pointPoint, newXMLNode("pos", paste(actualpoint, collapse = " "), namespace = "gml"))
        pontszam <- pontszam + sample(1:5, 1)
    }
### Text preocessing
    if(any(DATclass == "T")) {
        warning("Text exist!")
        currfidother <- currfid + sample(100:110, 1)
        actualtext <- texts[1,]
        textpoint <- round(st_coordinates(actualtext),2)
        textNode <- newXMLNode("FELIRATOK", parent=metadataNode, namespace = "eing")
        addAttributes(pointNode, "gml:id" = paste0("fid-", currfidother))
        pointBounded <- newXMLNode("boundedBy", parent=textNode, namespace = "gml")
        pointEnvelope <- newXMLNode("Envelope", parent=pointBounded, namespace = "gml")
        addAttributes(pointEnvelope, srsDimension = 2, srsName = srsName)
        addChildren(pointEnvelope, newXMLNode("lowerCorner", paste(textpoint, collapse = " "), namespace = "gml"))
        addChildren(pointEnvelope, newXMLNode("upperCorner", paste(textpoint, collapse = " "), namespace = "gml"))
        addChildren(textNode, newXMLNode("GEOBJ_ID", currfidother, namespace = "eing"))
        addChildren(textNode, newXMLNode("OBJ_FELS", actualtext$OBJ_FELS, namespace = "eing"))
        addChildren(textNode, newXMLNode("RETEG_ID", 2, namespace = "eing"))
        addChildren(textNode, newXMLNode("RETEG_NEV", "Feliratok" , namespace = "eing"))
        addChildren(textNode, newXMLNode("TELEPULES_ID", 3400, namespace = "eing"))
        actualHRSZ <- st_drop_geometry(actualtext[,"HRSZ", drop = TRUE])
        if(actualHRSZ != "") {
            addChildren(textNode, newXMLNode("HRSZ", actualHRSZ, namespace = "eing"))
        } else {
            addChildren(textNode, newXMLNode("HRSZ", namespace = "eing"))
        }
        addChildren(textNode, newXMLNode("FELIRAT",
                                         st_drop_geometry(actualtext[, "FELIRAT", drop = TRUE]),
                                         namespace = "eing"))
        addChildren(textNode, newXMLNode("SZINT", 0, namespace = "eing"))
        addChildren(textNode, newXMLNode("IRANY",
                                         st_drop_geometry(actualtext[, "IRANY", drop = TRUE]),
                                         namespace = "eing"))
        addChildren(textNode, newXMLNode("FRSZ_ID", currfid, namespace = "eing"))
        pointGeometry <- newXMLNode("geometry", parent=textNode, namespace = "eing")
        pointPoint <- newXMLNode("Point", parent=pointGeometry, namespace = "gml")
        addAttributes(pointPoint, srsDimension = 2, srsName = srsName)
        addChildren(pointPoint, newXMLNode("pos", paste(textpoint, collapse = " "), namespace = "gml"))
    }
### Save gml
    if(is.null(file)) {
        saveXML(doc, encoding = "UTF-8")
    } else {
        cat(saveXML(doc, prefix='<?xml version="1.0" encoding="UTF-8" standalone="no"?>'), file = file)
    }
}
