BuildNew <- function(coords, file = "gmlwithmeta.gml", currfid = round(abs(rnorm(1))*10^14)) {
    require(XML)
    srsName <- "urn:x-ogc:def:crs:EPSG:23700"
    ## Coordinates prepcocessing
    coords.matrix <- matrix(coords, ncol = 2, byrow = TRUE)
    ## Remove duplicated points
    coords.matrix <- coords.matrix[!duplicated(coords.matrix),]
    ## Meta data creation
    newgml <- xmlTree("gml:FeatureCollection", namespaces = list(eing = "eing.foldhivatal.hu",
                                                             gml = "http://www.opengis.net/gml",
                                                             xlink = "http://www.w3.org/1999/xlink",
                                                             xs="http://www.w3.org/2001/XMLSchema"))
    newgml$setNamespace("gml")
    newgml$addNode("metaDataProperty", close = FALSE)
    newgml$addNode("GenericMetaData", close = FALSE)
    newgml$setNamespace(NULL)
    newgml$addNode("MetaDataList", close = FALSE)
    newgml$addNode("gmlID", "691da01c-7911-45a7-b831-bc594bfaca16")
    newgml$addNode("gmlExportDate", round(as.numeric(Sys.time())*1000))
    newgml$addNode("gmlGeobjIds", currfid)
    newgml$addNode("xsdVersion", 2.3)
    newgml$closeNode()
    newgml$closeNode()
    ## Create gml
    gmlwithmeta <- xmlTreeParse(saveXML(newgml), useInternalNodes = T)
    root <- xmlRoot(gmlwithmeta)
    metadataNode <- newXMLNode("featureMembers", parent = root, namespace = "gml")
    ## Create a parcel node
    parcelNode = newXMLNode("FOLDRESZLETEK", parent=metadataNode, namespace = "eing")
    addAttributes(parcelNode, "gml:id" = paste0("fid-", currfid))
    parcelBounded <- newXMLNode("boundedBy", parent=parcelNode, namespace = "gml")
    parcelEnvelope <- newXMLNode("Envelope", parent=parcelBounded, namespace = "gml")
    addAttributes(parcelEnvelope, srsDimension = 2, srsName = srsName) 
    addChildren(parcelEnvelope, newXMLNode("lowerCorner", paste(min(coords.matrix[,1]), min(coords.matrix[,2])), namespace = "gml"))
    addChildren(parcelEnvelope, newXMLNode("upperCorner", paste(max(coords.matrix[,1]), max(coords.matrix[,2])), namespace = "gml"))
    addChildren(parcelNode, newXMLNode("GEOBJ_ID", currfid, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("OBJ_FELS", "BD01", namespace = "eing"))
    addChildren(parcelNode, newXMLNode("RETEG_ID", 20, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("RETEG_NEV", "Földrészletek" , namespace = "eing"))
    addChildren(parcelNode, newXMLNode("TELEPULES_ID", 1110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("FEKVES", 3719, namespace = "eing")) # Belter
    addChildren(parcelNode, newXMLNode("HRSZ", 110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("FELIRAT", 110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("SZINT", 0, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("IRANY", 0, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("MUVEL_AG", 4557, namespace = "eing")) # Kivett
    addChildren(parcelNode, newXMLNode("JOGI_TERULET", 14885, namespace = "eing"))
    parcelGeometry <- newXMLNode("geometry", parent=parcelNode, namespace = "eing")
    parcelPolygon <- newXMLNode("Polygon", parent=parcelGeometry, namespace = "gml")
    addAttributes(parcelPolygon, srsDimension = 2, srsName = srsName) 
    parcelExterior <- newXMLNode("exterior", parent=parcelPolygon, namespace = "gml")
    parcelRing <- newXMLNode("LinearRing", parent=parcelExterior, namespace = "gml")
    addAttributes(parcelRing, srsDimension = 2)
    addChildren(parcelRing, newXMLNode("posList", paste(coords, collapse = " "), namespace = "gml"))
    ### Points
    ## Random point geneeration related to original
    currfidother <- currfid + round(abs(rnorm(1))*10^4)
    pontszam <- 52421
    ## Points assigned to the polygon
    for(actualpoints in 1:nrow(coords.matrix)) {
        actualpoint <- coords.matrix[actualpoints,]
        pointNode <- newXMLNode("RESZLETPONTOK", parent=metadataNode, namespace = "eing")
        addAttributes(pointNode, "gml:id" = paste0("fid-", currfidother))
        pointBounded <- newXMLNode("boundedBy", parent=pointNode, namespace = "gml")
        pointEnvelope <- newXMLNode("Envelope", parent=pointBounded, namespace = "gml")
        addAttributes(pointEnvelope, srsDimension = 2, srsName = srsName)
        addChildren(pointEnvelope, newXMLNode("lowerCorner", paste(actualpoint, collapse = " "), namespace = "gml"))
        addChildren(pointEnvelope, newXMLNode("upperCorner", paste(actualpoint, collapse = " "), namespace = "gml"))
        addChildren(pointNode, newXMLNode("GEOBJ_ID", currfidother, namespace = "eing"))
        addChildren(pointNode, newXMLNode("OBJ_FELS", "AC02", namespace = "eing"))
        addChildren(pointNode, newXMLNode("RETEG_ID", 6, namespace = "eing"))
        addChildren(pointNode, newXMLNode("RETEG_NEV", "Részletpontok" , namespace = "eing"))
        addChildren(pointNode, newXMLNode("TELEPULES_ID", 1110, namespace = "eing"))
        addChildren(pointNode, newXMLNode("HRSZ", namespace = "eing"))
        addChildren(pointNode, newXMLNode("FELIRAT", pontszam, namespace = "eing"))
        addChildren(pointNode, newXMLNode("SZINT", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("IRANY", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("MAGASSAG", 0, namespace = "eing"))
        addChildren(pointNode, newXMLNode("PONTSZAM", pontszam, namespace = "eing"))
        addChildren(pointNode, newXMLNode("PONTKOD", 4236, namespace = "eing"))
        addChildren(pointNode, newXMLNode("JELKULCS", 0, namespace = "eing"))
        pointGeometry <- newXMLNode("geometry", parent=pointNode, namespace = "eing")
        pointPoint <- newXMLNode("Point", parent=pointGeometry, namespace = "gml")
        addAttributes(pointPoint, srsDimension = 2, srsName = srsName)
        addChildren(pointPoint, newXMLNode("pos", paste(actualpoint, collapse = " "), namespace = "gml"))
        pontszam <- pontszam + sample(1:5, 1)
        currfidother <- currfid + sample(1:5, 1)
    }
    ## Save gml
    saveXML(gmlwithmeta, file, prefix='<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
}
