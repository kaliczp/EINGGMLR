BuildNew <- function(coords, file = "gmlwithmeta.gml", currfid = round(abs(rnorm(1))*10^14)) {
    require(XML)
    srsName <- "urn:x-ogc:def:crs:EPSG:23700"
    ## Coordinates prepcocessing
    coords.matrix <- matrix(coords, ncol = 2, byrow = TRUE)
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
    addChildren(parcelNode, newXMLNode("OBJ_FELS", "BC04", namespace = "eing"))
    addChildren(parcelNode, newXMLNode("RETEG_ID", 20, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("RETEG_NEV", "Földrészletek" , namespace = "eing"))
    addChildren(parcelNode, newXMLNode("TELEPULES_ID", 1110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("FEKVES", 3720, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("HRSZ", 110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("FELIRAT", 110, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("SZINT", 0, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("IRANY", 0, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("MUVEL_AG", 4557, namespace = "eing"))
    addChildren(parcelNode, newXMLNode("JOGI_TERULET", 14885, namespace = "eing"))
    parcelGeometry <- newXMLNode("geometry", parent=parcelNode, namespace = "eing")
    parcelPolygon <- newXMLNode("Polygon", parent=parcelGeometry, namespace = "gml")
    addAttributes(parcelPolygon, srsDimension = 2, srsName = srsName) 
    parcelExterior <- newXMLNode("exterior", parent=parcelPolygon, namespace = "gml")
    parcelRing <- newXMLNode("LinearRing", parent=parcelExterior, namespace = "gml")
    addAttributes(parcelRing, srsDimension = 2)
    addChildren(parcelRing, newXMLNode("posList", paste(coords, collapse = " "), namespace = "gml"))
    ## Save gml
    saveXML(gmlwithmeta, file, prefix='<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
}
