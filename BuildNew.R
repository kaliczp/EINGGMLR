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
newgml$addNode("gmlGeobjIds", round(abs(rnorm(1))*10^14))
newgml$addNode("xsdVersion", 2.3)
newgml$closeNode()
newgml$closeNode()
newgml$ycloseNode()

gmlwithmeta <- xmlTreeParse(saveXML(newgml), useInternalNodes = T)
root <- xmlRoot(gmlwithmeta)
metadataNode <- newXMLNode("featureMembers", parent = root, namespace = "gml")
parcelNode = newXMLNode("FOLDRESZLETEK", parent=metadataNode, namespace = "eing")
xmlAttrs(parcelNode, TRUE, TRUE) <- c(id= "fid-11111111")
boundedBy
saveXML(gmlwithmeta, "gmlwithmeta.gml", prefix='<?xml version="1.0" encoding="UTF-8" standalone ="no"?>\n')
