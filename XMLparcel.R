parcels <- getNodeSet(test, "/gml:FeatureCollection/gml:featureMembers/eing:FOLDRESZLETEK")
xmlGetAttr(parcels[[3]], "gml:id")

getChildrenStrings(parcels[[3]])['GEOBJ_ID']

ParcCurrent <- xpathApply(test, "//gmlGeobjIds", xmlValue)

xpathSApply(test, "//eing:GEOBJ_ID", xmlValue) == ParcCurrent



