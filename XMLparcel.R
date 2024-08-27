parcels <- getNodeSet(test, "/gml:FeatureCollection/gml:featureMembers/eing:FOLDRESZLETEK")
xmlGetAttr(parcels[[3]], "gml:id")

getChildrenStrings(parcels[[3]])['GEOBJ_ID']

ParcCurrent <- xpathApply(test, "//gmlGeobjIds", xmlValue)

xpathSApply(test, "//eing:GEOBJ_ID", xmlValue) == ParcCurrent

xpathSApply(test, "//eing:FOLDRESZLETEK", function(x){xmlGetAttr(x, "gml:id")}) == paste0("fid-", ParcCurrent)

