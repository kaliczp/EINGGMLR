parcels <- getNodeSet(test, "/gml:FeatureCollection/gml:featureMembers/eing:FOLDRESZLETEK")
xmlGetAttr(parcels[[3]], "gml:id")

getChildrenStrings(parcels[[3]])['GEOBJ_ID']
