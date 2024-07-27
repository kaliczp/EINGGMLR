library(XML)
test <- xmlParse("teszt.gml")

library(xml2)
test2 <- read_xml("teszt.gml")
test2xml <- xmlParse(test2)
xml_structure(test2)

## Version
xml_text(xml_find_all(test2, ".//xsdVersion"))

## Parcel id
xml_text(xml_find_all(test2, ".//gmlGeobjIds"))
