# xmllint
## Check
`xmllint --noout --schema VTM_2_8_xsd_kieg/EING_GML_szabvany_leiras_2_8.xsd  EINGGMLR/Telkek/KaliczPéter.gml; echo $?`

## Format
`xmllint --format KaliczPéter.gml`

## Shell example
`xmllint --shell KaliczPéter.gml`
`setns gml=http://www.opengis.net/gml`
`cd /gml:FeatureCollection/gml:metaDataProperty`
`du`

## Get MetaDataList
`xmllint --xpath "//*[local-name()='FeatureCollection']/*[local-name()='metaDataProperty']/*[local-name()='GenericMetaData']/*[local-name()='MetaDataList']" KaliczPéter.gml`


# ogrinfo
`ogrinfo -ro -al -so EINGGMLR/Telkek/KaliczPéter.gml`
