
Check:
`xmllint --noout --schema VTM_2_8_xsd_kieg/EING_GML_szabvany_leiras_2_8.xsd  EINGGMLR/Telkek/KaliczPéter.gml`

`xmllint --noout --format VTM_2_8_xsd_kieg/EING_GML_szabvany_leiras_2_8.xsd  KaliczPéter.gml`

`ogrinfo -ro -al -so EINGGMLR/Telkek/KaliczPéter.gml`
