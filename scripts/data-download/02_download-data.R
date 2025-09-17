
# -------------------------------------------------------------------------
# From https://geoide.minvu.cl/server/rest/services/Catastros/Catastro_de_parques_urbanos/MapServer
# Date updated 12/7/2022
# saved as geojson
parques <- st_read("https://geoide.minvu.cl/server/rest/services/Catastros/Catastro_de_parques_urbanos/MapServer/0/query?where=1=1&outFields=*&f=geojson")
st_write(parques, "data/geo-data/parques_urbanos_minvu.geojson", driver = "GeoJSON")