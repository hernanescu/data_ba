## Intro ----
## El objetivo es hacer una Shiny que tome como insumo el input de un usuario y muestre la ubicación en la Ciudad de Buenos Aires.
## Montado sobre esa primera parte, lo que buscaremos es ir agregando información (también de CABA).

library(tidyverse)
library(RUMBA)
library(leaflet)
source(file = 'scripts/00_scripts.R')

## Existe un paquete llamado RUMBA (https://github.com/bitsandbricks/RUMBA) que nos ayuda para la georeferenciación.

## Paso 1 ----
## Lo primero que tenemos que hacer es probar cómo hacer que Leaflet lea las informaciones de un objeto almacenado. Estamos partiendo
## del presupuesto de que los usuarios clickearán un botón de acción con la dirección deseada.

start_value <- USIG_geocode('vera 40')

## Los valores de latitud y longitud son numéricos.

leaflet() %>% 
    addTiles() %>% 
    setView(lng=start_value$lon, lat=start_value$lat, zoom=11.5)

## Paso 2----
comunas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas.csv') %>% 
    rename("comuna"=comunas)

# El desafío es encontrar cómo pegarle a que con start_value me pueda dar la comuna de pertenencia.

lat <- start_value[1,2]
lon <- start_value[1,3]

sarasa2 <- st_point(c(lat, lon))

st_intersection(comunas, sarasa2)

# There you go!

## Paso 3 ----
## Ahora que sabemos cómo cruzar los puntos con sus respectivas comunas, vamos a buscar datos copados.

# Georeferenciación
escuelas <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/establecimientos-educativos/establecimientos-educativos.csv')
hospitales <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/hospitales/hospitales.csv')
saludprivada <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-salud-privados/centros-de-salud-privados.csv')
comisarias <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv')

# Datos interesantes
obras <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ba-obras/observatorio-de-obras-urbanas.csv')

## Paso 3.1 A manejar ----
escuelas <- escuelas %>% 
    rename('comuna' = comunas)

### Comisarias
### Long lat en numerico, barrio en mayuscula

comisarias <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv', encoding = 'UTF-8') %>% 
    select("nombre", "direccion", "barrio", "comuna", "long", "lat") %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Comisarias",
           region=as.factor(region),
           categoria=as.factor(categoria))


estabedu <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/establecimientos-educativos/establecimientos-educativos.csv', encoding = 'UTF-8') %>% 
    select("dom_establ", "nombre_estab", "nivel", "comunas", "barrio", "areaprogra", "long", "lat") %>% 
    mutate(barrio=case_when(barrio=="AGRONOMIA"~"Agronomía",
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="COGHLAN"~"Coghlan",
                            barrio=="COLEGIALES"~"Colegiales",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="LINIERS"~"Liniers",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="MONTSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE CHAS"~"Parque Chas",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VELEZ SARSFIELD"~"Vélez Sarsfield",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEL PARQUE"~"Villa del Parque",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA REAL"~"Villa Real",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SANTA RITA"~"Villa Santa Rita",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           subtipo=case_when(nivel %in%c('Inicial Común', 'Inicial Común - Otros Servicios Educativos de la modalidad Común', 'Inicial Especial')~'Inicial',
                             nivel %in%c('Primaria Común', 'Primario Común', 'Primario de Jóvenes y Adultos', 'Primario de Jóvenes y Adultos - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos',
                                         'Primario Especial', 'Primario Especial - Otros Servicios Educativos de la modalidad Especial')~'Primaria',
                             nivel %in% c('Secundario Común', 'Secundario Común - Otros Servicios Educativos de la modalidad Común', 'Secundario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos',
                                          'Secundario Común - Otros Servicios Educativos de las modalidadades Común y de Jóvenes y Adultos', 'Secundario Común - Secundario de Jóvenes y Adultos', 'Secundario Común - Secundario de Jóvenes y Adultos - Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común',
                                          'Secundario Común - Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común', 'Secundario Común - Superior No Universitario Común - Otros Servicios Educativos de las modalidadades Común y de Jóvenes y Adultos', 
                                          'Secundario de Jóvenes y Adultos', 'Secundario de Jóvenes y Adultos - Superior No Universitario Común', 'Secundario de Jóvenes y Adultos - Superior No Universitario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos')~'Secundaria',
                             nivel %in% c('Superior No Universitario Común', 'Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común', 'Superior No Universitario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos')~'Superior',
                             nivel %in% c('Otros Servicios Educativos de la modalidad Común', 'Otros Servicios Educativos de la modalidad de Jóvenes y Adultos', 'Otros Servicios Educativos de la modalidad Especial', 'Otros Servicios Educativos de las modalidades Común y de Jóvenes y Adultos')~'Otros',
                             TRUE~'Mixto')) %>% 
    rename("comuna" = comunas,
           "direccion" = dom_establ,
           "nombre"=nombre_estab) %>% 
    mutate(barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Establecimientos educativos",
           region=as.factor(region),
           categoria=as.factor(categoria))






## Paso 3.2 A mezclar
# Entonces: cómo tomar los valores de la comuna y armar algo copado?
# La respuesta está en get_comunas. Eso te devuelve la comuna en la que está el punto que dijo el usuario.

escuelas %>% 
    filter(comuna == get_comunas(start_value)) %>% 
    select(long, lat, nivel)

# Y así con todo lo que quiera. Ahora entonces la idea sería plotear sólo las cosas que están en la comuna donde está
# el usuario.


