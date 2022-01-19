# Backend

Api make in Haskell that provides the information of vaccination in Argentina.

# Setup

## Enviroment

Set these enviroment variable, the data between parenthesis is the default:

DATABASE_URL (postgresql://postgres:postgres@localhost:5432/covid)
PORT         (3000)
DATA_URL     (https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip)

Example on how to set them:

```
export PORT="4000"
```

Note: is important to create the database before running the application. The default database is name covid.

## Installation

* Install Stack.

* Install PostgreSQL.

* ```sudo apt-get install libbz2-dev```

* ```sudo apt install -y libpq-dev```

# Build

```
stack build
```

# Run

```
stack exec backend-exe
```

# Test

```
TODO
```

curl --request POST -i \
   --url http://localhost:3000/api/jurisdiction \
   --header 'content-type: application/json' \
   --data '{ "jurisdiction": { "name": "XD" } }'

curl --request GET \
   --url http://localhost:3000/api/jurisdiction?name="Buenos" \
   --header 'content-type: application/json'

curl --request GET \
   --url http://localhost:3000/api/department?name="Buenos" \
   --header 'content-type: application/json'

curl --request GET \
   --url http://localhost:3000/api/vaccine?name="Buenos" \
   --header 'content-type: application/json' 

curl --request DELETE -i \
   --url http://localhost:3000/api/jurisdiction/69

curl --request GET \
   --url http://localhost:3000/api/update\
   --header 'Content-Length: 0'

curl --request POST \
   --url http://localhost:3000/api/update\
   --header 'Content-Length: 0'

curl -X POST http://example.com

"https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip"
"https://github.com/AgustinAlejoDammiano/final-PF/blob/master/documentation/test.zip?raw=true"
"https://drive.google.com/uc?id=1O7jO75EU22h5Z58KxqM2UNEvp8zR9okp&export=download"

"sexo","grupo_etario","jurisdiccion_residencia","jurisdiccion_residencia_id","depto_residencia","depto_residencia_id","jurisdiccion_aplicacion","jurisdiccion_aplicacion_id","depto_aplicacion","depto_aplicacion_id","fecha_aplicacion","vacuna",cod_dosis_generica,"nombre_dosis_generica","condicion_aplicacion",orden_dosis,"lote_vacuna"
"M","40-49","Corrientes","18","Capital","021","Corrientes","18","Capital","021","2021-08-16","Sputnik",3,"2da","18 a 59 años CON Factores de Riesgo",2,"II-110221"

curl --request POST -i \
   --url http://localhost:3000/api/dose \
   --header 'content-type: application/json' \
   --data '{ "dose": { "sex": "M", "condition": "40-49", "lot" : "II-110221", "date" : "2021-08-16", "serie" : "2", "vaccineId" : "2",  "residenceJurisdictionId" : "18", "residenceDepartmentId" : "21", "applicationJurisdictionId" : "18", "applicationDepartmentId" : "21" } }'


source ./setEnv.sh && stack build && stack exec backend-exe

stack build && stack test
