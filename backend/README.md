# backend

http://eckyputrady.com/2018/09/22/Haskell-RESTful/

https://github.com/eckyputrady/haskell-scotty-realworld-example-app

# Enviroment

DATABASE_URL (postgresql://postgres:postgres@localhost:5432/covid)
PORT (3000)

Install Stack.

Install PostgreSQL.

sudo apt-get install libbz2-dev

stack setup

stack build

stack exec backend-exe

sudo apt install -y libpq-dev


curl --request POST -i \
   --url http://localhost:3000/api/jurisdiction \
   --header 'content-type: application/json' \
   --data '{ "jurisdiction": { "id": "69", "name": "XD" } }'

curl --request GET \
   --url http://localhost:3000/api/jurisdiction?name="Buenos" \
   --header 'content-type: application/json'
   
curl --request POST \
   --url http://localhost:3000/api/update\
   --header 'Content-Length: 0'

curl -X POST http://example.com

"https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip"
"https://github.com/AgustinAlejoDammiano/final-PF/blob/master/documentation/test.zip?raw=true"

"sexo","grupo_etario","jurisdiccion_residencia","jurisdiccion_residencia_id","depto_residencia","depto_residencia_id","jurisdiccion_aplicacion","jurisdiccion_aplicacion_id","depto_aplicacion","depto_aplicacion_id","fecha_aplicacion","vacuna",cod_dosis_generica,"nombre_dosis_generica","condicion_aplicacion",orden_dosis,"lote_vacuna"
"M","40-49","Corrientes","18","Capital","021","Corrientes","18","Capital","021","2021-08-16","Sputnik",3,"2da","18 a 59 años CON Factores de Riesgo",2,"II-110221"