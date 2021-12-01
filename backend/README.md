# backend

http://eckyputrady.com/2018/09/22/Haskell-RESTful/

https://github.com/eckyputrady/haskell-scotty-realworld-example-app

# Enviroment

DATABASE_URL (postgresql://postgres:postgres@localhost:5432/covid)
PORT (3000)

Install Stack.

Install PostgreSQL.

stack setup

stack build

stack exec backend-exe

sudo apt install -y libpq-dev


curl --request POST \
   --url http://localhost:3000/api/jurisdiction \
   --header 'content-type: application/json' \
   --data '{ "jurisdiction": { "name": "Catamarca" } }'

curl --request GET \
   --url http://localhost:3000/api/jurisdiction?name="Buenos" \
   --header 'content-type: application/json'
   