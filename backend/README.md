# Backend

Api make in Haskell that provides the information of vaccination in Argentina.

# Setup

## Enviroment

Set these enviroment variable, the data between parenthesis is the default:

```
DATABASE_URL (postgresql://postgres:postgres@localhost:5432/covid)

PORT         (3000)

DATA_URL     (https://github.com/AgustinAlejoDammiano/final-PF/blob/master/documentation/test.zip?raw=true)
```

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
