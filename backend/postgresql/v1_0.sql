create table jurisdiction(
    id serial primary key unique,
    name text not null unique
);

create table department(
    id serial primary key unique,
    name text not null unique 
);

create table vaccine(
    id serial primary key unique,
    name text not null unique 
);

create table dose_application(
    id serial primary key,
    sex text,
    age_group text,
    condition text,
    lot text,
    serie integer,
    date timestamptz not null,
    vaccine_id serial references vaccine(id),
    residence_jurisdiction_id serial references jurisdiction(id),
    residence_department_id serial references department(id),
    application_jurisdiction_id serial references jurisdiction(id),
    application_department_id serial references department(id)
);

create table information(
    last_update timestamp not null
);
