ALTER TABLE dose_application ALTER COLUMN vaccine_id DROP DEFAULT;
ALTER SEQUENCE dose_application_vaccine_id_seq OWNED BY NONE;
DROP SEQUENCE dose_application_vaccine_id_seq;

ALTER TABLE dose_application ALTER COLUMN residence_jurisdiction_id DROP DEFAULT;
ALTER SEQUENCE dose_application_residence_jurisdiction_id_seq OWNED BY NONE;
DROP SEQUENCE dose_application_residence_jurisdiction_id_seq;

ALTER TABLE dose_application ALTER COLUMN residence_department_id DROP DEFAULT;
ALTER SEQUENCE dose_application_residence_department_id_seq OWNED BY NONE;
DROP SEQUENCE dose_application_residence_department_id_seq;

ALTER TABLE dose_application ALTER COLUMN application_jurisdiction_id DROP DEFAULT;
ALTER SEQUENCE dose_application_application_jurisdiction_id_seq OWNED BY NONE;
DROP SEQUENCE dose_application_application_jurisdiction_id_seq;

ALTER TABLE dose_application ALTER COLUMN application_department_id DROP DEFAULT;
ALTER SEQUENCE dose_application_application_department_id_seq OWNED BY NONE;
DROP SEQUENCE dose_application_application_department_id_seq;

alter table dose_application
drop constraint dose_application_vaccine_id_fkey,
add constraint dose_application_vaccine_id_fkey
   foreign key (vaccine_id)
   references vaccine(id)
   on delete cascade;

alter table dose_application
drop constraint dose_application_residence_jurisdiction_id_fkey,
add constraint dose_application_residence_jurisdiction_id_fkey
   foreign key (residence_jurisdiction_id)
   references jurisdiction(id)
   on delete cascade;

alter table dose_application
drop constraint dose_application_residence_department_id_fkey,
add constraint dose_application_residence_department_id_fkey
   foreign key (residence_department_id)
   references department(id)
   on delete cascade;

alter table dose_application
drop constraint dose_application_application_jurisdiction_id_fkey,
add constraint dose_application_application_jurisdiction_id_fkey
   foreign key (application_jurisdiction_id)
   references jurisdiction(id)
   on delete cascade;

alter table dose_application
drop constraint dose_application_application_department_id_fkey,
add constraint dose_application_application_department_id_fkey
   foreign key (application_department_id)
   references department(id)
   on delete cascade;
