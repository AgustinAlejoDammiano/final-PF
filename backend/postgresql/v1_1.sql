ALTER TABLE jurisdiction ALTER COLUMN id DROP DEFAULT;
ALTER SEQUENCE jurisdiction_id_seq OWNED BY NONE;
DROP SEQUENCE jurisdiction_id_seq;

ALTER TABLE department ALTER COLUMN id DROP DEFAULT;
ALTER SEQUENCE department_id_seq OWNED BY NONE;
DROP SEQUENCE department_id_seq;
