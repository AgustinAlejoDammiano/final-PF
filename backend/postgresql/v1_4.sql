CREATE SEQUENCE jurisdiction_id_seq;
CREATE OR REPLACE FUNCTION f_jurisdiction_id_seq(OUT nextfree bigint) AS
$func$
BEGIN
LOOP
   SELECT INTO nextfree  val
   FROM   nextval('jurisdiction_id_seq'::regclass) val  
   WHERE  NOT EXISTS (SELECT 1 FROM jurisdiction WHERE id = val);

   EXIT WHEN FOUND;
END LOOP; 
END
$func$  LANGUAGE plpgsql;
ALTER TABLE jurisdiction ALTER COLUMN id SET DEFAULT f_jurisdiction_id_seq();

CREATE SEQUENCE department_id_seq;
CREATE OR REPLACE FUNCTION f_department_id_seq(OUT nextfree bigint) AS
$func$
BEGIN
LOOP
   SELECT INTO nextfree  val
   FROM   nextval('department_id_seq'::regclass) val  
   WHERE  NOT EXISTS (SELECT 1 FROM department WHERE id = val);

   EXIT WHEN FOUND;
END LOOP; 
END
$func$  LANGUAGE plpgsql;
ALTER TABLE department ALTER COLUMN id SET DEFAULT f_department_id_seq();