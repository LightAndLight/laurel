# Extracting types from `information_schema`

When `:connect`ing to a Postgres database, I want to infer the table schemas. 

1. List all the tables

   ```sql
   SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'public'
   ```

2. For each `table_name`, retrieve the table's columns and types

   ```sql
   SELECT column_name, data_type FROM information_schema.columns
   WHERE table_schema = 'public' AND table_name = $table_name
   ```

3. For each `table_name`, retrieve the constraints used

   ```sql
   SELECT constraint_name, constraint_type
   FROM information_schema.table_constraints
   WHERE table_schema = 'public' AND table_name = $table_name
   ```

4. For each `constraint_name`, retrieve the fields constrained by it

   ```sql
   SELECT column_name FROM information_schema.constraint_column_usage
   WHERE 
     table_schema = 'public' AND
     table_name = $table_name AND
     constraint_name = $constraint_name
   ```