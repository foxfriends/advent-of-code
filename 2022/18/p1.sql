/**
 * Alright, a bit unorthodox of a solution
 * Run this as `psql -f p1.sql`
 * and you will get the answer printed, like
 * normal. While this does not mind being run
 * on an already populated database, it does
 * not clean itself up after, so there is some
 * system state modified.
 */

\set QUIET TRUE
\t
\a

SET client_min_messages TO WARNING;

DROP DATABASE IF EXISTS p1;
CREATE DATABASE p1;
\c p1

CREATE TABLE IF NOT EXISTS points (
    x INT NOT NULL,
    y INT NOT NULL,
    z INT NOT NULL
);

TRUNCATE points;
\copy points (x, y, z) FROM './input' WITH (FORMAT CSV);

CREATE OR REPLACE VIEW faces (x, y, z, f, d) AS
    SELECT x, y, z, f, 1 FROM points, (values ('x'), ('y'), ('z')) f(f)
    UNION
    SELECT x + 1, y, z, 'x', -1 FROM points
    UNION
    SELECT x, y + 1, z, 'y', -1 FROM points
    UNION
    SELECT x, y, z + 1, 'z', -1 FROM points;

SELECT count(*) FROM (
    SELECT x, y, z, f 
        FROM faces i 
        GROUP BY x, y, z, f
        HAVING count(*) = 1
) a;
