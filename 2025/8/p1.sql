\set QUIET TRUE
\t
\a

SET client_min_messages TO WARNING;

DROP DATABASE IF EXISTS year_2025_day_8;
CREATE DATABASE year_2025_day_8;
\c year_2025_day_8
CREATE EXTENSION hstore;

CREATE TABLE points (
  i bigint primary key generated always as identity,
  x double precision not null,
  y double precision not null,
  z double precision not null, 
  unique (x, y, z)
);

CREATE TABLE edges (
  a bigint not null references points (i),
  b bigint not null references points (i),
  x1 double precision not null, y1 double precision not null, z1 double precision not null,
  x2 double precision not null, y2 double precision not null, z2 double precision not null,
  primary key (a, b),
  foreign key (x1, y1, z1) references points (x, y, z),
  foreign key (x2, y2, z2) references points (x, y, z)
);

CREATE INDEX edge_dist ON edges (((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2));

\COPY points (x, y, z) FROM 'sample' WITH (FORMAT csv, HEADER false);

INSERT INTO edges (a, b, x1, y1, z1, x2, y2, z2) 
  SELECT a.i, b.i, a.x, a.y, a.z, b.x, b.y, b.z 
  FROM points a, points b
  WHERE a.i < b.i;

CREATE FUNCTION mul (integer, integer) returns integer AS $$
  SELECT $1 * $2
$$ language sql;

CREATE AGGREGATE product (integer) (
  sfunc = mul,
  stype = integer,
  initcond = 1
);

WITH RECURSIVE
  closest as (
    SELECT hstore(a::text, '1'::text) || hstore(b::text, '1'::text) as s
      FROM edges 
      ORDER BY ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2) ASC 
      LIMIT 10
  ),
  spans (s, g) as (
    SELECT s, 1 FROM closest 
    UNION
    SELECT a.s || coalesce(b.s, ''), g + 1 
      FROM spans a, closest b 
      WHERE a.s ?| akeys(b.s) AND NOT a.s ?& akeys(b.s)
      and g < 10
  ),
  groups as (SELECT array_length(akeys(s), 1) l, s, g FROM spans ORDER BY g desc, array_length(akeys(s), 1) desc limit 3)
SELECT product(l) from groups;
