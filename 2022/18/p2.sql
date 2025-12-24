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

CREATE OR REPLACE VIEW filledpoints (x, y, z) AS
    SELECT * FROM (
        SELECT xs.x, ranges.y, ranges.z FROM
            (
                SELECT min(a.x) as lo, max(b.x) as hi, a.y, a.z
                FROM points a
                INNER JOIN points b ON a.y = b.y and a.z = b.z and a.x <= b.x
                GROUP BY (a.y, a.z)
            ) AS ranges (lo, hi, y, z),
            LATERAL generate_series(ranges.lo, ranges.hi) xs(x)
        UNION ALL
        SELECT ranges.x, ys.y, ranges.z FROM
            (
                SELECT min(a.y) as lo, max(b.y) as hi, a.x, a.z
                FROM points a
                INNER JOIN points b ON a.x = b.x and a.z = b.z and a.y <= b.y
                GROUP BY (a.x, a.z)
            ) AS ranges (lo, hi, x, z),
            LATERAL generate_series(ranges.lo, ranges.hi) ys(y)
        UNION ALL
        SELECT ranges.x, ranges.y, zs.z FROM
            (
                SELECT min(a.z) as lo, max(b.z) as hi, a.x, a.y
                FROM points a
                INNER JOIN points b ON a.x = b.x and a.y = b.y and a.z <= b.z
                GROUP BY (a.x, a.y)
            ) AS ranges (lo, hi, x, y),
            LATERAL generate_series(ranges.lo, ranges.hi) zs(z)
    ) innerpoints (x, y, z) GROUP BY (x, y, z) HAVING COUNT(*) >= 3;

CREATE OR REPLACE VIEW allfaces (x, y, z, f, d) AS
    SELECT x, y, z, f, 1 FROM filledpoints, (values ('x'), ('y'), ('z')) f(f)
    UNION
    SELECT x + 1, y, z, 'x', -1 FROM filledpoints
    UNION
    SELECT x, y + 1, z, 'y', -1 FROM filledpoints
    UNION
    SELECT x, y, z + 1, 'z', -1 FROM filledpoints;

CREATE OR REPLACE RECURSIVE VIEW fakefaces (x, y, z, f, d) AS
    SELECT x, y, z, f, SUM(d) as int FROM allfaces GROUP BY (x, y, z, f) HAVING SUM(d) <> 0  -- new external faces
    EXCEPT
    SELECT x, y, z, f, SUM(d) as int FROM faces GROUP BY (x, y, z, f) HAVING SUM(d) <> 0  -- old external faces
UNION (
    WITH
        excluded AS (SELECT * FROM fakefaces),
        adjacent (x, y, z, f, d) AS (
            -- The whole block this face is part of
            SELECT x, y, z, face.f, 1 FROM excluded, (values ('x'), ('y'), ('z')) as face(f)
            UNION
            SELECT x + 1, y, z, 'x', -1 FROM excluded
            UNION
            SELECT x, y + 1, z, 'y', -1 FROM excluded
            UNION
            SELECT x, y, z + 1, 'z', -1 FROM excluded

            UNION

            -- depending on face, the whole block of the other side as well
            SELECT x - 1, y, z, face.f, 1 FROM excluded, (values ('x'), ('y'), ('z')) as face(f) WHERE excluded.f = 'x'
            UNION
            SELECT x, y, z, 'x', -1 FROM excluded WHERE f = 'x'
            UNION
            SELECT x - 1, y + 1, z, 'y', -1 FROM excluded WHERE f = 'x'
            UNION
            SELECT x - 1, y, z + 1, 'z', -1 FROM excluded WHERE f = 'x'

            UNION

            SELECT x, y - 1, z, face.f, 1 FROM excluded, (values ('x'), ('y'), ('z')) as face(f) WHERE excluded.f = 'y'
            UNION
            SELECT x + 1, y - 1, z, 'x', -1 FROM excluded WHERE f = 'y'
            UNION
            SELECT x, y, z, 'y', -1 FROM excluded WHERE f = 'y'
            UNION
            SELECT x, y - 1, z + 1, 'z', -1 FROM excluded WHERE f = 'y'

            UNION

            SELECT x, y, z - 1, face.f, 1 FROM excluded, (values ('x'), ('y'), ('z')) as face(f) WHERE excluded.f = 'z'
            UNION
            SELECT x + 1, y, z - 1, 'x', -1 FROM excluded WHERE f = 'z'
            UNION
            SELECT x, y + 1, z - 1, 'y', -1 FROM excluded WHERE f = 'z'
            UNION
            SELECT x, y, z, 'z', -1 FROM excluded WHERE f = 'z'
        )
    SELECT adjacent.* FROM adjacent INNER JOIN allfaces USING (x, y, z, f, d) 
    EXCEPT 
    SELECT * FROM faces
);

SELECT count(*) FROM (
    SELECT x, y, z, f
        FROM (SELECT * FROM allfaces EXCEPT ALL SELECT * FROM fakefaces) i
        GROUP BY x, y, z, f
        HAVING count(*) = 1
) a;
