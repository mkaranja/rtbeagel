-- =====================================================================
-- RTB-EAGEL · 000 · What is actually in this database?
-- ---------------------------------------------------------------------
--     psql -d rtbeagel -f 000_verify.sql
--
-- Read-only. Run it any time the app complains that a column does not
-- exist - it names the migration that is missing instead of leaving you
-- to infer it from a query fragment.
--
-- The migrations are NOT independent. 004 alters tables 001 created, and
-- 005 alters tables 001 AND 004 created, so they must run in order:
--     001_core -> 002_seed -> 003_stages -> 004_lookups -> 005_reporting
--
-- 004 and 005 SWAPPED. 004_lookups replaces tbl_order.results_dispatch with
-- dispatch_code, and 005_reporting's views read dispatch_code - so lookups
-- must land first. Running them the other way round gives:
--     ERROR: column o.results_dispatch does not exist
-- =====================================================================

\echo ''
\echo '=================== MIGRATION STATUS ==================='

SELECT
    m.migration,
    m.provides,
    CASE WHEN m.present THEN 'OK' ELSE '*** MISSING - RUN IT ***' END AS status
FROM (
    VALUES
    ('001_core.sql', 'tbl_order, tbl_sample, tbl_order_service, next_order_number()',
      (SELECT count(*) = 1 FROM information_schema.tables
        WHERE table_name = 'tbl_order_service')),

    ('002_seed.sql', 'stages, states, legal (stage,state) pairs, services',
      (SELECT count(*) > 0 FROM information_schema.tables
        WHERE table_name = 'tbl_stage_state')
      AND COALESCE((SELECT count(*) > 0 FROM tbl_stage_state), false)),

    ('003_stages.sql', 'tbl_order_quarantine, thermotherapy + conservation detail',
      (SELECT count(*) = 1 FROM information_schema.tables
        WHERE table_name = 'tbl_order_quarantine')),

    ('004_lookups.sql', 'tbl_country, report_format_code, dispatch_code, origin_country_code',
      (SELECT count(*) = 1 FROM information_schema.columns
        WHERE table_name = 'tbl_order' AND column_name = 'dispatch_code')),

    ('005_reporting.sql', 'tbl_order.order_kind + parent_order_number, tbl_report, disposition',
      (SELECT count(*) = 1 FROM information_schema.columns
        WHERE table_name = 'tbl_order' AND column_name = 'order_kind'))
) AS m(migration, provides, present)
ORDER BY m.migration;

\echo ''
\echo '=================== COLUMNS THE APP READS ==============='

SELECT
    c.needed_by,
    c.col,
    CASE WHEN EXISTS (
        SELECT 1 FROM information_schema.columns i
        WHERE i.table_name = c.tbl AND i.column_name = c.col
    ) THEN 'OK' ELSE 'MISSING' END AS status,
    c.from_migration
FROM (
    VALUES
    ('order_management.R', 'tbl_order', 'order_kind',          '005'),
    ('view_order.R',       'tbl_order', 'parent_order_number', '005'),
    ('new_order.R',        'tbl_order', 'report_format_code',  '004'),
    ('new_order.R',        'tbl_order', 'dispatch_code',       '004'),
    ('new_order_details.R','tbl_order_detail', 'origin_country_code', '004'),
    ('quarantine.R',       'tbl_order_quarantine', 'bench_no', '003')
) AS c(needed_by, tbl, col, from_migration);

\echo ''
\echo '=================== SEED DATA ==========================='

SELECT 'tbl_stage'          AS tbl, count(*) AS rows, '16 expected' AS note FROM tbl_stage
UNION ALL SELECT 'tbl_state',       count(*), '14 expected' FROM tbl_state
UNION ALL SELECT 'tbl_stage_state', count(*), '63 expected' FROM tbl_stage_state
UNION ALL SELECT 'tbl_service_catalog', count(*), '9 expected' FROM tbl_service_catalog
UNION ALL SELECT 'tbl_country',    count(*), '249 expected (004)' FROM tbl_country
UNION ALL SELECT 'tbl_app_user',   count(*), 'at least 1 (system)' FROM tbl_app_user;

\echo ''
\echo '=================== ROW COUNTS ========================='

SELECT 'tbl_customer' AS tbl, count(*) AS rows FROM tbl_customer
UNION ALL SELECT 'tbl_crop',          count(*) FROM tbl_crop
UNION ALL SELECT 'tbl_order',         count(*) FROM tbl_order
UNION ALL SELECT 'tbl_order_service', count(*) FROM tbl_order_service
UNION ALL SELECT 'tbl_sample',        count(*) FROM tbl_sample
UNION ALL SELECT 'tbl_sample_event',  count(*) FROM tbl_sample_event
ORDER BY 1;

\echo ''
\echo 'If any migration says MISSING, run it and the ones after it, in order.'
\echo ''
