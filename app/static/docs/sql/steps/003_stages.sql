-- =====================================================================
-- RTB-EAGEL · 003 · Pipeline stage layer
-- Run after 001_core.sql and 002_seed.sql.
-- ---------------------------------------------------------------------
-- WHAT HAPPENED TO THE 13 STAGE TABLES
--
-- Every one of them was the same table wearing a different name:
--     (sample_code, source_code, date, quantity, actor) + review + status
-- with the columns spelled differently each time -
--     quantity : number_of_plants | number_of_meristems | number_of_copies
--                | number_conserved | quantity | number
--     actor    : recorded_by | done_by | technician | received_by
--     date     : date_of_entry | meristem_culture_date | sterilization_date
--                | received_date | date_of_conservation | date_of_distribution
--
-- That common shape is already covered by the core:
--     the record itself  -> tbl_sample (sample_code, parent_sample_code,
--                           stage_code, quantity, created_on, created_by)
--     the state changes  -> tbl_sample_event
--     the review         -> tbl_review
--     the test outcome   -> tbl_test_result
--     the code series    -> tbl_code_counter + next_sample_code()
--
-- So this file only has to carry what is GENUINELY stage-specific. Across
-- all 13 stages that turned out to be four things, in three tables.
--
--   tbl_project_test_methods had no PK and no FK, so anything at all could
--   be written into it. Every table here has both.
-- =====================================================================

BEGIN;

-- ---------------------------------------------------------------------
-- 1) QUARANTINE (glasshouse | growthroom)
--
-- DELIBERATE: keyed on order_number, not sample_code - matching the old
-- tbl_quarantine_glasshouse / _growthroom, which both keyed on project_code.
-- That is correct for the domain: quarantine receives the incoming
-- CONSIGNMENT onto a bench. Individual samples are not born until
-- initiation cuts explants from it. This is the one stage that genuinely
-- lives at order level; everything downstream is per-sample.
--
-- The old pair of identical tables collapses to one, discriminated by
-- stage_code, because glasshouse and growthroom differed in name only.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_order_quarantine (
    order_number  text NOT NULL REFERENCES tbl_order(order_number)
                       ON UPDATE CASCADE ON DELETE CASCADE,
    stage_code    text NOT NULL REFERENCES tbl_stage(stage_code),
    received_date date NOT NULL,
    received_by   text REFERENCES tbl_app_user(username),
    bench_no      text,                     -- the only stage-specific field
    quantity      integer CHECK (quantity > 0),
    notes         text,
    PRIMARY KEY (order_number, stage_code),
    CONSTRAINT ck_quarantine_stage
        CHECK (stage_code IN ('quarantine_glasshouse','quarantine_growthroom'))
);
CREATE INDEX ix_quarantine_bench ON tbl_order_quarantine (bench_no);

-- ---------------------------------------------------------------------
-- 2) THERMOTHERAPY
--
-- The richest stage: a sample occupies a conviron (growth chamber) for an
-- expected number of days, then exits. Those four fields are real and have
-- nowhere else to live.
--
-- Everything else the old tbl_thermotherapy carried is now core:
--   sample_code/source_code -> tbl_sample + parent_sample_code
--   date_of_entry           -> tbl_sample.created_on
--   status                  -> tbl_sample_event
--   review/reviewer_comments/reviewed_on/reviewed_by -> tbl_review
--
-- That last one mattered: those review columns were duplicated in
-- tbl_thermotherapy_review with DIFFERENT names for the same facts
-- (reviewer_comments vs review_comment, reviewed_on vs review_date) and
-- nothing kept the two copies in agreement.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_thermotherapy_detail (
    sample_code               text PRIMARY KEY REFERENCES tbl_sample(sample_code)
                                   ON DELETE CASCADE,
    conviron                  text NOT NULL,          -- chamber identifier
    expected_days_in_conviron integer CHECK (expected_days_in_conviron > 0),
    entered_on                date NOT NULL DEFAULT CURRENT_DATE,
    exit_date                 date,
    exit_notes                text,

    CONSTRAINT ck_exit_after_entry CHECK (exit_date IS NULL OR exit_date >= entered_on)
);
CREATE INDEX ix_thermo_conviron ON tbl_thermotherapy_detail (conviron);
-- Which samples are still occupying a chamber.
CREATE INDEX ix_thermo_open ON tbl_thermotherapy_detail (conviron) WHERE exit_date IS NULL;

-- ---------------------------------------------------------------------
-- 3) IN-VITRO CONSERVATION
--
-- storage_location was character(1) - a blank-padded single character, so
-- every comparison carried invisible padding. text here.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_conservation_detail (
    sample_code      text PRIMARY KEY REFERENCES tbl_sample(sample_code)
                          ON DELETE CASCADE,
    storage_location text NOT NULL,
    conserved_on     date NOT NULL DEFAULT CURRENT_DATE
);
CREATE INDEX ix_conservation_location ON tbl_conservation_detail (storage_location);

-- =====================================================================
-- STAGE-LAYER VIEWS
-- =====================================================================

-- Full lineage of any sample, back to the order. Replaces the old
-- source_code/source_type soft link that nothing enforced.
CREATE VIEW view_sample_lineage AS
WITH RECURSIVE chain AS (
    SELECT s.sample_code, s.parent_sample_code, s.order_number,
           s.stage_code, s.quantity, s.sample_code AS origin, 0 AS depth
    FROM tbl_sample s
    WHERE s.parent_sample_code IS NULL
    UNION ALL
    SELECT s.sample_code, s.parent_sample_code, s.order_number,
           s.stage_code, s.quantity, c.origin, c.depth + 1
    FROM tbl_sample s
    JOIN chain c ON s.parent_sample_code = c.sample_code
)
SELECT sample_code, parent_sample_code, order_number, stage_code,
       quantity, origin AS root_sample_code, depth
FROM chain;

-- The work queue: every sample sitting at a stage, with how long it has
-- been there. This is what the stage modules' "incoming / pending review /
-- pending update" tabs each rebuilt by hand.
CREATE VIEW view_stage_queue AS
SELECT
    c.sample_code,
    c.order_number,
    c.order_service_id,
    c.stage_code,
    st.label            AS stage_label,
    c.state_code,
    sv.label            AS state_label,
    sv.is_failure,
    c.since,
    (now() - c.since)   AS age,
    s.quantity,
    s.parent_sample_code
FROM view_sample_current c
JOIN tbl_sample s  ON s.sample_code = c.sample_code
JOIN tbl_stage st  ON st.stage_code = c.stage_code
JOIN tbl_state sv  ON sv.state_code = c.state_code;

-- Chamber occupancy - answers "what is in conviron 3 and for how long"
-- without the app scanning tbl_thermotherapy and filtering in R.
CREATE VIEW view_conviron_occupancy AS
SELECT
    t.conviron,
    t.sample_code,
    s.order_number,
    t.entered_on,
    t.expected_days_in_conviron,
    (t.entered_on + t.expected_days_in_conviron)::date AS due_out,
    CURRENT_DATE - t.entered_on                        AS days_in,
    CASE WHEN t.expected_days_in_conviron IS NOT NULL
          AND CURRENT_DATE > t.entered_on + t.expected_days_in_conviron
         THEN true ELSE false END                      AS overdue
FROM tbl_thermotherapy_detail t
JOIN tbl_sample s ON s.sample_code = t.sample_code
WHERE t.exit_date IS NULL;

COMMIT;
