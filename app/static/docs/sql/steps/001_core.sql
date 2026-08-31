-- =====================================================================
-- RTB-EAGEL · Core schema (fresh install)
-- Single-tenant · No data migration · PostgreSQL 13+
-- ---------------------------------------------------------------------
-- SCOPE: reference data, orders, service line items, samples, events,
--        reviews, audit, code generation.
-- NOT IN THIS FILE: the 13 pipeline stage tables (quarantine, indexing,
--        thermotherapy, meristem, sterilization, subculture, hardening,
--        conservation, distribution). Those land in 002 once the modules
--        are available - they plug into tbl_sample / tbl_sample_event
--        defined here.
--
-- FIVE DESIGN DECISIONS, all reversible - push back on any of them:
--
--  1. tbl_order HAS NO status COLUMN. Order status is DERIVED by rolling
--     up its service lines (view_order_progress). A stored parent status
--     is what let the old schema drift from reality.
--
--  2. Status is never a compound string. `molecular_virus_indexing_approved`
--     becomes stage_code + state, two columns. No more str_remove() surgery.
--
--  3. Sample status is an APPEND-ONLY EVENT LOG, not a mutable column.
--     Current status is the latest event (view_sample_current). You get the
--     audit trail free - non-negotiable for a phytosanitary lab.
--
--  4. Lookups live in the DB, not as R constants. SAMPLE_TYPES et al are
--     currently hardcoded in new_order_details.R; an admin should add a
--     sample type without a redeploy.
--
--  5. The tbl_ prefix is KEPT, to limit churn in modules I haven't seen.
--     Say the word and I'll strip it.
-- =====================================================================

BEGIN;

-- =====================================================================
-- SECTION 1 · REFERENCE DATA
-- =====================================================================

CREATE TABLE tbl_customer (
    customer_id       integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    customer_name     text NOT NULL,
    customer_type     text NOT NULL,
    customer_category text NOT NULL,
    group_name        text,
    description       text,
    address           text,
    alt_address       text,
    phone             text,
    email             text,
    active            boolean     NOT NULL DEFAULT true,
    created_on        timestamptz NOT NULL DEFAULT now(),
    created_by        text,
    CONSTRAINT uq_customer_name UNIQUE (customer_name)
);
-- text, not character(100): the old blank-padded columns silently carried
-- trailing spaces into every join and receipt.
CREATE UNIQUE INDEX uq_customer_email ON tbl_customer (lower(email)) WHERE email IS NOT NULL;

CREATE TABLE tbl_crop (
    crop_id         integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    crop_name       text    NOT NULL,
    scientific_name text,
    family          text,
    active          boolean NOT NULL DEFAULT true,
    CONSTRAINT uq_crop_name UNIQUE (crop_name)
);

CREATE TABLE tbl_variety (
    variety_id     integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    crop_id        integer NOT NULL REFERENCES tbl_crop(crop_id) ON UPDATE CASCADE,
    variety_name   text    NOT NULL,
    synonyms       text,
    origin_country text,
    description    text,
    active         boolean NOT NULL DEFAULT true,
    added_by       text,
    CONSTRAINT uq_variety UNIQUE (crop_id, variety_name)
);
CREATE INDEX ix_variety_crop ON tbl_variety (crop_id);

CREATE TABLE tbl_laboratory (
    laboratory_id   integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    laboratory_name text    NOT NULL,
    description     text,
    active          boolean NOT NULL DEFAULT true,
    CONSTRAINT uq_laboratory_name UNIQUE (laboratory_name)
);

-- Pathogens are a first-class table with their own crop FK and admin
-- module (add_pathogen.R) - not a text column on the test method.
CREATE TABLE tbl_pathogen (
    pathogen_id   integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    crop_id       integer NOT NULL REFERENCES tbl_crop(crop_id) ON UPDATE CASCADE,
    pathogen_name text    NOT NULL,
    synonyms      text,
    active        boolean NOT NULL DEFAULT true,
    added_by      text,
    added_on      date NOT NULL DEFAULT CURRENT_DATE,
    CONSTRAINT uq_pathogen UNIQUE (crop_id, pathogen_name)
);
CREATE INDEX ix_pathogen_crop ON tbl_pathogen (crop_id);

-- crop_id sits directly on the test (1:1), matching add_test_method.R.
-- `laboratory` was a free-text varchar in the old schema; a real FK here
-- means a renamed lab cannot orphan its tests.
CREATE TABLE tbl_test_method (
    test_id       integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    acronym       text    NOT NULL,
    test_name     text    NOT NULL,
    crop_id       integer NOT NULL REFERENCES tbl_crop(crop_id)       ON UPDATE CASCADE,
    laboratory_id integer NOT NULL REFERENCES tbl_laboratory(laboratory_id),
    pathogen_id   integer REFERENCES tbl_pathogen(pathogen_id),
    active        boolean NOT NULL DEFAULT true,
    CONSTRAINT uq_test_acronym UNIQUE (acronym)
);
CREATE INDEX ix_test_method_lab  ON tbl_test_method (laboratory_id);
CREATE INDEX ix_test_method_crop ON tbl_test_method (crop_id);

-- ---- Small controlled vocabularies -------------------------------
-- Separate tables rather than one generic lookup table: real FKs, real
-- typing, and each can grow its own columns.
--
-- WHY SOME USE TEXT CODES AND OTHERS INTEGER IDS:
--   text code  -> the workflow YAML branches on the value, so it must be
--                 readable and stable. cassava.yaml literally contains
--                 `condition: { sample_type: 'cutting' }`. An integer here
--                 would make the workflow unreadable and fragile.
--   integer id -> users create and rename these at runtime via the admin
--                 modules (add_sample_part.R, sampling_bag), so the name
--                 must be free to change without breaking references.

CREATE TABLE tbl_sample_type (
    sample_type_code text PRIMARY KEY,
    label            text    NOT NULL,
    sort_order       integer NOT NULL DEFAULT 0,
    active           boolean NOT NULL DEFAULT true
);

CREATE TABLE tbl_sample_condition (
    condition_code text PRIMARY KEY,
    label          text    NOT NULL,
    sort_order     integer NOT NULL DEFAULT 0,
    active         boolean NOT NULL DEFAULT true
);

CREATE TABLE tbl_sample_part (
    part_id     integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    part_name   text    NOT NULL,
    description text,
    active      boolean NOT NULL DEFAULT true,
    CONSTRAINT uq_part_name UNIQUE (part_name)
);

CREATE TABLE tbl_sampling_bag (
    bag_id      integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    bag_name    text    NOT NULL,
    description text,
    active      boolean NOT NULL DEFAULT true,
    CONSTRAINT uq_bag_name UNIQUE (bag_name)
);

-- ---- Pipeline stage vocabulary -----------------------------------
-- Single source of truth for stage names. The workflow YAML must
-- reference these codes; a stage that isn't here is a typo, and now the
-- FK says so instead of failing silently at runtime.
CREATE TABLE tbl_stage (
    stage_code  text PRIMARY KEY,
    label       text    NOT NULL,
    sort_order  integer NOT NULL DEFAULT 0,
    is_terminal boolean NOT NULL DEFAULT false,
    active      boolean NOT NULL DEFAULT true
);

-- ---- State vocabulary --------------------------------------------
-- The second half of the old compound status string. `..._approved`
-- becomes state 'approved'.
CREATE TABLE tbl_state (
    state_code text PRIMARY KEY,
    label      text    NOT NULL,
    is_failure boolean NOT NULL DEFAULT false   -- rejected/dead/contaminated
);

-- Which states are LEGAL at which stage. 'contaminated' means something at
-- surface sterilization and nothing at reception. tbl_sample_event carries a
-- composite FK to this table, so an illegal pair is rejected on write instead
-- of surfacing later as "Status not found in workflow".
CREATE TABLE tbl_stage_state (
    stage_code text NOT NULL REFERENCES tbl_stage(stage_code) ON DELETE CASCADE,
    state_code text NOT NULL REFERENCES tbl_state(state_code) ON DELETE CASCADE,
    PRIMARY KEY (stage_code, state_code)
);

-- ---- Service catalogue -------------------------------------------
CREATE TABLE tbl_service_catalog (
    service_code  text PRIMARY KEY,
    service_label text    NOT NULL,
    service_kind  text    NOT NULL CHECK (service_kind IN ('diagnostic','fulfilment')),
    unit          text    NOT NULL DEFAULT 'plantlet',
    weight        numeric NOT NULL DEFAULT 1.0,   -- for weighted completion rollup
    sort_order    integer NOT NULL DEFAULT 0,
    active        boolean NOT NULL DEFAULT true
);

-- =====================================================================
-- SECTION 2 · USERS
--
-- SHINYMANAGER OWNS ITS OWN TABLES. pg_template.yml contains the `init:`
-- DDL for `credentials`, `pwd_mngt` and `logs`, and shinymanager creates
-- and maintains them. This schema deliberately does NOT define them -
-- redefining them here would fight the package.
--
-- tbl_app_user is a SEPARATE actor registry, and the separation is the
-- point. shinymanager's config includes:
--     delete: DELETE FROM credentials WHERE "user" IN ({del_users*})
-- If audit columns pointed at credentials, removing a departed employee
-- would either be blocked by the FK or cascade away their history. For a
-- phytosanitary lab that history is the record: who approved this order,
-- who ran this test. It must outlive the account.
--
-- So: credentials answers "can this person log in"; tbl_app_user answers
-- "who did this work". Same username, no FK between them. The app upserts
-- into tbl_app_user on login (see ensure_app_user below) - the only field
-- the app actually reads from shinymanager is res_auth$user.
-- =====================================================================

CREATE TABLE tbl_app_user (
    username   text PRIMARY KEY,   -- matches credentials."user"
    full_name  text,
    email      text,
    role       text NOT NULL DEFAULT 'technician'
               CHECK (role IN ('admin','manager','technician','reviewer','viewer')),
    laboratory_id integer REFERENCES tbl_laboratory(laboratory_id),
    active     boolean     NOT NULL DEFAULT true,
    first_seen timestamptz NOT NULL DEFAULT now()
);

-- Call once per session after secure_server() returns res_auth. Makes the
-- username FK-able without duplicating shinymanager's account data.
CREATE FUNCTION ensure_app_user(p_username text, p_admin boolean DEFAULT false)
RETURNS void LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO tbl_app_user (username, role)
    VALUES (p_username, CASE WHEN p_admin THEN 'admin' ELSE 'technician' END)
    ON CONFLICT (username) DO NOTHING;
END; $$;

-- =====================================================================
-- SECTION 3 · CODE GENERATION
-- Generalises migration 001's counter to cover order numbers AND the
-- downstream sample prefixes (SS…, HD…). One mechanism, race-proof.
-- =====================================================================

CREATE TABLE tbl_code_counter (
    scope    text    NOT NULL,           -- 'order' | 'SS' | 'HD' | …
    yr       integer NOT NULL,
    mon      integer NOT NULL DEFAULT 0, -- 0 = counter is yearly, not monthly
    last_seq integer NOT NULL DEFAULT 0,
    PRIMARY KEY (scope, yr, mon)
);

-- PQS-YYYY-MON-NNN, minted atomically. Call INSIDE the save transaction.
-- The ON CONFLICT row lock serialises concurrent callers, so two
-- registrations can never receive the same number.
CREATE FUNCTION next_order_number() RETURNS text
LANGUAGE plpgsql AS $$
DECLARE
    v_yr  int := EXTRACT(year  FROM CURRENT_DATE)::int;
    v_mon int := EXTRACT(month FROM CURRENT_DATE)::int;
    v_seq int;
BEGIN
    INSERT INTO tbl_code_counter AS c (scope, yr, mon, last_seq)
    VALUES ('order', v_yr, v_mon, 1)
    ON CONFLICT (scope, yr, mon) DO UPDATE SET last_seq = c.last_seq + 1
    RETURNING last_seq INTO v_seq;

    RETURN format('PQS-%s-%s-%s', v_yr,
                  upper(to_char(CURRENT_DATE,'Mon')), lpad(v_seq::text,3,'0'));
END; $$;

-- Non-advancing preview for the registration form.
CREATE FUNCTION peek_order_number() RETURNS text
LANGUAGE plpgsql AS $$
DECLARE
    v_yr  int := EXTRACT(year  FROM CURRENT_DATE)::int;
    v_mon int := EXTRACT(month FROM CURRENT_DATE)::int;
    v_seq int;
BEGIN
    SELECT COALESCE(last_seq,0)+1 INTO v_seq
    FROM tbl_code_counter WHERE scope='order' AND yr=v_yr AND mon=v_mon;
    RETURN format('PQS-%s-%s-%s', v_yr,
                  upper(to_char(CURRENT_DATE,'Mon')), lpad(COALESCE(v_seq,1)::text,3,'0'));
END; $$;

-- Sample codes: SS26001, HD26001 - prefix + 2-digit year + 3-digit seq.
-- Yearly counter (mon = 0).
CREATE FUNCTION next_sample_code(p_prefix text) RETURNS text
LANGUAGE plpgsql AS $$
DECLARE
    v_yr  int := EXTRACT(year FROM CURRENT_DATE)::int;
    v_seq int;
BEGIN
    INSERT INTO tbl_code_counter AS c (scope, yr, mon, last_seq)
    VALUES (p_prefix, v_yr, 0, 1)
    ON CONFLICT (scope, yr, mon) DO UPDATE SET last_seq = c.last_seq + 1
    RETURNING last_seq INTO v_seq;

    RETURN format('%s%s%s', p_prefix,
                  to_char(CURRENT_DATE,'YY'), lpad(v_seq::text,3,'0'));
END; $$;

-- =====================================================================
-- SECTION 4 · ORDER
-- =====================================================================

CREATE TABLE tbl_order (
    order_number     text PRIMARY KEY,
    customer_id      integer NOT NULL REFERENCES tbl_customer(customer_id) ON UPDATE CASCADE,
    sample_amount    integer NOT NULL CHECK (sample_amount > 0),
    report_format    text,
    results_dispatch text,

    amount_charged   numeric(12,2) CHECK (amount_charged >= 0),
    receipt_no       text,
    payment_made     boolean NOT NULL DEFAULT false,

    -- Registration lifecycle ONLY. This is not pipeline progress:
    -- that is derived in view_order_progress.
    approval_state   text NOT NULL DEFAULT 'pending'
                     CHECK (approval_state IN ('pending','approved','rejected','cancelled')),
    approved_on      timestamptz,
    approved_by      text REFERENCES tbl_app_user(username),

    created_by       text REFERENCES tbl_app_user(username),
    created_on       timestamptz NOT NULL DEFAULT now(),
    updated_on       timestamptz NOT NULL DEFAULT now(),

    -- Billing fields are required once payment is claimed. The old schema
    -- enforced this only in the Shiny form, so any other write path could
    -- create a paid order with no receipt.
    CONSTRAINT ck_billing_complete CHECK (
        NOT payment_made OR (amount_charged IS NOT NULL AND receipt_no IS NOT NULL)
    )
);
CREATE INDEX ix_order_customer ON tbl_order (customer_id);
CREATE INDEX ix_order_created  ON tbl_order (created_on DESC);
CREATE INDEX ix_order_pending  ON tbl_order (approval_state) WHERE approval_state = 'pending';

-- Reception + incoming material. 1:1 with the order, matching the current
-- form. If an order ever needs several distinct crops/varieties, this is
-- the table that becomes 1:N - tbl_order needs no change.
CREATE TABLE tbl_order_detail (
    order_number      text PRIMARY KEY REFERENCES tbl_order(order_number)
                           ON UPDATE CASCADE ON DELETE CASCADE,
    ref_no            text,
    sampler           text,
    date_sampled      date,
    date_received     date,

    crop_id           integer REFERENCES tbl_crop(crop_id),
    variety_id        integer REFERENCES tbl_variety(variety_id),
    sample_type_code  text    REFERENCES tbl_sample_type(sample_type_code),
    condition_code    text    REFERENCES tbl_sample_condition(condition_code),
    part_id           integer REFERENCES tbl_sample_part(part_id),
    bag_id            integer REFERENCES tbl_sampling_bag(bag_id),
    sample_origin     text,
    sample_description text,
    additional_info   text,

    CONSTRAINT ck_sampled_before_received CHECK (
        date_sampled IS NULL OR date_received IS NULL OR date_sampled <= date_received
    )
);
CREATE INDEX ix_order_detail_crop ON tbl_order_detail (crop_id);

-- ---- Order-level event log (replaces tbl_project_log).
-- Append-only, mirroring tbl_sample_event so orders and samples get the
-- same treatment: history is a side effect of doing the work, not a
-- separate chore. Answers "who approved this, and when" from the record
-- rather than from a mutable column.
CREATE TABLE tbl_order_event (
    event_id     bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number text NOT NULL REFERENCES tbl_order(order_number)
                      ON UPDATE CASCADE ON DELETE CASCADE,
    module       text,
    action       text NOT NULL,
    occurred_on  timestamptz NOT NULL DEFAULT now(),
    actor        text REFERENCES tbl_app_user(username),
    notes        text
);
CREATE INDEX ix_order_event ON tbl_order_event (order_number, occurred_on DESC);

-- ---- Requested tests (was tbl_project_test_methods, which had no PK
--      and no FK at all - anything could be written into it).
CREATE TABLE tbl_order_test (
    order_number text NOT NULL REFERENCES tbl_order(order_number)
                      ON UPDATE CASCADE ON DELETE CASCADE,
    test_id      integer NOT NULL REFERENCES tbl_test_method(test_id),
    requested_on timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (order_number, test_id)
);

-- =====================================================================
-- SECTION 5 · SERVICE LINE ITEMS  (the keystone)
-- Surrogate PK, NOT (order_number, service_code): that is precisely what
-- lets the lab request the same service several times on one order -
-- subculture 200 for sale AND 50 for conservation. The old schema's
-- UNIQUE (project_code) made this impossible to express.
-- =====================================================================

CREATE TABLE tbl_order_service (
    order_service_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number     text NOT NULL REFERENCES tbl_order(order_number)
                          ON UPDATE CASCADE ON DELETE CASCADE,
    service_code     text NOT NULL REFERENCES tbl_service_catalog(service_code),

    target_qty       integer NOT NULL CHECK (target_qty > 0),
    purpose          text,           -- 'sale' | 'conservation' | 'distribution' | 'research'
    origin           text NOT NULL DEFAULT 'customer_order'
                          CHECK (origin IN ('customer_order','lab_initiated')),
    recipient        text,

    requested_by     text REFERENCES tbl_app_user(username),
    requested_on     timestamptz NOT NULL DEFAULT now(),
    started_on       timestamptz,
    cancelled_on     timestamptz,
    cancel_reason    text,
    notes            text
);
CREATE INDEX ix_os_order   ON tbl_order_service (order_number);
CREATE INDEX ix_os_service ON tbl_order_service (service_code);
CREATE INDEX ix_os_open    ON tbl_order_service (order_number) WHERE cancelled_on IS NULL;

-- =====================================================================
-- SECTION 6 · SAMPLES
-- =====================================================================

CREATE TABLE tbl_sample (
    sample_code      text PRIMARY KEY,
    order_number     text NOT NULL REFERENCES tbl_order(order_number) ON UPDATE CASCADE,

    -- NULL is meaningful: shared upstream material (quarantine, indexing,
    -- subculture stock) is not yet earmarked for any single request.
    order_service_id bigint REFERENCES tbl_order_service(order_service_id),

    -- Lineage: which sample this was derived from. Self-FK replaces the
    -- old source_code/source_type pair, which was an untyped soft link.
    parent_sample_code text REFERENCES tbl_sample(sample_code),

    stage_code       text NOT NULL REFERENCES tbl_stage(stage_code),

    -- Units this record represents (plantlets, copies, meristems, plants).
    -- The old schema spelled this six ways across 13 stage tables:
    -- number_of_plants / number_of_meristems / number_of_copies /
    -- number_conserved / quantity / number. One name, one meaning.
    quantity         integer NOT NULL DEFAULT 1 CHECK (quantity > 0),

    created_on       timestamptz NOT NULL DEFAULT now(),
    created_by       text REFERENCES tbl_app_user(username),

    CONSTRAINT ck_no_self_parent CHECK (parent_sample_code IS DISTINCT FROM sample_code)
);
CREATE INDEX ix_sample_order   ON tbl_sample (order_number);
CREATE INDEX ix_sample_service ON tbl_sample (order_service_id);
CREATE INDEX ix_sample_parent  ON tbl_sample (parent_sample_code);
CREATE INDEX ix_sample_stage   ON tbl_sample (stage_code);

-- ---- Append-only event log. Never UPDATE; only INSERT.
-- Replaces the mutable tbl_lab_sample_status. Current status becomes the
-- latest row (view_sample_current). History is a side effect, not a chore.
CREATE TABLE tbl_sample_event (
    event_id    bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    sample_code text NOT NULL REFERENCES tbl_sample(sample_code) ON DELETE CASCADE,

    -- Stage and state, SEPARATE. `molecular_virus_indexing_approved` was
    -- a compound key the app parsed with str_remove(); two columns end that.
    -- The composite FK enforces that this state is legal at this stage.
    stage_code  text NOT NULL,
    state_code  text NOT NULL,

    occurred_on timestamptz NOT NULL DEFAULT now(),
    actor       text REFERENCES tbl_app_user(username),
    notes       text,

    CONSTRAINT fk_event_stage_state
        FOREIGN KEY (stage_code, state_code) REFERENCES tbl_stage_state(stage_code, state_code)
);
CREATE INDEX ix_event_sample ON tbl_sample_event (sample_code, occurred_on DESC);
CREATE INDEX ix_event_stage  ON tbl_sample_event (stage_code, state_code);

-- ---- Allocations: work actually delivered against a request.
-- This is what makes partial fulfilment real: 6 of 10 plantlets.
-- qty lets one sample_code mean a single unit OR a jar of twenty.
CREATE TABLE tbl_service_allocation (
    allocation_id    bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_service_id bigint NOT NULL REFERENCES tbl_order_service(order_service_id)
                            ON DELETE CASCADE,
    sample_code      text REFERENCES tbl_sample(sample_code),
    qty              integer NOT NULL DEFAULT 1 CHECK (qty > 0),
    allocated_on     timestamptz NOT NULL DEFAULT now(),
    allocated_by     text REFERENCES tbl_app_user(username),
    notes            text
);
CREATE INDEX ix_alloc_service ON tbl_service_allocation (order_service_id);
CREATE INDEX ix_alloc_sample  ON tbl_service_allocation (sample_code);

-- =====================================================================
-- SECTION 7 · TEST RESULTS
-- =====================================================================

CREATE TABLE tbl_test_result (
    result_id   bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    sample_code text NOT NULL REFERENCES tbl_sample(sample_code) ON DELETE CASCADE,
    test_id     integer NOT NULL REFERENCES tbl_test_method(test_id),

    outcome     text NOT NULL CHECK (outcome IN ('positive','negative','inconclusive','pending')),
    value       text,
    tested_on   timestamptz NOT NULL DEFAULT now(),
    tested_by   text REFERENCES tbl_app_user(username),
    notes       text,
    CONSTRAINT uq_result UNIQUE (sample_code, test_id, tested_on)
);
CREATE INDEX ix_result_sample ON tbl_test_result (sample_code);
CREATE INDEX ix_result_test   ON tbl_test_result (test_id);

-- =====================================================================
-- SECTION 8 · REVIEW  (replaces 13 near-identical *_review tables)
-- They were all (reviewed_by, decision, comments, date) against a
-- different subject. One table, with an EXCLUSIVE ARC: exactly one FK
-- may be set. That keeps real referential integrity - unlike the usual
-- polymorphic subject_type/subject_id, which can't be enforced.
-- =====================================================================

CREATE TABLE tbl_review (
    review_id        bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,

    order_number     text   REFERENCES tbl_order(order_number)             ON DELETE CASCADE,
    sample_code      text   REFERENCES tbl_sample(sample_code)             ON DELETE CASCADE,
    order_service_id bigint REFERENCES tbl_order_service(order_service_id) ON DELETE CASCADE,

    stage_code       text REFERENCES tbl_stage(stage_code),
    decision         text NOT NULL CHECK (decision IN ('approved','rejected','returned')),
    comments         text,
    reviewed_by      text REFERENCES tbl_app_user(username),
    reviewed_on      timestamptz NOT NULL DEFAULT now(),

    CONSTRAINT ck_review_one_subject CHECK (
        (order_number     IS NOT NULL)::int
      + (sample_code      IS NOT NULL)::int
      + (order_service_id IS NOT NULL)::int = 1
    )
);
CREATE INDEX ix_review_order   ON tbl_review (order_number);
CREATE INDEX ix_review_sample  ON tbl_review (sample_code);
CREATE INDEX ix_review_service ON tbl_review (order_service_id);

-- =====================================================================
-- SECTION 9 · FILES & AUDIT
-- =====================================================================

CREATE TABLE tbl_file (
    file_id      bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number text REFERENCES tbl_order(order_number)  ON DELETE CASCADE,
    sample_code  text REFERENCES tbl_sample(sample_code)  ON DELETE CASCADE,
    file_name    text NOT NULL,
    stored_path  text NOT NULL,
    mime_type    text,
    size_bytes   bigint CHECK (size_bytes >= 0),
    description  text,
    uploaded_by  text REFERENCES tbl_app_user(username),
    uploaded_on  timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX ix_file_order  ON tbl_file (order_number);
CREATE INDEX ix_file_sample ON tbl_file (sample_code);

CREATE TABLE tbl_activity_log (
    log_id      bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    username    text REFERENCES tbl_app_user(username),
    module      text,
    action      text NOT NULL,
    subject     text,
    detail      jsonb,
    occurred_on timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX ix_activity_user ON tbl_activity_log (username, occurred_on DESC);
CREATE INDEX ix_activity_time ON tbl_activity_log (occurred_on DESC);

-- =====================================================================
-- SECTION 10 · DERIVED VIEWS
-- Status and progress are computed here, never stored, so they cannot
-- drift from the underlying facts.
-- =====================================================================

-- Current status of every sample = its most recent event.
CREATE VIEW view_sample_current AS
SELECT DISTINCT ON (e.sample_code)
    e.sample_code,
    s.order_number,
    s.order_service_id,
    e.stage_code,
    e.state_code,
    e.occurred_on AS since,
    e.actor
FROM tbl_sample_event e
JOIN tbl_sample s ON s.sample_code = e.sample_code
ORDER BY e.sample_code, e.occurred_on DESC, e.event_id DESC;

-- Progress per service line.
-- NOTE the LEFT JOIN: the old view_project_completion INNER JOINed a
-- table the app never populated, which is why progress was always 0.
-- A request with zero allocations must still appear, at 0%.
CREATE VIEW view_order_service_progress AS
SELECT
    os.order_service_id,
    os.order_number,
    os.service_code,
    c.service_label,
    c.service_kind,
    c.unit,
    c.weight,
    os.purpose,
    os.origin,
    os.recipient,
    os.target_qty,
    COALESCE(a.fulfilled_qty,0)::int                                   AS fulfilled_qty,
    GREATEST(os.target_qty - COALESCE(a.fulfilled_qty,0),0)::int       AS remaining_qty,
    LEAST(100, FLOOR(100.0*COALESCE(a.fulfilled_qty,0)
                     / NULLIF(os.target_qty,0)))::int                  AS pct_complete,
    CASE
        WHEN os.cancelled_on IS NOT NULL                   THEN 'cancelled'
        WHEN COALESCE(a.fulfilled_qty,0) >= os.target_qty  THEN 'fulfilled'
        WHEN COALESCE(a.fulfilled_qty,0) > 0               THEN 'in_progress'
        WHEN os.started_on IS NOT NULL                     THEN 'in_progress'
        ELSE 'requested'
    END                                                                AS status,
    os.requested_on, os.started_on, os.cancelled_on
FROM tbl_order_service os
JOIN tbl_service_catalog c ON c.service_code = os.service_code
LEFT JOIN (
    SELECT order_service_id, SUM(qty) AS fulfilled_qty
    FROM tbl_service_allocation GROUP BY order_service_id
) a ON a.order_service_id = os.order_service_id;

-- Rollup to the order. "Done when every requested service is done"
-- becomes true by construction rather than a column someone maintains.
CREATE VIEW view_order_progress AS
SELECT
    o.order_number,
    o.approval_state,
    COUNT(p.order_service_id)                                     AS services_requested,
    COUNT(*) FILTER (WHERE p.status = 'fulfilled')                AS services_fulfilled,
    COUNT(*) FILTER (WHERE p.status = 'in_progress')              AS services_in_progress,
    COUNT(*) FILTER (WHERE p.status = 'cancelled')                AS services_cancelled,
    COALESCE(FLOOR(SUM(p.pct_complete * p.weight)
                   / NULLIF(SUM(p.weight),0)),0)::int             AS pct_complete,
    CASE
        WHEN o.approval_state = 'rejected'  THEN 'rejected'
        WHEN o.approval_state = 'cancelled' THEN 'cancelled'
        WHEN o.approval_state = 'pending'   THEN 'pending_approval'
        WHEN COUNT(p.order_service_id) = 0  THEN 'approved'
        WHEN COUNT(*) FILTER (WHERE p.status NOT IN ('fulfilled','cancelled')) = 0
                                            THEN 'completed'
        WHEN COUNT(*) FILTER (WHERE p.status = 'in_progress') > 0
                                            THEN 'in_progress'
        ELSE 'approved'
    END                                                           AS derived_status
FROM tbl_order o
LEFT JOIN view_order_service_progress p ON p.order_number = o.order_number
GROUP BY o.order_number, o.approval_state;

COMMIT;
