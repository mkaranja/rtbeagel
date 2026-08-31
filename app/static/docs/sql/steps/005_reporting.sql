-- =====================================================================
-- RTB-EAGEL · 005 · Reporting, dispatch, and sample disposition
-- Run after 004_lookups.sql.
--
-- DEPENDS ON 004: the views below read tbl_order.dispatch_code, which
-- 004_lookups creates (replacing the old free-text results_dispatch).
-- ---------------------------------------------------------------------
-- THE GOVERNING IDEA: RECOMMENDED IS NOT PERMITTED
--
-- A workflow that dictates the only legal next step will be fought by the
-- people using it, and they will win - by writing the real decision on
-- paper and typing something else into the system. At that point the
-- database is fiction.
--
-- So this schema separates three things that the old design conflated:
--
--   PERMITTED   tbl_stage_state       - what is physically meaningful.
--                                       Enforced by FK. Cannot be bypassed.
--   RECOMMENDED workflow YAML         - the happy path. Advisory ONLY. No
--                                       FK, no trigger, no enforcement.
--   ACTUAL      tbl_sample_event      - what a human actually did, with a
--                                       reason when it departed from the
--                                       recommendation.
--
-- The user is never locked out. They are asked to say why.
--
-- Concepts here are the standard LIMS ones rather than invented names, so
-- that auditors and neighbouring systems recognise them:
--   * sample disposition  - the end-of-testing decision on physical material
--   * reflex / add-on     - work added after results, on the same material
--   * CoA                 - Certificate of Analysis, the client-facing report
--   * amended report      - a versioned reissue, with a stated reason
-- =====================================================================

BEGIN;

-- ---------------------------------------------------------------------
-- 1) OVERRIDE: let people leave the happy path, on the record
--
-- The workflow recommends; if the operator does something else, the event
-- still writes - it just carries the reason. This is the single most
-- important column in the file: it is what stops the system from being
-- rigid, while keeping the record honest.
-- ---------------------------------------------------------------------
ALTER TABLE tbl_sample_event
    ADD COLUMN is_override     boolean NOT NULL DEFAULT false,
    ADD COLUMN override_reason text;

-- If you departed from the recommendation, say why. That is the only toll.
ALTER TABLE tbl_sample_event
    ADD CONSTRAINT ck_override_has_reason
    CHECK (NOT is_override OR override_reason IS NOT NULL);

-- ---------------------------------------------------------------------
-- 2) CONTINUATION ORDERS
--
-- "Pathogen detection came back clean - now multiply it for distribution."
-- Two shapes, and labs genuinely use both:
--
--   AMENDMENT   add service lines to the SAME order
--               (tbl_order_service.origin = 'lab_initiated')
--               Use when the continuation is part of the same engagement.
--
--   CONTINUATION create a NEW order pointing back at the original
--               (parent_order_number)
--               Use when it bills separately, or belongs to a different
--               customer - which is exactly the "subculture for sale to
--               the public" case: same plants, different commercial event.
-- ---------------------------------------------------------------------
ALTER TABLE tbl_order
    ADD COLUMN parent_order_number text REFERENCES tbl_order(order_number)
        ON UPDATE CASCADE,
    ADD COLUMN order_kind text NOT NULL DEFAULT 'primary'
        CHECK (order_kind IN ('primary','continuation','repeat')),
    ADD CONSTRAINT ck_parent_not_self CHECK (parent_order_number IS DISTINCT FROM order_number),
    ADD CONSTRAINT ck_continuation_has_parent
        CHECK (order_kind <> 'continuation' OR parent_order_number IS NOT NULL);

CREATE INDEX ix_order_parent ON tbl_order (parent_order_number)
    WHERE parent_order_number IS NOT NULL;

-- ---------------------------------------------------------------------
-- 3) SAMPLE DISPOSITION
--
-- The decision the user makes once testing is done and the report is out:
-- what happens to the physical plant material?
--
-- Recording this is not bureaucracy. Right now that decision happens in a
-- corridor and leaves no trace, so "what happened to the material from
-- PQS-2026-MAR-013?" is unanswerable. It is also the natural trigger for
-- the continuation flow.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_sample_disposition (
    disposition_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    sample_code    text NOT NULL REFERENCES tbl_sample(sample_code) ON DELETE CASCADE,

    decision       text NOT NULL CHECK (decision IN (
                        'discard',           -- destroyed / autoclaved
                        'continue',          -- more lab work; see the new service lines
                        'return_to_client',  -- sent back
                        'retain',            -- held pending a decision
                        'transfer'           -- moved to another programme/holder
                    )),
    reason         text,
    decided_on     timestamptz NOT NULL DEFAULT now(),
    decided_by     text REFERENCES tbl_app_user(username),

    -- 'discard' is irreversible, so it gets a witness, as destruction of
    -- regulated plant material normally does.
    witnessed_by   text REFERENCES tbl_app_user(username),

    CONSTRAINT ck_discard_witnessed
        CHECK (decision <> 'discard' OR witnessed_by IS NOT NULL)
);
CREATE INDEX ix_disposition_sample ON tbl_sample_disposition (sample_code);
CREATE INDEX ix_disposition_decision ON tbl_sample_disposition (decision, decided_on DESC);

-- Traceability for the continuation: this service line exists BECAUSE of
-- that disposition decision. Answers "why is there a lab-initiated
-- subculture on a pathogen-detection order?" without folklore.
ALTER TABLE tbl_order_service
    ADD COLUMN from_disposition_id bigint REFERENCES tbl_sample_disposition(disposition_id);

CREATE INDEX ix_os_from_disposition ON tbl_order_service (from_disposition_id)
    WHERE from_disposition_id IS NOT NULL;

-- ---------------------------------------------------------------------
-- 4) REPORTS  (Certificate of Analysis and friends)
--
-- Versioned, because amended reports are a fact of lab life: you issue v1,
-- find a transcription error, issue v2. Both must survive - the client
-- acted on v1. An UPDATE-in-place would erase what you told them.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_report (
    report_id     bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number  text NOT NULL REFERENCES tbl_order(order_number)
                       ON UPDATE CASCADE ON DELETE CASCADE,
    report_type   text NOT NULL DEFAULT 'coa'
                       CHECK (report_type IN ('coa','interim','phytosanitary','summary')),
    version       integer NOT NULL DEFAULT 1 CHECK (version > 0),

    supersedes_report_id bigint REFERENCES tbl_report(report_id),
    amendment_reason     text,

    file_id       bigint REFERENCES tbl_file(file_id),

    generated_on  timestamptz NOT NULL DEFAULT now(),
    generated_by  text REFERENCES tbl_app_user(username),

    -- A report is not dispatchable until someone signs it off. The old app
    -- generated a receipt PDF inline with no approval step at all.
    approved_on   timestamptz,
    approved_by   text REFERENCES tbl_app_user(username),

    CONSTRAINT uq_report_version UNIQUE (order_number, report_type, version),
    -- v2+ must say why it exists
    CONSTRAINT ck_amendment_reason
        CHECK (version = 1 OR (amendment_reason IS NOT NULL AND supersedes_report_id IS NOT NULL))
);
CREATE INDEX ix_report_order ON tbl_report (order_number, report_type, version DESC);

-- ---------------------------------------------------------------------
-- 5) DISPATCH
--
-- Who sent what, to whom, when, and did it arrive. The old
-- tbl_order.results_dispatch already said "Electronically" but nothing
-- recorded whether it ever happened. It is tbl_order.dispatch_code now
-- (see 004_lookups) - a real FK, so "Electronically" and "electronically"
-- can no longer be two different answers.
-- ---------------------------------------------------------------------
CREATE TABLE tbl_report_dispatch (
    dispatch_id   bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    report_id     bigint NOT NULL REFERENCES tbl_report(report_id) ON DELETE CASCADE,

    method        text NOT NULL CHECK (method IN ('email','portal','download','post','hand')),
    recipient     text NOT NULL,          -- email address, or a named person
    subject       text,
    body          text,

    dispatched_by text REFERENCES tbl_app_user(username),
    queued_on     timestamptz NOT NULL DEFAULT now(),
    sent_on       timestamptz,
    status        text NOT NULL DEFAULT 'queued'
                       CHECK (status IN ('queued','sent','failed','bounced','cancelled')),
    error_detail  text,

    CONSTRAINT ck_sent_has_time CHECK (status <> 'sent' OR sent_on IS NOT NULL),
    CONSTRAINT ck_failed_has_reason CHECK (status NOT IN ('failed','bounced') OR error_detail IS NOT NULL)
);
CREATE INDEX ix_dispatch_report ON tbl_report_dispatch (report_id);
CREATE INDEX ix_dispatch_queued ON tbl_report_dispatch (status) WHERE status = 'queued';

-- =====================================================================
-- VIEWS
-- =====================================================================

-- The current version of every report. Amended reports supersede, never
-- overwrite, so "the report" always means "the latest one".
CREATE VIEW view_report_current AS
SELECT DISTINCT ON (r.order_number, r.report_type)
    r.report_id, r.order_number, r.report_type, r.version,
    r.file_id, r.generated_on, r.generated_by,
    r.approved_on, r.approved_by,
    (r.approved_on IS NOT NULL) AS is_approved,
    r.amendment_reason
FROM tbl_report r
ORDER BY r.order_number, r.report_type, r.version DESC;

-- Orders whose work is finished but which have no approved current report.
-- This is the "ready to report" queue.
CREATE VIEW view_report_ready AS
SELECT
    p.order_number,
    p.pct_complete,
    p.services_requested,
    p.services_fulfilled,
    o.customer_id,
    cu.customer_name,
    cu.email        AS customer_email,
    dm.label        AS preferred_dispatch,
    rc.report_id    AS current_report_id,
    rc.version      AS current_version,
    rc.is_approved
FROM view_order_progress p
JOIN tbl_order o     ON o.order_number = p.order_number
JOIN tbl_customer cu ON cu.customer_id = o.customer_id
LEFT JOIN tbl_dispatch_method dm ON dm.dispatch_code = o.dispatch_code
LEFT JOIN view_report_current rc
       ON rc.order_number = p.order_number AND rc.report_type = 'coa'
WHERE p.derived_status = 'completed'
  AND (rc.report_id IS NULL OR rc.is_approved = false);

-- Approved reports not yet dispatched. Drives the "send to client" queue.
CREATE VIEW view_dispatch_ready AS
SELECT
    rc.report_id, rc.order_number, rc.report_type, rc.version,
    cu.customer_name, cu.email AS customer_email,
    o.dispatch_code    AS preferred_method_code,
    dm.label           AS preferred_method,
    rc.approved_on
FROM view_report_current rc
JOIN tbl_order o     ON o.order_number = rc.order_number
JOIN tbl_customer cu ON cu.customer_id = o.customer_id
LEFT JOIN tbl_dispatch_method dm ON dm.dispatch_code = o.dispatch_code
WHERE rc.is_approved
  AND NOT EXISTS (
      SELECT 1 FROM tbl_report_dispatch d
      WHERE d.report_id = rc.report_id AND d.status IN ('queued','sent')
  );

-- Samples whose work is done but whose physical fate is undecided.
-- THIS is the pathogen-detection-only case: testing complete, report out,
-- and a human still has to say discard or continue. Without this view that
-- material sits on a shelf and is quietly forgotten.
CREATE VIEW view_pending_disposition AS
SELECT
    s.sample_code,
    s.order_number,
    s.order_service_id,
    s.stage_code,
    s.quantity,
    c.state_code   AS current_state,
    c.since        AS at_stage_since,
    p.derived_status AS order_status,
    rc.report_id,
    rc.is_approved AS report_approved
FROM tbl_sample s
JOIN view_sample_current c    ON c.sample_code = s.sample_code
JOIN view_order_progress p    ON p.order_number = s.order_number
LEFT JOIN view_report_current rc
       ON rc.order_number = s.order_number AND rc.report_type = 'coa'
WHERE p.derived_status = 'completed'
  AND NOT EXISTS (
      SELECT 1 FROM tbl_sample_disposition d WHERE d.sample_code = s.sample_code
  );

-- Full continuation lineage: which orders grew out of which.
CREATE VIEW view_order_lineage AS
WITH RECURSIVE chain AS (
    SELECT order_number, parent_order_number, order_kind,
           order_number AS root_order, 0 AS depth
    FROM tbl_order WHERE parent_order_number IS NULL
    UNION ALL
    SELECT o.order_number, o.parent_order_number, o.order_kind,
           c.root_order, c.depth + 1
    FROM tbl_order o JOIN chain c ON o.parent_order_number = c.order_number
)
SELECT * FROM chain;

COMMIT;
