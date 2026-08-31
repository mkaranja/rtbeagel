-- =====================================================================
-- RTB-EAGEL · 005 · SMOKE TEST
-- ---------------------------------------------------------------------
-- Run AFTER 001-004, against the fresh database:
--     psql -U postgres -d rtbeagel -v ON_ERROR_STOP=1 -f 005_smoke_test.sql
--
-- Everything happens inside a transaction that ROLLS BACK at the end, so
-- this leaves no data behind and is safe to re-run.
--
-- It proves the three things a parser cannot:
--   1. the views actually COMPILE and return sane numbers
--   2. the plpgsql functions actually RUN
--   3. the CHECK constraints actually FIRE
--
-- Every check RAISEs on failure, so with ON_ERROR_STOP=1 the script stops
-- at the first real problem. Silence to the end means everything passed.
-- =====================================================================

\set ON_ERROR_STOP on
BEGIN;

\echo '=== 1. reference data + code generation ==='

INSERT INTO tbl_app_user (username, full_name, role) VALUES
    ('smoke_tech','Smoke Technician','technician'),
    ('smoke_rev','Smoke Reviewer','reviewer');

INSERT INTO tbl_customer (customer_name, customer_type, customer_category, email)
VALUES ('Smoke Test Institute','Institution','Research','smoke@example.org');

INSERT INTO tbl_crop (crop_name, scientific_name) VALUES ('SMOKECROP','Testus smokei');
INSERT INTO tbl_variety (crop_id, variety_name)
SELECT crop_id,'SMOKEVAR' FROM tbl_crop WHERE crop_name='SMOKECROP';
INSERT INTO tbl_laboratory (laboratory_name) VALUES ('Smoke Lab');
INSERT INTO tbl_pathogen (crop_id, pathogen_name)
SELECT crop_id,'Smoke virus' FROM tbl_crop WHERE crop_name='SMOKECROP';
INSERT INTO tbl_test_method (acronym, test_name, crop_id, laboratory_id)
SELECT 'SMK','Smoke PCR', c.crop_id, l.laboratory_id
FROM tbl_crop c, tbl_laboratory l
WHERE c.crop_name='SMOKECROP' AND l.laboratory_name='Smoke Lab';

-- functions must RUN, not merely parse
DO $$
DECLARE a text; b text; p text;
BEGIN
    p := peek_order_number();
    a := next_order_number();
    b := next_order_number();
    IF a = b THEN
        RAISE EXCEPTION 'next_order_number() returned the same code twice: %', a;
    END IF;
    IF a !~ '^PQS-\d{4}-[A-Z]{3}-\d{3}$' THEN
        RAISE EXCEPTION 'order number has wrong shape: %', a;
    END IF;
    RAISE NOTICE 'peek=%  next=%  next=%  (distinct, correct shape)', p, a, b;

    IF next_sample_code('SS') !~ '^SS\d{2}\d{3}$' THEN
        RAISE EXCEPTION 'sample code has wrong shape';
    END IF;
    RAISE NOTICE 'sample codes: % %', next_sample_code('SS'), next_sample_code('HD');
END $$;

\echo '=== 2. an order requesting ONLY pathogen detection ==='

CREATE TEMP TABLE _t (order_number text);

INSERT INTO tbl_order (order_number, customer_id, sample_amount, approval_state, approved_by, created_by)
SELECT next_order_number(), customer_id, 1, 'approved', 'smoke_rev', 'smoke_tech'
FROM tbl_customer WHERE customer_name='Smoke Test Institute';

INSERT INTO _t (order_number)
SELECT order_number FROM tbl_order WHERE created_by='smoke_tech';

INSERT INTO tbl_order_detail (order_number, crop_id, sample_type_code, date_sampled, date_received)
SELECT t.order_number, c.crop_id, 'cutting', CURRENT_DATE-1, CURRENT_DATE
FROM _t t, tbl_crop c WHERE c.crop_name='SMOKECROP' LIMIT 1;

INSERT INTO tbl_order_service (order_number, service_code, target_qty, requested_by)
SELECT order_number, 'pathogen_detection', 1, 'smoke_tech' FROM _t LIMIT 1;

INSERT INTO tbl_sample (sample_code, order_number, stage_code, quantity, created_by)
SELECT next_sample_code('SM'), order_number, 'reception', 1, 'smoke_tech' FROM _t LIMIT 1;

INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor)
SELECT sample_code,'reception','logged','smoke_tech' FROM tbl_sample WHERE created_by='smoke_tech';

DO $$
DECLARE st text; pc int;
BEGIN
    SELECT derived_status, pct_complete INTO st, pc
    FROM view_order_progress
    WHERE order_number = (SELECT order_number FROM _t LIMIT 1);
    RAISE NOTICE 'after registration -> status=% pct=%', st, pc;
    IF st <> 'approved' THEN RAISE EXCEPTION 'expected approved, got %', st; END IF;
END $$;

\echo '=== 3. test resulted -> order completes -> report becomes available ==='

INSERT INTO tbl_test_result (sample_code, test_id, outcome, tested_by)
SELECT s.sample_code, t.test_id, 'negative', 'smoke_tech'
FROM tbl_sample s, tbl_test_method t
WHERE s.created_by='smoke_tech' AND t.acronym='SMK';

INSERT INTO tbl_service_allocation (order_service_id, sample_code, qty, allocated_by)
SELECT os.order_service_id, s.sample_code, 1, 'smoke_tech'
FROM tbl_order_service os, tbl_sample s
WHERE os.service_code='pathogen_detection' AND s.created_by='smoke_tech';

DO $$
DECLARE st text; pc int; n int;
BEGIN
    SELECT derived_status, pct_complete INTO st, pc
    FROM view_order_progress WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    RAISE NOTICE 'after result -> status=% pct=%', st, pc;
    IF st <> 'completed' THEN RAISE EXCEPTION 'expected completed, got %', st; END IF;

    SELECT count(*) INTO n FROM view_report_ready
     WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    IF n <> 1 THEN RAISE EXCEPTION 'view_report_ready should list this order, found %', n; END IF;
    RAISE NOTICE 'view_report_ready lists the order (report now due)';

    SELECT count(*) INTO n FROM view_pending_disposition
     WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    IF n <> 1 THEN RAISE EXCEPTION 'view_pending_disposition should list the sample, found %', n; END IF;
    RAISE NOTICE 'view_pending_disposition lists the sample (fate undecided)';
END $$;

\echo '=== 4. report must be APPROVED before it can be dispatched ==='

INSERT INTO tbl_report (order_number, report_type, version, generated_by)
SELECT order_number,'coa',1,'smoke_tech' FROM _t LIMIT 1;

DO $$
DECLARE n int;
BEGIN
    SELECT count(*) INTO n FROM view_dispatch_ready
     WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    IF n <> 0 THEN RAISE EXCEPTION 'unapproved report reached the dispatch queue!'; END IF;
    RAISE NOTICE 'unapproved report is NOT dispatchable  <- the gate holds';
END $$;

UPDATE tbl_report SET approved_on=now(), approved_by='smoke_rev'
WHERE order_number=(SELECT order_number FROM _t LIMIT 1);

DO $$
DECLARE n int;
BEGIN
    SELECT count(*) INTO n FROM view_dispatch_ready
     WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    IF n <> 1 THEN RAISE EXCEPTION 'approved report should be dispatchable, found %', n; END IF;
    RAISE NOTICE 'approved report IS dispatchable';
END $$;

\echo '=== 5. USER DECIDES: continue -> order reopens by itself ==='

INSERT INTO tbl_sample_disposition (sample_code, decision, reason, decided_by)
SELECT sample_code,'continue','Clean result - multiply for distribution','smoke_rev'
FROM tbl_sample WHERE created_by='smoke_tech';

INSERT INTO tbl_order_service
    (order_number, service_code, target_qty, purpose, origin, requested_by, from_disposition_id)
SELECT t.order_number,'subculture',200,'sale','lab_initiated','smoke_rev', d.disposition_id
FROM _t t, tbl_sample_disposition d
JOIN tbl_sample s ON s.sample_code=d.sample_code
WHERE s.created_by='smoke_tech' LIMIT 1;

DO $$
DECLARE st text; pc int;
BEGIN
    SELECT derived_status, pct_complete INTO st, pc
    FROM view_order_progress WHERE order_number=(SELECT order_number FROM _t LIMIT 1);
    RAISE NOTICE 'after continuation -> status=% pct=%  <- REOPENED with no manual reset', st, pc;
    IF st = 'completed' THEN
        RAISE EXCEPTION 'order should have reopened after adding a service line';
    END IF;
END $$;

\echo '=== 6. CONSTRAINTS MUST FIRE (each of these SHOULD fail) ==='

DO $$
DECLARE ok boolean;
BEGIN
    -- illegal stage/state pair
    ok := false;
    BEGIN
        INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor)
        SELECT sample_code,'reception','contaminated','smoke_tech'
        FROM tbl_sample WHERE created_by='smoke_tech';
    EXCEPTION WHEN foreign_key_violation THEN ok := true;
    END;
    IF NOT ok THEN RAISE EXCEPTION 'FAIL: contaminated-at-reception was accepted'; END IF;
    RAISE NOTICE 'illegal (stage,state) rejected            <- tbl_stage_state FK holds';

    -- override without a reason
    ok := false;
    BEGIN
        INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, is_override)
        SELECT sample_code,'thermotherapy','inprogress','smoke_tech',true
        FROM tbl_sample WHERE created_by='smoke_tech';
    EXCEPTION WHEN check_violation THEN ok := true;
    END;
    IF NOT ok THEN RAISE EXCEPTION 'FAIL: override without reason was accepted'; END IF;
    RAISE NOTICE 'override without reason rejected          <- ck_override_has_reason holds';

    -- ...but WITH a reason it is allowed: the user is never locked out
    INSERT INTO tbl_sample_event
        (sample_code, stage_code, state_code, actor, is_override, override_reason)
    SELECT sample_code,'thermotherapy','inprogress','smoke_tech',true,
           'Customer asked for thermotherapy despite clean result'
    FROM tbl_sample WHERE created_by='smoke_tech';
    RAISE NOTICE 'override WITH reason accepted             <- not rigid';

    -- discard without a witness
    ok := false;
    BEGIN
        INSERT INTO tbl_sample_disposition (sample_code, decision, decided_by)
        SELECT sample_code,'discard','smoke_tech' FROM tbl_sample WHERE created_by='smoke_tech';
    EXCEPTION WHEN check_violation THEN ok := true;
    END;
    IF NOT ok THEN RAISE EXCEPTION 'FAIL: unwitnessed discard was accepted'; END IF;
    RAISE NOTICE 'unwitnessed discard rejected              <- ck_discard_witnessed holds';

    -- paid order with no receipt
    ok := false;
    BEGIN
        INSERT INTO tbl_order (order_number, customer_id, sample_amount, payment_made, created_by)
        SELECT 'PQS-BAD-001', customer_id, 1, true, 'smoke_tech'
        FROM tbl_customer WHERE customer_name='Smoke Test Institute';
    EXCEPTION WHEN check_violation THEN ok := true;
    END;
    IF NOT ok THEN RAISE EXCEPTION 'FAIL: paid order with no receipt was accepted'; END IF;
    RAISE NOTICE 'paid order without receipt rejected       <- ck_billing_complete holds';

    -- amended report with no reason
    ok := false;
    BEGIN
        INSERT INTO tbl_report (order_number, report_type, version, generated_by)
        SELECT order_number,'coa',2,'smoke_tech' FROM _t LIMIT 1;
    EXCEPTION WHEN check_violation THEN ok := true;
    END;
    IF NOT ok THEN RAISE EXCEPTION 'FAIL: v2 report without amendment_reason was accepted'; END IF;
    RAISE NOTICE 'amended report without reason rejected    <- ck_amendment_reason holds';
END $$;

\echo '=== 7. every view compiles and is queryable ==='

DO $$
DECLARE v text; n int;
BEGIN
    FOR v IN SELECT table_name FROM information_schema.views
             WHERE table_schema='public' AND table_name LIKE 'view_%' ORDER BY 1
    LOOP
        EXECUTE format('SELECT count(*) FROM %I', v) INTO n;
        RAISE NOTICE '  % -> % rows', rpad(v,34), n;
    END LOOP;
END $$;

\echo ''
\echo '=== ALL SMOKE TESTS PASSED - rolling back, no data left behind ==='
ROLLBACK;
