-- =====================================================================
-- RTB-EAGEL · Atomic project-code generation
-- ---------------------------------------------------------------------
-- PROBLEM THIS FIXES
--   The app generated PQS-YYYY-MON-NNN in R from:
--       SELECT count(*) FROM current_month_project_count
--   read at form-render time and written at save time. Under concurrent
--   users two registrations read the same count, computed the same code,
--   and collided on tbl_project's primary key - the second save rolled
--   back and the user lost their order. COUNT(*) also drifts below the
--   true max if any row is ever deleted, causing code reuse.
--
-- APPROACH
--   A dedicated counter table with one row per (year, month), incremented
--   under a row lock inside the caller's transaction. Concurrent callers
--   serialize on that row, so every caller gets a distinct sequence number.
--   The number is a monotonic high-water mark - deleting an order never
--   lowers it, so codes are never reused.
--
-- Run this once against the RTB-EAGEL database (psql -f).
-- Safe to re-run: guarded with IF NOT EXISTS / CREATE OR REPLACE.
-- =====================================================================

-- 1) Per-month counter -------------------------------------------------
CREATE TABLE IF NOT EXISTS public.project_code_counter (
    yr        integer NOT NULL,
    mon       integer NOT NULL,
    last_seq  integer NOT NULL DEFAULT 0,
    PRIMARY KEY (yr, mon)
);

ALTER TABLE public.project_code_counter OWNER TO postgres;

-- 2) Seed the counter from existing data so we never re-mint a code that
--    already exists. This reads the true MAX sequence per month from the
--    codes already in tbl_project (format PQS-YYYY-MON-NNN), not COUNT(*).
--    The WHERE clause guarantees every row matches PQS-YYYY-MON-NNN, so the
--    trailing digit group always extracts cleanly to an integer.
INSERT INTO public.project_code_counter (yr, mon, last_seq)
SELECT
    EXTRACT(year  FROM created_on)::int  AS yr,
    EXTRACT(month FROM created_on)::int  AS mon,
    MAX(regexp_replace(project_code, '^.*-(\d+)$', '\1')::int) AS last_seq
FROM public.tbl_project
WHERE project_code ~ '^PQS-\d{4}-[A-Z]{3}-\d+$'
GROUP BY 1, 2
ON CONFLICT (yr, mon) DO UPDATE
    SET last_seq = GREATEST(public.project_code_counter.last_seq, EXCLUDED.last_seq);

-- 3) The atomic generator ---------------------------------------------
--    Returns the NEXT code for the current month and advances the counter
--    in one statement. Because it locks/creates the counter row, two
--    concurrent transactions calling this cannot receive the same number.
CREATE OR REPLACE FUNCTION public.next_project_code()
RETURNS varchar
LANGUAGE plpgsql
AS $$
DECLARE
    v_yr   int := EXTRACT(year  FROM CURRENT_DATE)::int;
    v_mon  int := EXTRACT(month FROM CURRENT_DATE)::int;
    v_seq  int;
    v_mon_abbr text := upper(to_char(CURRENT_DATE, 'Mon'));  -- e.g. JUL
BEGIN
    -- Upsert + increment in a single statement. The INSERT ... ON CONFLICT
    -- takes a row lock on the (yr, mon) row, so concurrent callers queue
    -- here and each leaves with a distinct last_seq.
    INSERT INTO public.project_code_counter AS c (yr, mon, last_seq)
    VALUES (v_yr, v_mon, 1)
    ON CONFLICT (yr, mon) DO UPDATE
        SET last_seq = c.last_seq + 1
    RETURNING last_seq INTO v_seq;

    RETURN format('PQS-%s-%s-%s', v_yr, v_mon_abbr, lpad(v_seq::text, 3, '0'));
END;
$$;

ALTER FUNCTION public.next_project_code() OWNER TO postgres;

-- 4) Preview helper (non-advancing) -----------------------------------
--    For the form to SHOW a provisional code without consuming a number.
--    The value shown is advisory only; the authoritative code is minted
--    by next_project_code() inside the save transaction.
CREATE OR REPLACE FUNCTION public.peek_project_code()
RETURNS varchar
LANGUAGE plpgsql
AS $$
DECLARE
    v_yr   int := EXTRACT(year  FROM CURRENT_DATE)::int;
    v_mon  int := EXTRACT(month FROM CURRENT_DATE)::int;
    v_seq  int;
    v_mon_abbr text := upper(to_char(CURRENT_DATE, 'Mon'));
BEGIN
    SELECT COALESCE(last_seq, 0) + 1 INTO v_seq
    FROM public.project_code_counter
    WHERE yr = v_yr AND mon = v_mon;

    IF v_seq IS NULL THEN
        v_seq := 1;
    END IF;

    RETURN format('PQS-%s-%s-%s', v_yr, v_mon_abbr, lpad(v_seq::text, 3, '0'));
END;
$$;

ALTER FUNCTION public.peek_project_code() OWNER TO postgres;

-- 5) Belt-and-braces: keep the counter ahead of any code inserted by a
--    path that bypasses next_project_code() (legacy scripts, imports).
--    This trigger advances the counter to match any manually-set code so
--    the generator can never later collide with it.
CREATE OR REPLACE FUNCTION public.sync_project_code_counter()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
    v_yr  int;
    v_mon int;
    v_seq int;
BEGIN
    IF NEW.project_code ~ '^PQS-\d{4}-[A-Z]{3}-\d+$' THEN
        v_yr  := EXTRACT(year  FROM NEW.created_on)::int;
        v_mon := EXTRACT(month FROM NEW.created_on)::int;
        v_seq := regexp_replace(NEW.project_code, '^.*-(\d+)$', '\1')::int;

        INSERT INTO public.project_code_counter AS c (yr, mon, last_seq)
        VALUES (v_yr, v_mon, v_seq)
        ON CONFLICT (yr, mon) DO UPDATE
            SET last_seq = GREATEST(c.last_seq, EXCLUDED.last_seq);
    END IF;
    RETURN NEW;
END;
$$;

ALTER FUNCTION public.sync_project_code_counter() OWNER TO postgres;

DROP TRIGGER IF EXISTS trg_sync_project_code_counter ON public.tbl_project;
CREATE TRIGGER trg_sync_project_code_counter
    AFTER INSERT ON public.tbl_project
    FOR EACH ROW
    EXECUTE FUNCTION public.sync_project_code_counter();

-- =====================================================================
-- USAGE
--   Preview (form):   SELECT public.peek_project_code();
--   Authoritative:    SELECT public.next_project_code();   -- inside the
--                     same transaction that INSERTs into tbl_project.
-- =====================================================================
