# Fix: atomic project-code generation

## What was broken

`new_order.R` generated the order code in R like this:

```r
SELECT count FROM current_month_project_count   -- COUNT(*) for this month
no_ <- count + 1
"PQS-2026-JUL-{no_}"
```

read when the **form renders**, written when the user clicks **Save** — often
minutes apart. Three failure modes:

1. **Race / lost order (severe).** Two technicians registering in the same
   month both read the same count, compute the same code, and collide on
   `tbl_project`'s primary key. The second save rolls back the *entire*
   transaction and that user loses their order behind a generic
   "Failed to save project" toast.
2. **Code reuse (severe).** `COUNT(*)` drops if any order is ever deleted, so
   the next code reuses a number — another PK collision, or silent confusion.
3. **Stale preview.** The code shown can be out of date by the time Save runs.

## The fix

The authoritative code is now minted **inside the save transaction** by a
database counter that serializes concurrent callers. The form shows a
*provisional* preview only.

| Piece | File | Role |
|---|---|---|
| Counter + generator | `migrations/001_atomic_project_code.sql` | `next_project_code()` (atomic), `peek_project_code()` (preview), a seed, and a safety trigger |
| Save transaction | `order_registration.R` | calls `next_project_code()` once, threads the result through all 8 writes |
| Form preview | `new_order.R` | calls `peek_project_code()`, field relabelled "provisional — confirmed on save" |

### Why it's race-proof

`next_project_code()` does an `INSERT ... ON CONFLICT (yr, mon) DO UPDATE SET
last_seq = last_seq + 1 RETURNING last_seq`. The conflicting row is **locked**
for the duration of the transaction, so two concurrent transactions queue on
it and each leaves with a **distinct** sequence number. Because the counter is
a monotonic high-water mark, deleting an order never lowers it — codes are
never reused.

## Deploy

1. **Apply the migration** (idempotent; safe to re-run):
   ```bash
   psql -U postgres -d <your_db> -f migrations/001_atomic_project_code.sql
   ```
   The seed step reads the true `MAX` sequence per month from existing codes,
   so the first new code continues cleanly from your current data.

2. **Deploy the two R files** (`order_registration.R`, `new_order.R`).

3. **Restart the app.**

The R has a fallback: if the migration hasn't been applied yet, the form
preview shows `PQS-YYYY-MON-XXX` and the save will error clearly rather than
mint a bad code — so deploy order isn't fragile, but apply the SQL first.

## Verify

```sql
-- 1. Counter seeded from existing data
SELECT * FROM public.project_code_counter ORDER BY yr, mon;

-- 2. Preview does NOT advance the counter (run twice, same result)
SELECT public.peek_project_code();
SELECT public.peek_project_code();

-- 3. next_project_code DOES advance (run twice, two different codes)
BEGIN;
SELECT public.next_project_code();   -- e.g. PQS-2026-JUL-006
SELECT public.next_project_code();   -- PQS-2026-JUL-007
ROLLBACK;   -- rollback so this test doesn't consume real numbers

-- 4. Concurrency: open two psql sessions, BEGIN in both, call
--    next_project_code() in each. Session 2 blocks until session 1
--    commits/rolls back, then returns the NEXT number - never the same one.
```

In the app: open the registration form in two browser sessions, fill both,
and save nearly simultaneously. Before the fix, one fails; after, both succeed
with consecutive codes.

## Note on historical data

The seed buckets old codes by `created_on`'s year/month. If any legacy row's
embedded month (e.g. `MAR` in the code) disagrees with its `created_on` month,
its counter bucket follows `created_on`. This never affects live generation
(`next_project_code()` always uses `CURRENT_DATE`) and the `AFTER INSERT`
trigger keeps the counter ahead of any code inserted by other paths. If you
want the seed to bucket strictly by the code's embedded month instead, that's
a one-line change to the seed query — say the word.
