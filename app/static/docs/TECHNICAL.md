# RTB-EAGEL — Technical Documentation

**Roots, Tubers & Banana East Africa Germplasm Exchange Laboratory**
Plant quarantine and tissue-culture LIMS · KEPHIS

A laboratory information management system for phytosanitary quarantine and
tissue culture. This document describes the system: its domain model, data
model, workflow engine, modules, and the reasoning behind each design
decision.

*Status: under active development. This document is updated as modules land;
§11 records what exists and what does not.*

---

## 1. What the system does

Plant material arrives at KEPHIS for quarantine, testing, and propagation. A
customer sends a consignment — cuttings, tubers, plantlets — and requests
services against it: test it for pathogens, clean it, multiply it, conserve
it, distribute it.

RTB-EAGEL tracks that material from arrival to final disposition, and produces
the certificates the customer and the regulator rely on.

The physical pipeline:

```
reception → quarantine → virus indexing → thermotherapy → meristem culture
          → surface sterilization → subculture → hardening
          → conservation | distribution
```

Not every consignment takes every step. A clean sample skips thermotherapy. A
customer wanting only a phytosanitary certificate stops after indexing. The
system must represent the path actually taken, not an idealised one.

### 1.1 The governing model

The system is modelled on a **hospital laboratory information system**:

> A patient is registered once. Many tests are ordered against that
> registration. Each test has its own status, its own result, its own
> turnaround. The visit is complete when every ordered test is resulted and
> reviewed.

Mapped to plant material:

> A consignment is registered once as an **order**. Many **services** are
> requested against it. Each service has its own target, its own progress, its
> own fulfilment. The order is complete when every requested service is
> fulfilled.

This analogy is the architecture, not an illustration of it. It determines the
table structure (§3), the status model (§4), and the module boundaries (§6).

---

## 2. Domain vocabulary

| Term | Meaning |
|---|---|
| **Order** | One consignment from one customer. Identified `PQS-YYYY-MMM-NNN`. |
| **Service line** | One requested service on an order, with a target quantity. |
| **Sample** | One individually tracked unit of plant material. |
| **Stage** | Where a sample is in the pipeline (`quarantine_glasshouse`, `subculture`…). |
| **State** | What is true of it there (`received`, `established`, `approved`, `contaminated`…). |
| **Event** | A sample arriving at a (stage, state), with actor and timestamp. |
| **Consignment** | The physical material of an order, before it is cut into samples. |
| **Disposition** | The decision about a sample's fate once its work is done. |

**Order and sample are different scales, and the distinction is load-bearing.**
An order is a request. A sample is a thing you can hold. Between reception and
initiation the lab has an order but no samples — the consignment exists as one
undivided quantity. Initiation (§6.5) is where that changes.

---

## 3. Data model

34 tables, 11 views. PostgreSQL 10+ (`GENERATED ALWAYS AS IDENTITY`); 12+
recommended.

### 3.1 The spine

```
tbl_order                     identity, customer, billing
  ├── tbl_order_detail        the incoming material: crop, variety, origin
  ├── tbl_order_service       one row per requested service   ← keystone
  │     └── tbl_service_allocation   quantity delivered per event
  ├── tbl_order_test          which test methods were requested
  ├── tbl_order_quarantine    which bench the consignment sits on
  └── tbl_sample              one row per tracked explant
        ├── tbl_sample_event  append-only state history
        ├── tbl_test_result   indexing outcomes
        └── tbl_sample_disposition   final fate
```

### 3.2 `tbl_order_service` — the keystone

```sql
CREATE TABLE tbl_order_service (
    order_service_id  integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number      text NOT NULL REFERENCES tbl_order(order_number),
    service_code      text NOT NULL REFERENCES tbl_service_catalog(service_code),
    target_qty        integer,
    purpose           text,
    origin            text NOT NULL DEFAULT 'customer_order',
    recipient         text,
    from_disposition_id integer REFERENCES tbl_sample_disposition(disposition_id),
    ...
);
```

**The primary key is a surrogate, not `(order_number, service_code)`.** This is
the single most consequential choice in the schema.

A natural key on `(order_number, service_code)` would permit each service once
per order. Real requests do not obey that:

> *Subculture 200 plantlets for sale, and 50 for conservation.*

Two subculture lines on one order, different targets, different purposes,
different recipients, tracked independently. With a surrogate key this is two
rows. With a natural key it is not expressible at all.

`origin` distinguishes what the customer asked for (`customer_order`) from work
the lab adds itself (`lab_initiated`) — a continuation after a disposition
decision, for instance, traced back via `from_disposition_id`.

### 3.3 `tbl_sample` — two orthogonal columns

```sql
CREATE TABLE tbl_sample (
    sample_code        text PRIMARY KEY,
    order_number       text NOT NULL REFERENCES tbl_order(order_number),
    parent_sample_code text REFERENCES tbl_sample(sample_code),
    order_service_id   integer REFERENCES tbl_order_service(order_service_id),
    stage_code         text NOT NULL REFERENCES tbl_stage(stage_code),
    quantity           integer NOT NULL DEFAULT 1,
    ...
);
```

| Column | Question it answers | Changes? |
|---|---|---|
| `order_service_id` | what is this material **for**? | set once, at allocation |
| `stage_code` | where **is** it? | with every move |

These are independent, and conflating them would break both. Material can be
at `subculture` and destined for `in_vitro_distribution`; it can also be at
`subculture` and not yet destined for anything.

**`order_service_id` is nullable, and the null is meaningful.** Upstream stock
— material in quarantine, in indexing, in early subculture — is not yet
earmarked for any particular request. It is shared. Forcing an allocation at
creation time would be a lie about what the lab knows.

`parent_sample_code` is a self-referencing FK recording lineage. It is NULL for
roots (samples created at initiation, whose source is a consignment, not
another sample) and set for derived samples (subculture children). The
recursive view `view_sample_lineage` walks it.

### 3.4 Stage and state are separate columns

A sample's position is `(stage_code, state_code)` — never a single compound
string.

```sql
CREATE TABLE tbl_stage_state (
    stage_code text REFERENCES tbl_stage(stage_code),
    state_code text REFERENCES tbl_state(state_code),
    PRIMARY KEY (stage_code, state_code)
);

CREATE TABLE tbl_sample_event (
    ...
    stage_code text NOT NULL,
    state_code text NOT NULL,
    FOREIGN KEY (stage_code, state_code)
        REFERENCES tbl_stage_state(stage_code, state_code)
);
```

The composite foreign key means **an illegal position cannot be written**. The
63 legal pairs are seeded in `002_seed.sql`. `subculture/contaminated` is
legal; `subculture/hardened` is not; the database enforces the difference
rather than the application remembering to.

A compound string (`molecular_virus_indexing_approved`) would require string
surgery to read either half, and would admit any value at all.

### 3.5 History is append-only

`tbl_sample_event` is INSERT-only. Current status is a view:

```sql
CREATE VIEW view_sample_current AS
SELECT DISTINCT ON (e.sample_code)
       e.sample_code, s.order_number, e.stage_code, e.state_code,
       e.occurred_on AS since, e.actor
FROM tbl_sample_event e
JOIN tbl_sample s ON s.sample_code = e.sample_code
ORDER BY e.sample_code, e.occurred_on DESC, e.event_id DESC;
```

Nothing updates a status in place, so **history is a by-product of doing the
work** rather than a separate chore. For a laboratory subject to
phytosanitary audit, *"where was this sample on 3 March, and who moved it?"*
is not an optional feature.

Order-level events use `tbl_order_event` on the same principle.

### 3.6 One review table, exclusive arc

Review is a repeated shape: someone examines something, decides, comments,
signs. The things reviewed are of different kinds — an order at registration,
a sample at a stage gate, a service line at fulfilment.

```sql
CREATE TABLE tbl_review (
    review_id        integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number     text    REFERENCES tbl_order(order_number),
    sample_code      text    REFERENCES tbl_sample(sample_code),
    order_service_id integer REFERENCES tbl_order_service(order_service_id),
    decision   text NOT NULL CHECK (decision IN ('approved','rejected','returned')),
    comments   text,
    reviewed_by text NOT NULL REFERENCES tbl_app_user(username),
    reviewed_on timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT ck_review_one_subject CHECK (
        (order_number IS NOT NULL)::int
      + (sample_code IS NOT NULL)::int
      + (order_service_id IS NOT NULL)::int = 1
    )
);
```

Three nullable foreign keys, exactly one set — an **exclusive arc**. Unlike the
common `subject_type` / `subject_id` polymorphic pattern, every reference here
is a real FK the database can enforce. A review cannot point at an order that
does not exist.

### 3.7 Reference data lives in the database

Sample types, conditions, parts, bags, crops, varieties, pathogens, test
methods, laboratories, customers, and the **service catalogue** are all tables,
administered through the UI (§6.6). None are application constants.

The service catalogue matters most: it drives what the registration form
offers. Adding a service is an `INSERT`, and it appears on the intake form.

**Text code vs integer id** — the rule:

- **Text primary key** where the workflow branches on the value.
  `tbl_sample_type.sample_type_code = 'cutting'` is matched by name in
  `cassava.yaml`. Renaming it would silently break the branch, so codes are
  frozen after creation (§6.6).
- **Integer id** where users rename freely. `tbl_crop.crop_name` can change
  from "Cassava" to "Cassava (Manihot esculenta)" without consequence.

### 3.8 Identifier minting

```sql
CREATE FUNCTION next_order_number() RETURNS text ...
CREATE FUNCTION next_sample_code(prefix text) RETURNS text ...
```

Both serialise on a counter row in `tbl_code_counter` inside the caller's
transaction. Two technicians registering simultaneously block briefly and
receive different codes.

- Orders: `PQS-2026-JUL-001` — reset monthly.
- Samples: `IN26001` — prefix + 2-digit year + sequence. The prefix records
  the act that created it: `IN` initiation, `SS` subculture, `HD` hardening.

`peek_order_number()` previews without advancing, for form display.

---

## 4. The status model

Three distinct questions, three distinct mechanisms. Conflating them is the
mistake this design exists to avoid.

### 4.1 Order status is derived, never stored

`tbl_order` has **no status column**. Status is computed:

```sql
CREATE VIEW view_order_progress AS
SELECT
    o.order_number,
    CASE
        WHEN o.approval_state = 'pending'   THEN 'pending_approval'
        WHEN o.approval_state = 'rejected'  THEN 'rejected'
        WHEN o.approval_state = 'cancelled' THEN 'cancelled'
        WHEN count(sp.order_service_id) = 0 THEN 'approved'
        WHEN bool_and(sp.status = 'fulfilled') THEN 'completed'
        WHEN bool_or(sp.fulfilled_qty > 0)  THEN 'in_progress'
        ELSE 'approved'
    END AS derived_status,
    ...
FROM tbl_order o
LEFT JOIN view_order_service_progress sp ON sp.order_number = o.order_number
GROUP BY o.order_number, o.approval_state;
```

A stored parent status must be maintained by every code path that changes a
child. Miss one, and the stored value is wrong with no way to detect it. A
derived status cannot drift, because there is nothing to drift from.

**The proof that this is right:** adding a service line to a completed order
reopens it automatically. Nothing resets a flag; the rollup simply produces a
different answer. §7.3 traces this end to end.

### 4.2 `approval_state` is a different question

`tbl_order.approval_state` ∈ `pending | approved | rejected | cancelled` **is**
stored, because it is not derivable — it records a human decision.

It is the **registration lifecycle**: may this order enter the lab at all?
Pipeline progress is a separate axis, derived from service lines. An order can
be `approved` and 0% complete; it cannot be `in_progress` without being
approved.

### 4.3 Permitted vs recommended vs actual

| Level | Mechanism | Enforced |
|---|---|---|
| **PERMITTED** | `tbl_stage_state` — 63 legal pairs | **Yes**, composite FK |
| **RECOMMENDED** | workflow YAML | **No** — advisory |
| **ACTUAL** | `tbl_sample_event` | Recorded, with a reason if off-workflow |

This separation is deliberate and central.

A workflow that dictates the only permissible next step will be worked around
the first time reality diverges — and then the database records fiction while
the truth lives on paper. So the workflow **recommends**; the database
**permits**; the operator **decides**.

```sql
ALTER TABLE tbl_sample_event
  ADD COLUMN is_override boolean NOT NULL DEFAULT false,
  ADD COLUMN override_reason text,
  ADD CONSTRAINT ck_override_has_reason
      CHECK (NOT is_override OR override_reason IS NOT NULL);
```

The constraint requires a **reason**, not a permitted path. `record_event()`
never refuses an off-workflow move; it flags it. The UI shows such events with
an OFF-WORKFLOW marker, so divergence is visible rather than hidden.

`next_options()` returns what is recommended. `allowed_transitions()` returns
everything permitted. Interfaces lead with the first and keep the second one
click away.

---

## 5. The workflow engine

`app/logic/fct_workflows.R` — declarative, data-driven, no code execution.

### 5.1 Definition format

A workflow is YAML. Nodes are keyed by `(stage, state)` and carry exactly one
transition block:

```yaml
- stage: reception
  state: approved
  rules:
    - when: { sample_type: [cutting] }
      to_stage: quarantine_glasshouse
      step: establishment
      label: Establishment in Quarantine Glasshouse
    - default: true
      to_stage: quarantine_growthroom
      step: reception
      label: Reception in Quarantine Growthroom

- stage: subculture
  state: completed
  fan_out:
    - when: { service: [in_vitro_conservation] }
      to_stage: in_vitro_conservation
      step: conservation
    - when: { service: [in_vitro_distribution] }
      to_stage: hardening
      step: hardening
```

| Block | Semantics |
|---|---|
| `then:` | unconditional — exactly one successor |
| `rules:` | **first match wins** — branch; `default: true` is the fallback |
| `fan_out:` | **all matches apply** — the order proceeds down several paths at once |

`fan_out` is what lets one subculture feed conservation and distribution
simultaneously. It is the reason an order can be current in several stages, and
why that is correct rather than an error.

`cassava.yaml`: 51 nodes — 47 `then`, 3 `rules`, 1 `fan_out`; 29 edges; all 15
pipeline stages reachable.

### 5.2 Conditions are data

```yaml
when: { sample_type: [cutting] }
when: { virus_indexing: [positive] }
when: { service: [in_vitro_conservation] }
```

A condition is a map of context key → permitted values. Matching is set
membership. There is **no `eval()`, no `parse()`, no expression language** —
a workflow file cannot execute code, so an editable workflow is not a remote
code execution surface.

Context is assembled by `order_context()`:

```r
list(crop = "cassava", sample_type = "cutting",
     service = c("pathogen_detection", "in_vitro_distribution"),
     virus_indexing = "positive")
```

### 5.3 Validation at startup

`validate_workflow(wf, conn)` checks:

- every node's `(stage, state)` exists in `tbl_stage_state`
- every `to_stage` exists in `tbl_stage`
- every node has exactly one transition block

It runs at session start (`app_ui.R`), and raises. A workflow naming a stage
that does not exist stops the application in front of whoever deployed it,
rather than silently reporting "no next step defined" for months.

### 5.4 A note on the `then:` key

The unconditional block is `then:`, not `next:`. `next` is a reserved word in
R — a loop-control keyword — so `node$next` is a **parse error**, not a runtime
one: the module would fail to load entirely. The YAML key and the R accessor
must agree, and `then` is safe in both. `lint_r_reserved.py` enforces this
across the codebase (§9).

---

## 6. Modules

Shiny, `box::use` modules, bs4Dash shell. Every module exposes
`ui(id)` and `server(id, res_auth, page, tab, ...)`. **`ui()` takes exactly one
argument** — configuration is resolved inside the module, never at the call
site.

### 6.1 Application shell — `app_ui.R`

Three sidebar entries: ORDER MANAGEMENT, QUARANTINE, ADMINISTRATION. The orders
tab holds a `tabBox` with two panels (management, registration).

Modules are **lazily instantiated** on first visit and once only, tracked in
`initialized_modules`.

Session init runs twice-guarded:

1. `ensure_app_user()` — shinymanager owns the `credentials` table; the
   application owns `tbl_app_user`. Every `created_by` / `actor` /
   `reviewed_by` column is an FK to `tbl_app_user`, so a first-time user would
   otherwise fail on their first write. The two tables are **deliberately not
   FK'd to each other**: shinymanager deletes credentials when an account is
   removed, and laboratory history must outlive the account.
2. `workflow_cache(..., conn = pool)` — validates the workflow (§5.3).

The design system is linked **once** here (`order_theme$theme_css()`), not per
module.

### 6.2 Registration — `order_registration.R` + `new_order.R` + `new_order_details.R`

Writes, in one transaction:

```
tbl_order          identity + billing; order_number from next_order_number()
tbl_order_detail   the material
tbl_order_service  N rows — one per requested service
tbl_order_test     requested test methods
tbl_file           attachments
tbl_order_event    'registered'
```

The order number is minted as the **first statement of the transaction** and
threaded through every subsequent write.

**No samples are created here.** At reception the lab has a consignment.

The services form is rendered *from* `tbl_service_catalog` — no service is
named in R. An order must request at least one service; an order with none
would sit in the queue forever with nothing to complete.

### 6.3 Order management — `order_management.R`

The list. Three columns answer "what needs doing?", from three sources:

| Column | Source |
|---|---|
| STATUS | `view_order_progress.derived_status` |
| CURRENT | `view_sample_current` |
| NEXT (RECOMMENDED) | the workflow |

`order_board()` in `fct_tracking.R` does this in **three queries total**,
regardless of row count: one for the orders, one for all current positions, one
for all workflow contexts — then evaluates the workflow in memory. Calling
`next_steps()` per row would be two queries per order.

Unapproved orders show "Review & approve"; approved-but-unstarted orders ask
the workflow from `reception/approved`. Default filter is "Needs action".

### 6.4 Order view and approval — `view_order.R`

The detail. Shows the order's identity, its progress tracker, its service lines
with fulfilment, its sample details, its billing, and its full event history.

The tracker renders every stage as done / current / **next** / future. Next
steps are dashed — the visual language carries "recommended, not required"
without a legend.

**Approval lives here**, on the record being judged, so the reviewer decides
with the tracker, the service lines and the sample details in front of them.
One transaction writes `tbl_order` (the decision), `tbl_review` (the reviewer's
record), and `tbl_order_event` (the trail). Rejection requires a reason;
approval does not.

Approval is the gate: it is what makes a consignment visible to quarantine.

### 6.5 Quarantine — `quarantine.R`

**Order-level, not sample-level.** Quarantine receives a *consignment* onto a
bench; there are no samples yet. Writes `tbl_order_quarantine` (keyed on
`order_number`) and `tbl_order_event`.

Glasshouse and growthroom are **one module and one table**, discriminated by
`stage_code`. They differ only in which bench the material lands on; the act is
identical.

The workflow recommends the destination — cuttings to the glasshouse, else the
growthroom — and the recommendation is **preselected, not enforced**.

"Awaiting bench" = approved **and** no quarantine row.

### 6.6 Initiation — `initiation.R`

**Where sample records begin.** Takes a consignment on a bench and cuts it into
individually tracked explants.

Writes:

```
tbl_sample        one row per explant, code from next_sample_code('IN')
tbl_sample_event  each sample's first event
tbl_order_event   the order-level record
```

**Initiation is an act, not a stage.** There is no `initiation` row in
`tbl_stage`, deliberately: cutting explants does not move material anywhere.
The samples are born *at the quarantine stage*, because that is where they
physically are.

Their first state depends on the bench — both pairs legal in
`tbl_stage_state`:

| Bench | Birth state |
|---|---|
| `quarantine_glasshouse` | `established` |
| `quarantine_growthroom` | `received` |

`parent_sample_code` is NULL: these are roots. Their source is a consignment —
an order, not a sample — and the self-FK can only point at another sample. The
link to the consignment is `order_number`.

Each code is minted individually inside the transaction, so concurrent
initiations cannot collide. The queue shows consignments on a bench with a
count of samples cut so far; a consignment can be initiated more than once if
the material allows.

This module is the hinge of the system. Before it, tracking is per-order;
after it, per-sample. Every downstream stage — indexing, thermotherapy,
subculture — operates on the codes it creates.

### 6.7 Administration — `administration.R` + `admin_crud.R` + `fct_admin.R`

One page, eleven reference tables, one generic CRUD module driven by a registry
in `fct_admin.R`. Adding a reference table is an entry in `ADMIN`, not a new
module.

Grouped: Customers & crops · Lab setup · Sample vocabulary · Services.

Two rules encoded:

1. **Nothing is deleted.** Every reference table has `active`; the UI
   deactivates. A customer with twenty years of orders cannot be removed — the
   FK refuses, and removal would orphan history. Deactivating hides it from
   pickers while every past order stays readable. Before deactivating, the UI
   reports how many records already reference the row.
2. **Text codes are frozen after creation** (§3.7). Editable at creation,
   disabled forever after, with the reason shown in the form.

Column names come from the registry — never user input, safe to interpolate.
Every value is a bind parameter.

---

## 7. Reporting, dispatch, disposition

### 7.1 Reports are versioned

```sql
CREATE TABLE tbl_report (
    report_id integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    order_number text NOT NULL REFERENCES tbl_order(order_number),
    report_type  text NOT NULL,
    version      integer NOT NULL DEFAULT 1,
    amendment_reason text,
    approved_by text REFERENCES tbl_app_user(username),
    approved_on timestamptz,
    ...
    CONSTRAINT ck_amendment_reason
        CHECK (version = 1 OR amendment_reason IS NOT NULL),
    UNIQUE (order_number, report_type, version)
);
```

A report is issued; an error is found; a corrected report is issued. **Version 1
must survive**, because the customer acted on it. `ck_amendment_reason` requires
v2+ to state why. `view_report_current` resolves "the report" to the latest
version.

### 7.2 Approval gates dispatch

`view_dispatch_ready` surfaces only reports with `approved_on` set. A report
cannot leave the building without a signature.

Dispatch is **queued** — `tbl_report_dispatch`, `queued` → `sent` — not sent
inline. A mail server timeout must not roll back a database transaction.

### 7.3 Disposition and continuation

When a sample's work is done, a human decides its fate:

```sql
decision text NOT NULL CHECK (decision IN
    ('discard','continue','return','retain','transfer'))
```

`view_pending_disposition` makes this a queue rather than a corridor
conversation. `ck_discard_witnessed` requires a witness for `discard` —
destruction of regulated plant material is irreversible.

**The continuation case, traced:**

| Step | `derived_status` | % |
|---|---|---|
| Pathogen detection resulted | `completed` | 100 |
| CoA generated → approved → dispatched | `completed` | 100 |
| Disposition: *continue*; subculture 200 added | `approved` | 66 |
| 60 subcultured | `in_progress` | 76 |
| All 200 done | `completed` | 100 → amended CoA due |

The order reopens by itself. This is only possible because status is derived
(§4.1).

Both continuation shapes are supported, because laboratories use both:

- **Amendment** — new service lines on the same order
  (`origin = 'lab_initiated'`, traced via `from_disposition_id`).
- **Continuation order** — a new order with `parent_order_number` set, billed
  separately. `view_order_lineage` walks the chain.

---

## 8. Verification

The system is developed without an R runtime or a PostgreSQL server available
in the authoring environment. What that permits — and what it does not — shapes
the process.

| Artefact | Method | Catches |
|---|---|---|
| SQL syntax | `pglast` / **libpg_query** — PostgreSQL's own C parser | real syntax errors, including plpgsql bodies |
| SQL semantics | AST walk: FK graph, creation order, view column resolution | dangling refs, forward references |
| Migration chain | replay `ADD`/`DROP COLUMN` in order; check each view resolves at creation | cross-file breakage |
| R syntax | bracket balance (string/comment aware) + `lint_r_reserved.py` | parse errors |
| R semantics | declared-output vs renderer cross-check | blank pages |
| Schema conformance | every written column checked against the parsed DDL | typos, drift |
| Runtime behaviour | **execution against a live database** | everything else |

Using PostgreSQL's actual parser rather than a reimplementation matters:
`RETURNING ... INTO STRICT` is valid plpgsql but invalid in plain SQL, and only
a real parser knows the difference.

**The limits are real.** Static checks catch syntax and cross-file consistency.
They do not catch runtime semantics — what `$<-` does to a zero-row data frame,
whether a reactive fires, whether a query plan is fast. Those require
execution, and execution is the developer's responsibility.

**The principle that emerged:** the defects that survive per-file validation are
those where each file is correct in isolation and wrong in combination — an
engine and a config that disagree, a module signature and its call site, a
migration and the migration after it. Per-file checking is structurally blind
to all of them. **Cross-file checks are the only defence**, and each one in the
table above exists because a specific defect got through without it.

---

## 9. Conventions

- **`box::use` imports must match calls.** An imported-but-uncalled function is
  harmless; a called-but-unimported one fails at runtime under box's strict
  namespacing.
- **Every module is `X$ui(ns("id"))`** — one argument.
- **`df$col <- rep(NA, nrow(df))`**, never `<- NA`. A zero-row frame raises
  *"replacement has 1 row, data has 0"*, and **empty is the normal state on a
  fresh database**.
- **Reserved words are never `$` accessors.** No `$next`, `$repeat`, `$in` —
  these are parse errors, not runtime errors. `lint_r_reserved.py` enforces it.
- **`load_data()` always returns a data frame** — zero rows on failure, never
  NULL, never a string. Callers can rely on `nrow()` and `$col`.
- **Values are bind parameters; identifiers come from configuration.** Never
  interpolate user input into SQL.
- **Secrets live in `.Renviron`**, never in source. `fct_conn.R` and
  `pg_template.yml` read the same five `PG*` variables. Note that `.gitignore`
  does not remove what git already holds — rotate.
- **Prefer rebuilding a file over regex-editing it.** Pattern-based edits to
  source have twice silently deleted working code that balanced brackets
  perfectly.

---

## 10. Deployment

```bash
createdb rtbeagel

psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/001_core.sql
psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/002_seed.sql
psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/003_stages.sql
psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/004_lookups.sql     # BEFORE reporting
psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/005_reporting.sql

psql -d rtbeagel -f schema/000_verify.sql                         # what landed?
psql -d rtbeagel -v ON_ERROR_STOP=1 -f schema/006_smoke_test.sql  # proves it; rolls back
```

**Order matters.** `005_reporting` creates views reading
`tbl_order.dispatch_code`, which `004_lookups` creates. Reporting depends on
lookups; lookups therefore run first.

`ON_ERROR_STOP=1` is not optional — without it `psql` continues past failures
and reports success. Each file is `BEGIN`/`COMMIT`-wrapped, so a failure rolls
back whole.

`000_verify.sql` is read-only and reports which migrations landed.
`006_smoke_test.sql` runs a full order → sample → test → report → disposition →
continuation scenario inside a transaction and **rolls back**, asserting that
functions execute, views compile, and constraints fire.

Application configuration: copy `.Renviron.example` to `.Renviron`, set
`PGHOST` / `PGPORT` / `PGDATABASE` / `PGUSER` / `PGPASSWORD`, restart R.
Grant the application role `SELECT/INSERT/UPDATE` and `EXECUTE`; it never needs
superuser.

---

## 11. Status

**Implemented**

| Area | Detail |
|---|---|
| Schema | 34 tables, 11 views, 4 functions; migrations 001–006 |
| Workflow engine | declarative loader, validator, `next_options`, `record_event` |
| Registration | order + N service lines + tests + attachments |
| Order management | derived status, current stage, recommended next |
| Order view | tracker, service fulfilment, history, **approval** |
| Quarantine | order-level bench reception, glasshouse + growthroom |
| Initiation | first sample records, atomic code minting |
| Administration | 11 reference tables, one generic module |
| Reporting | versioning, approval gate, dispatch queue, disposition (schema) |
| Design system | `app/static/css/style.css`, linked once |

**Not yet implemented**

Virus indexing · thermotherapy · meristem culture · surface sterilization ·
subculture (and its fan-out) · hardening · the shared allocation component ·
conservation and distribution modules · the dispatch sender · the disposition
UI.

**Open questions**

- `surface_sterilization` terminal state: `complete` vs `completed`, and
  whether it reads as `healthy`.
- Seed vocabulary (`002_seed.sql` labels) against KEPHIS house terms — these
  were inferred and need a domain review.
- Whether allocation and dispatch are one act or two in KEPHIS practice.
