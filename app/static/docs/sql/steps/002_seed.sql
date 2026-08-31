-- =====================================================================
-- RTB-EAGEL · Core seed data
-- Run after 001_core.sql.
--
-- These values are lifted from where they currently live HARDCODED:
--   * stages         <- cassava.yaml next_stage values
--   * services       <- the 8 columns of the old tbl_project_services
--   * sample types,
--     conditions,
--     parts, bags     <- R constants in new_order_details.R
--
-- They are data now, not code. Adding a sample type is an INSERT, not a
-- redeploy. Adjust the labels to match KEPHIS house style before running.
-- =====================================================================

BEGIN;

-- ---- Pipeline stages ------------------------------------------------
-- sort_order is the nominal pipeline position; branches share positions.
INSERT INTO tbl_stage (stage_code, label, sort_order, is_terminal) VALUES
    ('reception',                'Reception',                    10, false),
    ('quarantine_glasshouse',    'Quarantine glasshouse',        20, false),
    ('quarantine_growthroom',    'Quarantine growthroom',        20, false),
    ('molecular_virus_indexing', 'Molecular virus indexing',     30, false),
    ('grafting_virus_indexing',  'Grafting virus indexing',      30, false),
    ('thermotherapy',            'Thermotherapy',                40, false),
    ('meristem_culture',         'Meristem culture',             50, false),
    ('surface_sterilization',    'Surface sterilization',        50, false),
    ('subculture',               'Subculture / multiplication',  60, false),
    ('in_vitro_conservation',    'In vitro conservation',        70, false),
    ('in_vitro_distribution',    'In vitro distribution',        70, false),
    ('hardening',                'Hardening',                    70, false),
    ('in_vivo_conservation',     'In vivo conservation',         80, false),
    ('mini_tubers_distribution', 'Mini-tubers distribution',     80, false),
    ('tracking_complete',        'Workflow finished',            90, true),
    ('archived',                 'Archived',                    100, true);

-- ---- Service catalogue ----------------------------------------------
-- weight drives the completion rollup in view_order_progress. Diagnostic
-- services are weighted higher because an order cannot finish without
-- them; tune to taste.
INSERT INTO tbl_service_catalog
    (service_code, service_label, service_kind, unit, weight, sort_order) VALUES
    ('pathogen_detection',      'Pathogen detection',          'diagnostic', 'sample',   2.0, 10),
    ('subculture',              'Subculture / multiplication', 'fulfilment', 'plantlet', 1.0, 20),
    ('in_vitro_conservation',   'In vitro conservation',       'fulfilment', 'plantlet', 1.0, 30),
    ('in_vitro_distribution',   'In vitro distribution',       'fulfilment', 'plantlet', 1.0, 40),
    ('in_vivo_conservation',    'In vivo conservation',        'fulfilment', 'plant',    1.0, 50),
    ('mini_tuber_distribution', 'Mini-tuber distribution',     'fulfilment', 'tuber',    1.0, 60),
    ('cold_room',               'Cold room storage',           'fulfilment', 'plantlet', 1.0, 70),
    ('plantlets_distribution',  'Plantlets distribution',      'fulfilment', 'plantlet', 1.0, 80),
    ('vines_distribution',      'Vines distribution',          'fulfilment', 'vine',     1.0, 90);

-- ---- Controlled vocabularies ---------------------------------------
INSERT INTO tbl_sample_type (sample_type_code, label, sort_order) VALUES
    ('cutting',   'Cutting',    10),
    ('in_vitro',  'In vitro',   20),
    ('seed',      'Seed',       30),
    ('tuber',     'Tuber',      40),
    ('plantlet',  'Plantlet',   50);

INSERT INTO tbl_sample_condition (condition_code, label, sort_order) VALUES
    ('good',          'Good',           10),
    ('fair',          'Fair',           20),
    ('poor',          'Poor',           30),
    ('contaminated',  'Contaminated',   40);

-- part_id / bag_id are IDENTITY: let the DB assign them.
INSERT INTO tbl_sample_part (part_name, description) VALUES
    ('Stem',        NULL),
    ('Leaf',        NULL),
    ('Root',        NULL),
    ('Tuber',       NULL),
    ('Whole plant', NULL);

INSERT INTO tbl_sampling_bag (bag_name, description) VALUES
    ('Envelope',    NULL),
    ('Paper bag',   NULL),
    ('Plastic bag', NULL),
    ('Box',         NULL);

-- ---- State vocabulary -----------------------------------------------
-- The 14 distinct states in cassava.yaml, once the stage prefix is
-- stripped off the compound status string.
--
-- NOTE: the workflow contains BOTH `surface_sterilization_complete`
-- ("Healthy") and `surface_sterilization_completed` ("Completed") - two
-- different meanings one letter apart. Seeded here as 'healthy' and
-- 'completed'. Correct me if that reads the intent wrong.
INSERT INTO tbl_state (state_code, label, is_failure) VALUES
    ('logged',            'Logged',             false),
    ('transferred',       'Transferred',        false),
    ('received',          'Received',           false),
    ('established',       'Established',        false),
    ('inprogress',        'In progress',        false),
    ('results_available', 'Results available',  false),
    ('updated',           'Updated',            false),
    ('approved',          'Approved',           false),
    ('completed',         'Completed',          false),
    ('healthy',           'Healthy',            false),
    ('rejected',          'Rejected',           true),
    ('contaminated',      'Contaminated',       true),
    ('dead',              'Dead',               true),
    ('depleted',          'Depleted',           true);

-- ---- Legal (stage, state) pairs --------------------------------------
-- tbl_sample_event carries a composite FK here, so 'contaminated at
-- reception' is rejected by the database rather than accepted and puzzled
-- over later.
INSERT INTO tbl_stage_state (stage_code, state_code) VALUES
    ('reception','logged'), ('reception','approved'), ('reception','rejected'),

    ('quarantine_glasshouse','transferred'), ('quarantine_glasshouse','established'),
    ('quarantine_glasshouse','approved'),    ('quarantine_glasshouse','rejected'),

    ('quarantine_growthroom','transferred'), ('quarantine_growthroom','received'),
    ('quarantine_growthroom','approved'),    ('quarantine_growthroom','rejected'),

    ('molecular_virus_indexing','inprogress'), ('molecular_virus_indexing','results_available'),
    ('molecular_virus_indexing','approved'),   ('molecular_virus_indexing','rejected'),
    ('molecular_virus_indexing','completed'),

    ('grafting_virus_indexing','inprogress'), ('grafting_virus_indexing','results_available'),
    ('grafting_virus_indexing','approved'),   ('grafting_virus_indexing','rejected'),
    ('grafting_virus_indexing','completed'),

    ('thermotherapy','inprogress'), ('thermotherapy','updated'),
    ('thermotherapy','rejected'),   ('thermotherapy','completed'),

    ('meristem_culture','established'), ('meristem_culture','updated'),
    ('meristem_culture','rejected'),    ('meristem_culture','completed'),

    ('surface_sterilization','established'),  ('surface_sterilization','updated'),
    ('surface_sterilization','healthy'),      ('surface_sterilization','contaminated'),
    ('surface_sterilization','dead'),         ('surface_sterilization','depleted'),
    ('surface_sterilization','rejected'),     ('surface_sterilization','completed'),

    ('subculture','established'), ('subculture','updated'),
    ('subculture','rejected'),    ('subculture','completed'),

    ('in_vitro_conservation','established'), ('in_vitro_conservation','updated'),
    ('in_vitro_conservation','rejected'),    ('in_vitro_conservation','completed'),

    ('in_vitro_distribution','established'), ('in_vitro_distribution','updated'),
    ('in_vitro_distribution','rejected'),    ('in_vitro_distribution','completed'),

    ('hardening','established'), ('hardening','updated'),
    ('hardening','rejected'),    ('hardening','completed'),

    ('in_vivo_conservation','established'), ('in_vivo_conservation','updated'),
    ('in_vivo_conservation','rejected'),    ('in_vivo_conservation','completed'),

    ('mini_tubers_distribution','established'), ('mini_tubers_distribution','updated'),
    ('mini_tubers_distribution','rejected'),    ('mini_tubers_distribution','completed'),

    ('tracking_complete','completed'),
    ('archived','completed');

-- ---- Bootstrap user -------------------------------------------------
-- Needed because created_by/actor columns FK to tbl_app_user. Replace
-- with real accounts; 'system' is the actor for migrations and jobs.
INSERT INTO tbl_app_user (username, full_name, role) VALUES
    ('system', 'System', 'admin');

COMMIT;
