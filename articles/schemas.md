# Database schemas

To use comlandr, you will need permissions to access several databases.
The following is a list of the databases and the tables that you will
need to access. We recommend that you request read access to all tables
in the schemas. However below is a list of the current tables that are
used in the package.

| svdbs                      | stockeff                      | obdbs      | NEFSC_GARFO         |
|----------------------------|-------------------------------|------------|---------------------|
| survan_conversion_factors  | mv_cf_landings                | obspp      | maine_herring_catch |
| svcruise_purpose           | v_cf_stock_recent_stat_area_o | asmpp      | area                |
| length_weight_coefficients |                               | obinc      | gear                |
| union_fscs_svbio           |                               | obotgh     | loc                 |
| union_fscs_svsta           |                               | obspecconv | port                |
| union_fscs_svcat           |                               | obspec     | cfspp               |
| union_fscs_svlen           |                               |            | species_itis_ne     |
| svdbs_cruises              |                               |            | mstrvess            |
| fscs_maturity_codes        |                               |            |                     |
| sex_codes                  |                               |            |                     |
| fscs_sex_codes             |                               |            |                     |
| svspecies_list             |                               |            |                     |
| svmstrata                  |                               |            |                     |
| sv_vessel                  |                               |            |                     |
| mstr_cruise                |                               |            |                     |
