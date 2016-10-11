


pd.cohort <- enrolled.subjects %>%
  filter(enroll_cat=="PD")


## selecting the baseline, 12M, 24M, 36M, 48M, 60M, Premature Withdrawal and Symptomatic Therapy visits
select.event <- c("BL", "V04", "V06", "V08",
                  "V10", "V12", "PW", "ST")

ppmi.full <- ppmi.full %>%
  filter(event_id %in% select.event)

ppmi.pd <- ppmi.full %>%
  filter(event_id %in% select.event) %>%
  semi_join(pd.cohort, by="patno")



### MDS UPDRS - part i
updrs.i.pd <- mds_updrs_part_i %>%
  semi_join(pd.cohort, by="patno")
### MDS UPDRS - part i questionnaire
updrs.iq.pd <- mds_updrs_part_i_questionnaire %>%
  semi_join(pd.cohort, by="patno")
### MDS UPDRS - part ii questionnaire
updrs.ii.pd <- mds_updrs_part_ii_questionnaire %>%
  semi_join(pd.cohort, by="patno")
### MDS UPDRS - part iii
updrs.iii.pd <- mds_updrs_part_iii_post_dose %>%
  semi_join(pd.cohort, by="patno")
### MDS UPDRS - part iv
updrs.iv.pd <- mds_updrs_part_iv %>%
  semi_join(pd.cohort, by="patno")
### Modified Schwab & England ADL
mmseadl.pd <- modified_schwab_england_adl %>%
  semi_join(pd.cohort, by="patno")
### Benton Judgment of Line Orientation
benton.pd <- benton_judgment_of_line_orientation %>%
  semi_join(pd.cohort, by="patno")
### Hopkins Verbal Learning Test - Revised
hvlt.pd <- hopkins_verbal_learning_test %>%
  semi_join(pd.cohort, by="patno")
### Letter Number Sequencing
lnspd.pd <- letter_number_sequencing_pd %>%
  semi_join(pd.cohort, by="patno")
### Montreal Cognitive Assessment (MoCA)
moca.pd <- montreal_cognitive_assessment_moca %>%
  semi_join(pd.cohort, by="patno")
### Semantic Fluency
sft.pd <- semantic_fluency %>%
  semi_join(pd.cohort, by="patno")
### Symbol Digit Modalities Test
sdm.pd <- symbol_digit_modalities %>%
  semi_join(pd.cohort, by="patno")
### Epworth Sleepiness Scale
ess.pd <- epworth_sleepiness_scale %>%
  semi_join(pd.cohort, by="patno")
### REM Sleep Behavior Questionnaire
rem.pd <- rem_sleep_disorder_questionnaire %>%
  semi_join(pd.cohort, by="patno")
### Geriatric Depression Scale (GDS-15)
gds.pd <- geriatric_depression_scale_short %>%
  semi_join(pd.cohort, by="patno")
### Questionnaire for Impulsive-Compulsive Disorders (QUIP-Current-Short)
quip.pd <- quip_current_short %>%
  semi_join(pd.cohort, by="patno")
### State-Trait Anxiety Inventory for Adults
stai.pd <- state_trait_anxiety_inventory %>%
  semi_join(pd.cohort, by="patno")
### SCOPA-AUT
scopa.pd <- scopa_aut %>%
  semi_join(pd.cohort, by="patno")

