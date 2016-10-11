
### Read in the datafiles
randomization_table    <- read.ppmi(data.folder, "Randomization_table.csv")
patient_status         <- read.ppmi(data.folder, "Patient_Status.csv")
screening_demographics <- read.ppmi(data.folder, "Screening___Demographics.csv")
#motor assessments
mds_updrs_part_i                <- read.ppmi(data.folder, "MDS_UPDRS_Part_I.csv")
mds_updrs_part_i_questionnaire  <- read.ppmi(data.folder, "MDS_UPDRS_Part_I__Patient_Questionnaire.csv")
mds_updrs_part_ii_questionnaire <- read.ppmi(data.folder, "MDS_UPDRS_Part_II__Patient_Questionnaire.csv")
mds_updrs_part_iii_post_dose    <- read.ppmi(data.folder, "MDS_UPDRS_Part_III__Post_Dose_.csv")
mds_updrs_part_iv               <- read.ppmi(data.folder, "MDS_UPDRS_Part_IV.csv")
modified_schwab_england_adl     <- read.ppmi(data.folder, "Modified_Schwab_+_England_ADL.csv")
#neuropsych
benton_judgment_of_line_orientation <- read.ppmi(data.folder, "Benton_Judgment_of_Line_Orientation.csv")
hopkins_verbal_learning_test        <- read.ppmi(data.folder, "Hopkins_Verbal_Learning_Test.csv")
letter_number_sequencing_pd         <- read.ppmi(data.folder, "Letter_-_Number_Sequencing__PD_.csv")
montreal_cognitive_assessment_moca  <- read.ppmi(data.folder, "Montreal_Cognitive_Assessment__MoCA_.csv")
semantic_fluency                    <- read.ppmi(data.folder, "Semantic_Fluency.csv")
symbol_digit_modalities             <- read.ppmi(data.folder, "Symbol_Digit_Modalities.csv")
#sleep disorder
epworth_sleepiness_scale         <- read.ppmi(data.folder, "Epworth_Sleepiness_Scale.csv")
rem_sleep_disorder_questionnaire <- read.ppmi(data.folder, "REM_Sleep_Disorder_Questionnaire.csv")
#neurobehavioral
geriatric_depression_scale_short <- read.ppmi(data.folder, "Geriatric_Depression_Scale__Short_.csv")
quip_current_short               <- read.ppmi(data.folder, "QUIP_Current_Short.csv")
state_trait_anxiety_inventory    <- read.ppmi(data.folder, "State-Trait_Anxiety_Inventory.csv")
#autonomic
scopa_aut <- read.ppmi(data.folder, "SCOPA-AUT.csv")


### Do some light data cleaning
randomization_table    <- clean.ppmi.random(randomization_table)
patient_status         <- clean.ppmi.patient(patient_status)
screening_demographics <- clean.ppmi.screen(screening_demographics)
#motor assessments
mds_updrs_part_i                <- clean.ppmi(mds_updrs_part_i)
mds_updrs_part_i_questionnaire  <- clean.ppmi(mds_updrs_part_i_questionnaire)
mds_updrs_part_ii_questionnaire <- clean.ppmi(mds_updrs_part_ii_questionnaire)
mds_updrs_part_iii_post_dose    <- clean.ppmi(mds_updrs_part_iii_post_dose)
mds_updrs_part_iv               <- clean.ppmi(mds_updrs_part_iv)
modified_schwab_england_adl     <- clean.ppmi(modified_schwab_england_adl)
#neuropsych
benton_judgment_of_line_orientation <- clean.ppmi(benton_judgment_of_line_orientation)
hopkins_verbal_learning_test        <- clean.ppmi(hopkins_verbal_learning_test)
letter_number_sequencing_pd         <- clean.ppmi(letter_number_sequencing_pd)
montreal_cognitive_assessment_moca  <- clean.ppmi(montreal_cognitive_assessment_moca)
semantic_fluency                    <- clean.ppmi(semantic_fluency)
symbol_digit_modalities             <- clean.ppmi(symbol_digit_modalities)
#sleep disorder
epworth_sleepiness_scale         <- clean.ppmi(epworth_sleepiness_scale)
rem_sleep_disorder_questionnaire <- clean.ppmi(rem_sleep_disorder_questionnaire)
#neurobehavioral
geriatric_depression_scale_short <- clean.ppmi(geriatric_depression_scale_short)
quip_current_short               <- clean.ppmi(quip_current_short)
state_trait_anxiety_inventory    <- clean.ppmi(state_trait_anxiety_inventory)
#autonomic
scopa_aut <- clean.ppmi(scopa_aut)


n.updrs.iii.drop<-table(mds_updrs_part_iii_post_dose$nupdrs3.pag_name)[2]
mds_updrs_part_iii_post_dose <- mds_updrs_part_iii_post_dose %>%
  filter(nupdrs3.pag_name != "NUPDRS3A")

mds_updrs_part_iii_post_dose <- mds_updrs_part_iii_post_dose %>%
  mutate(xxx = paste(paste(patno, event_id))) %>%
  group_by(xxx) %>%
  arrange(nupdrs3.last_update) %>%
  slice(n()) %>%
  ungroup()

benton_judgment_of_line_orientation <- benton_judgment_of_line_orientation %>%
  mutate(xxx = paste(paste(patno, event_id))) %>%
  group_by(xxx) %>%
  arrange(bentonev.last_update) %>%
  slice(n()) %>%
  ungroup()

## recoding the MoCA done at screening to baseline
montreal_cognitive_assessment_moca$event_id[montreal_cognitive_assessment_moca$event_id=="SC"] <- "BL"
montreal_cognitive_assessment_moca$timefr[montreal_cognitive_assessment_moca$event_id=="BL"] <- 0

montreal_cognitive_assessment_moca <- montreal_cognitive_assessment_moca %>%
  mutate(xxx = paste(paste(patno, event_id))) %>%
  group_by(xxx) %>%
  arrange(moca.last_update) %>%
  slice(n()) %>%
  ungroup()

montreal_cognitive_assessment_moca %>%
  select(patno, event_id, contains("visit")) %>%
  filter(patno==41411)

