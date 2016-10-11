


ppmi.full <- mds_updrs_part_i  %>%
  full_join(mds_updrs_part_i_questionnaire,  by=c("patno", "event_id", "timefr")) %>%
  full_join(mds_updrs_part_ii_questionnaire, by=c("patno", "event_id", "timefr")) %>%
  full_join(mds_updrs_part_iii_post_dose,    by=c("patno", "event_id", "timefr")) %>%
  full_join(mds_updrs_part_iv,               by=c("patno", "event_id", "timefr")) %>%
  full_join(modified_schwab_england_adl,     by=c("patno", "event_id", "timefr")) %>%
  full_join(benton_judgment_of_line_orientation,  by=c("patno", "event_id", "timefr")) %>%
  full_join(hopkins_verbal_learning_test ,        by=c("patno", "event_id", "timefr")) %>%
  full_join(letter_number_sequencing_pd,          by=c("patno", "event_id", "timefr")) %>%
  full_join(montreal_cognitive_assessment_moca,   by=c("patno", "event_id", "timefr")) %>%
  full_join(semantic_fluency,                     by=c("patno", "event_id", "timefr")) %>%
  full_join(symbol_digit_modalities,              by=c("patno", "event_id", "timefr")) %>%
  full_join(epworth_sleepiness_scale,             by=c("patno", "event_id", "timefr")) %>%
  full_join(rem_sleep_disorder_questionnaire,     by=c("patno", "event_id", "timefr")) %>%
  full_join(geriatric_depression_scale_short,     by=c("patno", "event_id", "timefr")) %>%
  full_join(quip_current_short,                   by=c("patno", "event_id", "timefr")) %>%
  full_join(state_trait_anxiety_inventory,        by=c("patno", "event_id", "timefr")) %>%
  full_join(scopa_aut,                            by=c("patno", "event_id", "timefr")) %>%
  arrange(patno, event_id) %>%
  mutate(nmissing = is.na(nupdrs1.pag_name) +
           is.na(nupdrs1p.pag_name) +
           is.na(nupdrs2p.pag_name) +
           is.na(nupdrs3.pag_name) +
           is.na(nupdrs4.pag_name) +
           is.na(modseadl.pag_name) +
           is.na(bentonev.pag_name) +
           is.na(hvlt.pag_name) +
           is.na(lnspd.pag_name) +
           is.na(moca.pag_name) +
           is.na(sft.pag_name) +
           is.na(sdm.pag_name) +
           is.na(epworth.pag_name) +
           is.na(remsleep.pag_name) +
           is.na(gdsshort.pag_name) +
           is.na(quipcs.pag_name) +
           is.na(stai.pag_name) +
           is.na(scopaaut.pag_name)) %>%
  group_by(patno) %>%
  mutate(n = n()) %>%
  ungroup()


visit.avg <- ppmi.full %>%
  select(patno, event_id, contains("visit")) %>%
  gather(key, visit, 3:20) %>%
  arrange(patno, event_id) %>%
  mutate(xxx = paste(patno, event_id)) %>%
  group_by(xxx) %>%
  summarize(patno = patno[1],
            event_id = event_id[1],
            visit = mean(visit, na.rm=TRUE)) %>%
  select(-xxx) %>%
  arrange(patno)

ppmi.full <- ppmi.full %>%
  left_join(visit.avg, by = c("patno", "event_id")) %>%
  mutate(year5 =visit/5)


ppmi.full <- ppmi.full %>%
  mutate(np2f1 = rowSums(.[c("np2spch", "np2salv", "np2swal", "np2hwrt", "np2hobb")], na.rm=TRUE),
         np2f2 = rowSums(.[c("np2eat", "np2trmr")], na.rm=TRUE),
         np2f3 = rowSums(.[c("np2dres", "np2hygn", "np2turn", "np2rise", "np2walk", "np2frez")], na.rm=TRUE),
         np3f1 = rowSums(.[c("np3spch", "np3facxp", "np3risng", "np3gait", "np3frzgt", "np3pstbl", "np3postr", "np3brady")], na.rm=TRUE),
         np3f2 = rowSums(.[c("np3rtaru", "np3rtalu", "np3rtarl", "np3rtall", "np3rtalj", "np3rtcon")], na.rm=TRUE),
         np3f3 = rowSums(.[c("np3rign", "np3rigru", "np3riglu", "pn3rigrl", "np3rigll")], na.rm=TRUE),
         np3f4 = rowSums(.[c("np3ftapr", "np3hmovr", "np3prspr")], na.rm=TRUE),
         np3f5 = rowSums(.[c("np3ftapl", "np3hmovl", "np3prspl")], na.rm=TRUE),
         np3f6 = rowSums(.[c("np3ptrmr", "np3ptrml", "np3ktrmr", "np3ktrml")], na.rm=TRUE),
         np3f7 = rowSums(.[c("np3ttapr", "np3ttapl", "np3lgagr", "np3lgagl")], na.rm=TRUE),
         scauf1 = rowSums(.[c("scau1", "scau2", "scau3", "scau4", "scau5", "scau6", "scau7")], na.rm=TRUE),
         scauf2 = rowSums(.[c("scau8", "scau9", "scau10", "scau11", "scau12", "scau13")], na.rm=TRUE),
         scauf3 = rowSums(.[c("scau14", "scau15", "scau16")], na.rm=TRUE),
         scauf4 = rowSums(.[c("scau17", "scau18", "scau20", "scau21")], na.rm=TRUE))


enrolled.subjects <- patient_status %>%
  semi_join(randomization_table, by="patno")

ppmi.full <- ppmi.full %>%
  semi_join(enrolled.subjects, by = "patno")

ppmi.full <- ppmi.full %>%
  left_join(enrolled.subjects, by = "patno")

write.csv(ppmi.full, file = "c:/work/ppmi/posted/data/derived/ppmi-full.csv")
write.dta(ppmi.full, file = "c:/work/ppmi/posted/data/derived/ppmi-full.dta")

