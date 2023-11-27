#Filtering 




Time_5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["5","upr"])%>%
  filter(VSc > VSc_model_AFC["5","lwr"])%>%
  filter(VSc < VSc_model_AFC["5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["5","upr"])%>%
  filter(SrY > SrY_model_AFC["5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["5","upr"])%>%
  add_column(Date = 5)
Time_5Ma


Time_5.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["5.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["5.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["5.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["5.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["5.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["5.5","upr"])%>%
  filter(VSc > VSc_model_AFC["5.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["5.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["5.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["5.5","upr"])%>%
  filter(SrY > SrY_model_AFC["5.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["5.5","upr"])%>%
  add_column(Date = 5.5)
Time_5.5Ma

Time_6Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["6","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["6","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["6","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["6","upr"])%>%
  filter(Th_ppm>Th_model_AFC["6","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["6","upr"])%>%
  filter(VSc > VSc_model_AFC["6","lwr"])%>%
  filter(VSc < VSc_model_AFC["6","upr"])%>%
  filter(DyYb > DyYb_model_AFC["6","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["6","upr"])%>%
  filter(SrY > SrY_model_AFC["6","lwr"])%>% 
  filter(SrY < SrY_model_AFC["6","upr"])%>%
  add_column(Date = 6)
  
Time_6Ma

Time_6.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["6.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["6.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["6.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["6.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["6.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["6.5","upr"])%>%
  filter(VSc > VSc_model_AFC["6.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["6.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["6.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["6.5","upr"])%>%
  filter(SrY > SrY_model_AFC["6.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["6.5","upr"])%>%
  add_column(Date = 6.5)
Time_6.5Ma

Time_7Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["7","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["7","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["7","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["7","upr"])%>%
  filter(Th_ppm>Th_model_AFC["7","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["7","upr"])%>%
  filter(VSc > VSc_model_AFC["7","lwr"])%>%
  filter(VSc < VSc_model_AFC["7","upr"])%>%
  filter(DyYb > DyYb_model_AFC["7","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["7","upr"])%>%
  filter(SrY > SrY_model_AFC["7","lwr"])%>% 
  filter(SrY < SrY_model_AFC["7","upr"])%>%
  add_column(Date = 7)
Time_7Ma

Time_7.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["7.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["7.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["7.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["7.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["7.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["7.5","upr"])%>%
  filter(VSc > VSc_model_AFC["7.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["7.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["7.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["7.5","upr"])%>%
  filter(SrY > SrY_model_AFC["7.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["7.5","upr"])%>%
  add_column(Date = 7.5)
Time_7.5Ma

Time_8Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["8","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["8","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["8","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["8","upr"])%>%
  filter(Th_ppm>Th_model_AFC["8","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["8","upr"])%>%
  filter(VSc > VSc_model_AFC["8","lwr"])%>%
  filter(VSc < VSc_model_AFC["8","upr"])%>%
  filter(DyYb > DyYb_model_AFC["8","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["8","upr"])%>%
  filter(SrY > SrY_model_AFC["8","lwr"])%>% 
  filter(SrY < SrY_model_AFC["8","upr"])%>%
  add_column(Date = 8)
Time_8Ma

Time_8.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["8.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["8.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["8.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["8.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["8.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["8.5","upr"])%>%
  filter(VSc > VSc_model_AFC["8.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["8.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["8.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["8.5","upr"])%>%
  filter(SrY > SrY_model_AFC["8.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["8.5","upr"])%>%
  add_column(Date = 8.5)
Time_8.5Ma

Time_9Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["9","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["9","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["9","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["9","upr"])%>%
  filter(Th_ppm>Th_model_AFC["9","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["9","upr"])%>%
  filter(VSc > VSc_model_AFC["9","lwr"])%>%
  filter(VSc < VSc_model_AFC["9","upr"])%>%
  filter(DyYb > DyYb_model_AFC["9","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["9","upr"])%>%
  filter(SrY > SrY_model_AFC["9","lwr"])%>% 
  filter(SrY < SrY_model_AFC["9","upr"])%>%
  add_column(Date = 9)
Time_9Ma

Time_9.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["9.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["9.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["9.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["9.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["9.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["9.5","upr"])%>%
  filter(VSc > VSc_model_AFC["9.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["9.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["9.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["9.5","upr"])%>%
  filter(SrY > SrY_model_AFC["9.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["9.5","upr"])%>%
  add_column(Date = 9.5)
Time_9.5Ma

Time_10Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["10","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["10","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["10","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["10","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["10","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["10","upr"])%>%
  filter(DyYb > DyYb_model_AFC["10","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["10","upr"])%>%
  filter(VSc > VSc_model_AFC["10","lwr"])%>%
  filter(VSc < VSc_model_AFC["10","upr"])%>%
  filter(SrY > SrY_model_AFC["10","lwr"])%>% 
  filter(SrY < SrY_model_AFC["10","upr"])%>%
  add_column(Date = 10)
Time_10Ma

Time_10.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["10.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["10.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["10.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["10.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["10.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["10.5","upr"])%>%
  filter(VSc > VSc_model_AFC["10.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["10.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["10.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["10.5","upr"])%>%
  filter(SrY > SrY_model_AFC["10.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["10.5","upr"])%>%
  add_column(Date = 10.5)
Time_10.5Ma

Time_11Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["11","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["11","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["11","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["11","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["11","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["11","upr"])%>%
  filter(DyYb > DyYb_model_AFC["11","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["11","upr"])%>%
  filter(VSc > VSc_model_AFC["11","lwr"])%>%
  filter(VSc < VSc_model_AFC["11","upr"])%>%
  filter(SrY > SrY_model_AFC["11","lwr"])%>% 
  filter(SrY < SrY_model_AFC["11","upr"])%>%
  add_column(Date = 11)
Time_11Ma


#prepare data in two groups to reduce computing
filter1 <- do.call(rbind, lapply(ls(pattern = "Time"), get))


select1 <-list(Time_5.5Ma, Time_8Ma, Time_10.5Ma)
violin1 <- do.call(rbind, select1)


violin1<- violin1%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

filter1<- filter1%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

write.csv(filter1,"temp/filter1.csv")
write.csv(violin1,"temp/violin1.csv")

rm(list = ls()[grepl("Ma$", ls())])#deletes everything ending with MC


Time_11.5Ma <- AFC_tot.df%>%
  filter(Th_ppm>Th_model_AFC["11.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["11.5","upr"])%>%
  filter(BaTh > BaTh_model_AFC["11.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["11.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["11.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["11.5","upr"])%>%
  filter(VSc > VSc_model_AFC["11.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["11.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["11.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["11.5","upr"])%>%
  filter(SrY > SrY_model_AFC["11.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["11.5","upr"])%>%
  add_column(Date = 11.5)
Time_11.5Ma

Time_12Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["12","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["12","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["12","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["12","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["12","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["12","upr"])%>%
  filter(DyYb > DyYb_model_AFC["12","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["12","upr"])%>%
  filter(VSc > VSc_model_AFC["12","lwr"])%>%
  filter(VSc < VSc_model_AFC["12","upr"])%>%
  filter(SrY > SrY_model_AFC["12","lwr"])%>% 
  filter(SrY < SrY_model_AFC["12","upr"])%>%
  add_column(Date = 12)
Time_12Ma

Time_12.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["12.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["12.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["12.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["12.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["12.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["12.5","upr"])%>%
  filter(VSc > VSc_model_AFC["12.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["12.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["12.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["12.5","upr"])%>%
  filter(SrY > SrY_model_AFC["12.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["12.5","upr"])%>%
  add_column(Date = 12.5)
Time_12.5Ma

Time_13Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["13","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["13","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["13","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["13","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["13","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["13","upr"])%>%
  filter(DyYb > DyYb_model_AFC["13","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["13","upr"])%>%
  filter(VSc > VSc_model_AFC["13","lwr"])%>%
  filter(VSc < VSc_model_AFC["13","upr"])%>%
  filter(SrY > SrY_model_AFC["13","lwr"])%>% 
  filter(SrY < SrY_model_AFC["13","upr"])%>%
  add_column(Date = 13)
Time_13Ma

Time_13.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["13.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["13.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["13.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["13.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["13.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["13.5","upr"])%>%
  filter(VSc > VSc_model_AFC["13.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["13.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["13.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["13.5","upr"])%>%
  filter(SrY > SrY_model_AFC["13.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["13.5","upr"])%>%
  add_column(Date = 13.5)
Time_13.5Ma

Time_14Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["14","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["14","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["14","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["14","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["14","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["14","upr"])%>%
  filter(DyYb > DyYb_model_AFC["14","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["14","upr"])%>%
  filter(VSc > VSc_model_AFC["14","lwr"])%>%
  filter(VSc < VSc_model_AFC["14","upr"])%>%
  filter(SrY > SrY_model_AFC["14","lwr"])%>% 
  filter(SrY < SrY_model_AFC["14","upr"])%>%
  add_column(Date = 14)
Time_14Ma

Time_14.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["14.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["14.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["14.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["14.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["14.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["14.5","upr"])%>%
  filter(VSc > VSc_model_AFC["14.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["14.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["14.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["14.5","upr"])%>%
  filter(SrY > SrY_model_AFC["14.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["14.5","upr"])%>%
  add_column(Date = 14.5)
Time_14.5Ma

Time_15Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["15","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["15","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["15","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["15","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["15","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["15","upr"])%>%
  filter(DyYb > DyYb_model_AFC["15","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["15","upr"])%>%
  filter(VSc > VSc_model_AFC["15","lwr"])%>%
  filter(VSc < VSc_model_AFC["15","upr"])%>%
  filter(SrY > SrY_model_AFC["15","lwr"])%>% 
  filter(SrY < SrY_model_AFC["15","upr"])%>%
  add_column(Date = 15)
Time_15Ma

Time_15.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["15.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["15.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["15.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["15.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["15.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["15.5","upr"])%>%
  filter(VSc > VSc_model_AFC["15.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["15.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["15.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["15.5","upr"])%>%
  filter(SrY > SrY_model_AFC["15.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["15.5","upr"])%>%
  add_column(Date = 15.5)
Time_15.5Ma

filter2 <- do.call(rbind, lapply(ls(pattern = "Time"), get))


select2 <-list(Time_13Ma, Time_15.5Ma)
violin2 <- do.call(rbind, select2)

violin2<- violin2%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

filter2<- filter2%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

write.csv(filter2,"temp/filter2.csv")
write.csv(violin2,"temp/violin2.csv")

rm(list = ls()[grepl("Ma$", ls())])#deletes everything ending with MC


Time_16Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["16","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["16","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["16","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["16","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["16","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["16","upr"])%>%
  filter(DyYb > DyYb_model_AFC["16","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["16","upr"])%>%
  filter(VSc > VSc_model_AFC["16","lwr"])%>%
  filter(VSc < VSc_model_AFC["16","upr"])%>%
  filter(SrY > SrY_model_AFC["16","lwr"])%>% 
  filter(SrY < SrY_model_AFC["16","upr"])%>%
  add_column(Date = 16)
Time_16Ma

Time_16.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["16.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["16.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["16.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["16.5","upr"])%>%
  filter(Th_ppm>Th_model_AFC["16.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["16.5","upr"])%>%
  filter(VSc > VSc_model_AFC["16.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["16.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["16.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["16.5","upr"])%>%
  filter(SrY > SrY_model_AFC["16.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["16.5","upr"])%>%
  add_column(Date = 16.5)
Time_16.5Ma

Time_17Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["17","lwr"]) %>%
  filter(ThLa > ThLa_model_AFC["17","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["17","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["17","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["17","upr"])%>%
  filter(BaTh < BaTh_model_AFC["17","upr"]) %>%
  filter(DyYb > DyYb_model_AFC["17","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["17","upr"])%>%
  filter(VSc > VSc_model_AFC["17","lwr"])%>%
  filter(VSc < VSc_model_AFC["17","upr"])%>%
  filter(SrY > SrY_model_AFC["17","lwr"])%>% 
  filter(SrY < SrY_model_AFC["17","upr"])%>%
  add_column(Date = 17)
Time_17Ma

Time_17.5Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["17.5","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["17.5","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["17.5","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["17.5","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["17.5","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["17.5","upr"])%>%
  filter(DyYb > DyYb_model_AFC["17.5","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["17.5","upr"])%>%
  filter(VSc > VSc_model_AFC["17.5","lwr"])%>%
  filter(VSc < VSc_model_AFC["17.5","upr"])%>%
  filter(SrY > SrY_model_AFC["17.5","lwr"])%>% 
  filter(SrY < SrY_model_AFC["17.5","upr"])%>%
  add_column(Date = 17.5)
Time_17.5Ma

Time_18Ma <- AFC_tot.df%>%
  filter(BaTh > BaTh_model_AFC["18","lwr"]) %>%
  filter(BaTh < BaTh_model_AFC["18","upr"]) %>%
  filter(ThLa > ThLa_model_AFC["18","lwr"])%>%
  filter(ThLa < ThLa_model_AFC["18","upr"])%>% 
  filter(Th_ppm>Th_model_AFC["18","lwr"])%>%
  filter(Th_ppm<Th_model_AFC["18","upr"])%>%
  filter(DyYb > DyYb_model_AFC["18","lwr"])%>%
  filter(DyYb < DyYb_model_AFC["18","upr"])%>%
  filter(VSc > VSc_model_AFC["18","lwr"])%>%
  filter(VSc < VSc_model_AFC["18","upr"])%>%
  filter(SrY > SrY_model_AFC["18","lwr"])%>% 
  filter(SrY < SrY_model_AFC["18","upr"])%>%
  add_column(Date = 18)
Time_18Ma


filter3 <- do.call(rbind, lapply(ls(pattern = "Time"), get))


select3 <-list(Time_18Ma)
violin3 <- do.call(rbind, select2)

violin3<- violin3%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

filter3<- filter3%>%
  inner_join(Info.df, by = "Run")%>%
  mutate(AmpPlg = Xi_AmpMC/Xi_PlgMC)%>%
  mutate(PlgAmp = Xi_PlgMC/Xi_AmpMC)%>%
  mutate(Ca_BaTh = Ca_Ba/Ca_Th)%>%
  mutate(Ca_SrY = Ca_Sr/Ca_Y)%>%
  mutate(Ca_VSc = Ca_V/Ca_Sc)%>%
  mutate(Ca_DyYb = Ca_Dy/Ca_Yb)%>%
  mutate(Ca_ThLa = Ca_Th/Ca_La)%>%
  mutate(GrtAmp = Xi_GrtMC/Xi_AmpMC)

write.csv(filter3,"temp/filter3.csv")
write.csv(violin3,"temp/violin3.csv")

rm(list = ls()[grepl("Ma$", ls())])#deletes everything ending with MC

filter1 <- read.csv("temp/filter1.csv")
violin1 <- read.csv("temp/violin1.csv")
filter2 <- read.csv("temp/filter2.csv")
violin2 <- read.csv("temp/violin2.csv")
filter3 <- read.csv("temp/filter3.csv")
violin3 <- read.csv("temp/violin3.csv")

violin <- do.call(rbind, lapply(ls(pattern = "violin"), get))
violin$Date <- as.factor(violin$Date)

filter <- rbind(filter1, filter2, filter3)
filter$Date <- as.factor(filter$Date)

filter<- filter%>%
  mutate(CpxPlg = Xi_CpxMC/Xi_PlgMC)%>%
  mutate(OpxPlg = Xi_OpxMC/Xi_PlgMC)%>%
  mutate(CpxAmp = Xi_CpxMC/Xi_AmpMC)


Plot<-
  filter %>% 
  group_by(Date) %>%
  summarise(N= length(Run), Runs = n_distinct(Run), medianCa_BaTh = median(Ca_BaTh), medianCa_ThLa = median(Ca_ThLa),
            medianCa_SrY = median(Ca_SrY), medianCa_VSc = median(Ca_VSc),medianCa_DyYb = median(Ca_DyYb),
            medianAmpPlg = median(AmpPlg), meanGrtAmp = mean(GrtAmp), medianGrtAmp = median(GrtAmp), 
            medianCpxPlg = median(CpxPlg), medianOpxPlg = median(OpxPlg), medianCpxAmp = median(CpxAmp),
            medianFraction= median(Fraction), medianR_Assim= median(R_Assim),
            medianCa_Th= median(Ca_Th),  medianCa_Ba= median(Ca_Ba), medianCa_Sr= median(Ca_Sr), medianCa_Y= median(Ca_Y), 
            medianCa_La= median(Ca_La),  medianCa_Dy= median(Ca_Dy), medianCa_Yb= median(Ca_Yb), 
            medianAmp= median(Xi_AmpMC), medianPlg= median(Xi_PlgMC), meanGrt= mean(Xi_GrtMC), 
            medianApa= median(Xi_ApaMC),  medianCpx= median(Xi_CpxMC),  medianOpx= median(Xi_OpxMC), 
            medianRut= median(Xi_RutMC), medianMgt= median(Xi_MgtMC))
Plot
write.csv(Plot,"temp/summary.csv", row.names = FALSE)
write.csv(violin,"temp/violin.csv")
write.csv(filter,"temp/filter.csv")

Xi <- ggplot(Plot)+ #no pipes with plots but +
  geom_path(aes(x= Date, y = medianAmp, group = 1),  size = 1, colour = "chartreuse3")+ #lines connecting run data
  geom_point(aes(x= Date, y = medianAmp, group = 1), size = 2, colour = "chartreuse3")+ #points coloured and sized for certain information
  geom_path(aes(x= Date, y = medianPlg, group = 1), size = 1,colour = "cornflowerblue")+
  geom_point(aes(x= Date, y = medianPlg, group = 1), size = 2, colour = "cornflowerblue")+
  geom_path(aes(x= Date, y = medianCpx, group = 1), size = 1,colour = "burlywood4")+
  geom_point(aes(x= Date, y = medianCpx, group = 1), size = 2,colour = "burlywood4")+
  geom_path(aes(x= Date, y = medianOpx, group = 1), size = 1,colour = "cornsilk3")+
  geom_point(aes(x= Date, y = medianOpx, group = 1), size = 2,colour = "cornsilk3")+
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian(ylim = c(0, 0.75), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Xi")

Minerals <- ggplot(Plot)+ #no pipes with plots but +
  geom_path(aes(x= Date, y = medianAmpPlg, group = 1),  size = 1, colour = "chartreuse3")+ #lines connecting run data
  geom_point(aes(x= Date, y = medianAmpPlg, group = 1), size = 2, colour = "chartreuse3")+ #points coloured and sized for certain information
  geom_path(aes(x= Date, y = medianCpxPlg, group = 1), size = 1,colour = "cornflowerblue")+
  geom_point(aes(x= Date, y = medianCpxPlg, group = 1), size = 2, colour = "cornflowerblue")+
  geom_path(aes(x= Date, y = medianCpxAmp, group = 1), size = 1,colour = "burlywood4")+
  geom_point(aes(x= Date, y = medianCpxAmp, group = 1), size = 2,colour = "burlywood4")+
  geom_path(aes(x= Date, y = medianOpxPlg, group = 1), size = 1,colour = "cornsilk3")+
  geom_point(aes(x= Date, y = medianOpxPlg, group = 1), size = 2,colour = "cornsilk3")+
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian(ylim = c(0, 3), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Xi")

Apa <- ggplot(Plot,aes(x= Date, y = medianApa, group = 1))+ #no pipes with plots but +
  geom_path(size = 1,)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  coord_cartesian(ylim = c(0, 0.07), expand = FALSE)+
  theme_bw()+
  theme(aspect.ratio=1)+
  xlab("Date (Ma)")+
  ylab("Xi Apa")
Apa

AmpPlg <- ggplot(Plot,aes(x= Date, y = medianAmpPlg, group = 1))+ #no pipes with plots but +
  geom_path(size = 1,)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 8), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Xi Amphibole / Xi Plagioclase")

GrtAmp <- ggplot(Plot,aes(x= Date, y = medianGrtAmp, group = 1))+ #no pipes with plots but +
  geom_path(size = 1,)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 0.07), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Xi Garnet /  Xi Amphibole")

meanGrtAmp <- ggplot(Plot,aes(x= Date, y = meanGrtAmp, group = 1))+ #no pipes with plots but +
  geom_path(size = 1,)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 0.1), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Xi Garnet /  Xi Amphibole")

Ca_BaTh <- ggplot(Plot,aes(x= Date, y = medianCa_BaTh, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 1300), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ba/Th of assimilant")

R_Ass <- ggplot(Plot,aes(x= Date, y = medianR_Assim, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 0.7), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Assimilation")

Melt <- ggplot(Plot,aes(x= Date, y = medianFraction, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0.3, 1), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Melt content")

Depth<- ggplot(Plot,aes(x= Date, y = depth))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian(xlim = c(4, 18.5), ylim = c(0, 1), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("depth")

Ca_ThLa <- ggplot(Plot,aes(x= Date, y = medianCa_ThLa, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian(ylim = c(0, 2), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_Th/La")

Ca_SrY <- ggplot(Plot,aes(x= Date, y = medianCa_SrY, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian(ylim = c(0, 60), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_Sr/Y")

Ca_VSc <- ggplot(Plot,aes(x= Date, y = medianCa_VSc, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 15), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_V/Sc")

Ca_DyYb <- ggplot(Plot,aes(x= Date, y = medianCa_DyYb, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(1.0, 3.2), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_DyYb")

Ca_Th <- ggplot(Plot,aes(x= Date, y = medianCa_Th, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 20), expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_Th")
Ca_Th

Ca_Ba <- ggplot(Plot,aes(x= Date, y = medianCa_Ba, group = 1))+ #no pipes with plots but +
  geom_path(size = 1)+ #lines connecting run data
  geom_point(size = 2)+ #points coloured and sized for certain information
  theme_bw()+
  theme(aspect.ratio=1)+
  coord_cartesian( ylim = c(0, 1000),  expand = FALSE)+
  xlab("Date (Ma)")+
  ylab("Ca_Ba")
Ca_Ba


ggplot(violin.df, aes(x= Date, y= Ca_DyYb)) + 
  geom_violin(trim=FALSE, scale = "width",aes(fill = Date))+
  fill_palette(palette="jco")+
  xlab("Date (Ma)")+ 
  scale_y_continuous("y",  limits = c(0,5), expand = c(0,0))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

Evolution<-ggarrange(Xi, Melt, Minerals, R_Ass, GrtAmp,Ca_BaTh,Ca_SrY,Ca_ThLa,Ca_DyYb,Ca_VSc,Ca_Th,Ca_Ba, #name the plots you want to combine
                   labels = c("A", "B", "C", "D", "E", "F"), #Labels of the diagram
                   ncol = 3, nrow = 4,
                   common.legend = TRUE, legend = "bottom")
Evolution



