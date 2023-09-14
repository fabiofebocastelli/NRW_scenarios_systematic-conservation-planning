### analysis of the results ###


# merging solutions with existing SPA to get the overall representation of SPA (new ones + old ones)

# merging s1a with existing spa
s1a_reclass <- classify(s1, matrix(c(0, NA), nrow = 1))
spc1a<- sprc(s1a_reclass, existing_spa)
s1a_existingSPA<- merge(spc1a)
plot(s1a_existingSPA, main="scenario 5")


# merging s1b with existing spa
s1b_reclass <- classify(s1_wild_reclass, matrix(c(0, NA), nrow = 1))
spc1b<- sprc(s1b_reclass, existing_spa)
s1b_existingSPA<- merge(spc1b)
plot(s1b_existingSPA, main="scenario 1")


# merging s3 with existing spa
s3_reclass <- classify(s3, matrix(c(0, NA), nrow = 1))
spc3<- sprc(s3_reclass, existing_spa)
s3_existingSPA<- merge(spc3)
plot(s3_existingSPA, main="scenario 3")

#plot everything together
par( mfrow= c(2,2) )
plot(s1b_existingSPA, main="scenario 1")
plot(s3_existingSPA, main="scenario 3")
plot(s1a_existingSPA, main="scenario 5 (wilderness)")


#######################################################################################################################

# check the aggregation values to assess the spatial allocations in the scenarios

# Aggregation index (Aggregation metric)

landscapemetrics::lsm_l_ai(s3_existingSPA)

landscapemetrics::lsm_l_ai(s1a_existingSPA)

landscapemetrics::lsm_l_ai(s1b_existingSPA)


#####################################################################################################################

# evaluating feature representation

# scenario 1a
s1a_existingSPA_solution <- mask(
  subst(s1a_existingSPA, NA, 0),
  tfc_const_costs
)
stats_1a <- print(eval_feature_representation_summary(p1, s1a_existingSPA_solution), n = 29)

# scenario 1b
s1b_existingSPA_solution <- mask(
  subst(s1b_existingSPA, NA, 0),
  tfc_const_costs
)

stats_1b <- print(eval_feature_representation_summary(p1, s1b_existingSPA_solution), n = 29)

# scenario 3
s3_existingSPA_solution <- mask(
  subst(s3_existingSPA, NA, 0),
  tfc_const_costs
)

stats_3 <- print(eval_feature_representation_summary(p1, s3_existingSPA_solution), n = 29)

write.table(stats_1a, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/results_1a.csv")
 
write.table(stats_1b, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/results_1b.csv")
 
write.table(stats_3, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/results_3.csv")

