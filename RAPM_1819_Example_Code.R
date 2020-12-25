### CLEAN NAMES - THIS WAY WE CAN REDUCE TO EACH INDIVIDUAL SKATER ###

colnames(combined_shifts_dummies) = gsub("offense_1", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_2", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_3", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_4", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_5", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_6", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_1", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_2", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_3", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_4", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_5", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_6", "defense_", colnames(combined_shifts_dummies))

### REMOVE DUPLICATE SKATERS ###

combined_shifts_dummies <- as.data.frame(lapply(split.default(combined_shifts_dummies, names(combined_shifts_dummies)), function(x) Reduce(`+`, x)))

### REMOVE GOALIES ###

combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Goalie"))
combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Missing"))

### CREATE VECTORS TO PREPARE FOR RAPM CALCULATION ###

xGF60 <- as.numeric(c(combined_shifts_dummies$xGF_60))
shift_length <- as.numeric(c(combined_shifts_dummies$shift_length))
subsetted_dummies = subset(combined_shifts_dummies, select = -c(shift_length, xGF_60))
RAPM_xGF <- as.matrix(subsetted_dummies)
RAPM_xGF[!is.finite(RAPM_xGF)] <- 0

### CONVERT MATRIX TO SPARSE - MAKES CROSS VALIDATION TEN TIMES FASTER!!! ###

Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)

Cross_Validated_Results <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

### INSERT LAMBDA OBTAINED FROM CROSS VALIDATION INTO FULL RUN ###

Run_RAPM <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

### OBTAIN COEFFICIENTS FROM THE REGRESSION ###

RAPM_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM)))

### CLEAN DATA AND MAKE IT PRESENTABLE ###

Binded_Coefficients <- cbind(rownames(RAPM_coefficients), RAPM_coefficients) %>%
  rename(Player = `rownames(RAPM_coefficients)`, xGF_60 = s0)

write_excel_csv(Binded_Coefficients, "Vanilla_EV_RAPM_1819_Coefficients.csv")

offense_RAPM <- Binded_Coefficients %>%
  filter(grepl("offense", Binded_Coefficients$Player))

offense_RAPM$Player = str_replace_all(offense_RAPM$Player, "offense__", "")

defense_RAPM <- Binded_Coefficients %>%
  filter(grepl("defense", Binded_Coefficients$Player)) %>%
  rename(xGA_60 = xGF_60)

defense_RAPM$Player = str_replace_all(defense_RAPM$Player, "defense__", "")

joined_RAPM <- inner_join(offense_RAPM, defense_RAPM, by="Player")

joined_RAPM$xGPM_60 <- joined_RAPM$xGF_60 - joined_RAPM$xGA_60

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))

### VIEW THE FINISHED PRODUCT! :) ###

View(joined_RAPM)

sd(joined_RAPM$xGPM_60)

write_excel_csv(joined_RAPM, "Cleaned_EV_Vanilla_RAPM_1819.csv")
