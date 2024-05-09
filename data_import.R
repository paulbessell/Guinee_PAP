cDate <- Sys.Date()
prefecture <- params$Prefecture
outFolder <- paste0(prefecture, "-", cDate)

if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('xlsx')) install.packages('xlsx'); library('xlsx')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('writexl')) install.packages('writexl'); library('writexl')

filePath <- "C:\\Users\\paulb\\Dropbox\\Paul\\FIND\\Guinea\\RCode\\RCode\\Guinea summary\\Gassama_System"
downloadPath <- "c:/Users/paulb/Downloads"
outdir <- paste0(filePath, "\\", "outputs")
outFile <- paste0(filePath, "\\", "outputs", "\\", outFolder)
if(!dir.exists(outFile)) dir.create(outFile)



cFiles <- list.files(downloadPath)
cFiles <- cFiles[grepl("PORTE-A-PORTE", cFiles)]
tDate <- rev(sort(substr(cFiles, nchar(cFiles)-23, nchar(cFiles) -5)))
fName <- paste0(downloadPath, "/", cFiles[grepl(tDate[1], cFiles)])

outFileFile <- paste0(outFile, "\\", outFolder, ".xlsx")

doorToDoor <- readxl::read_excel(fName)

doorToDoor <-  doorToDoor %>%
  filter(reportdate >= dateRange[1],
         reportdate <= dateRange[2]) %>%
  rename("test.Longitude" = "_test_longitude",
         "test.Latitude" = "_test_latitude",
         "KEY" = "_uuid",
         "ID" = "_id")

doorToDoor$Final_Village <- doorToDoor$village_label

doorToDoorRepeat <- read_excel(fName, sheet = "test_results")

doorToDoorRepeatComb <- doorToDoor %>%
  left_join(doorToDoorRepeat, by = c("KEY" = "_submission__uuid")) %>%
  mutate(Case = FALSE)


doorToDoorIndividuals <- doorToDoorRepeatComb %>%
  mutate(inc = TRUE) %>%
  filter(inc) %>%
  dplyr::select(tested_code, 
         test_district, 
         Final_Village,
         age,
         reportdate,
         final_result,
         coris_done,
         sd2_done,
         historical_hat_case,
         signs_present
         ) %>%
  arrange(desc(reportdate)) %>%
  data.frame()

# xlsx::write.xlsx2(doorToDoorIndividuals, outFileFile, showNA = FALSE, append = F, sheetName = "Demographie", row.names = F)


# Exporting data ----------------------------------------------------------

doorToDoorIndividualExport <- doorToDoorRepeatComb %>%
  dplyr::select(team_code,
                household_num, 
                suspect_surname,
                suspect_firstname,
                phone_number,
                tested_code, 
                test_district, 
                Final_Village,
                sex,
                age,
                reportdate,
                final_result,
                coris_done,
                sd2_done,
                sd2_bands,
                historical_hat_case,
                signs_present,
                suspect_surname,
                suspect_firstname,
                phone_number
  ) %>%
  filter(coris_done == "positive" | signs_present == "present") %>%
  data.frame()

# xlsx::write.xlsx2(doorToDoorIndividualExport, outFileFile, showNA = FALSE, append = T, sheetName = "TDR positives", row.names = F)


# Household Level stats ---------------------------------------------------

household <- doorToDoorRepeatComb %>%
  group_by(KEY, 
           team_code,
           Final_Village,
           household_code, 
           reportdate,
           test.Latitude, 
           test.Longitude, 
           household_members) %>%
  summarise("Tested" = n(),
            SD2_Pos = sum(sd2_done == "positive"),
            Coris_Pos = sum(coris_done == "positive", na.rm = T),
            Historical_Case = sum(historical_hat_case == "yes", na.rm = T),
            RDT_Suspect = sum(final_result == "Positif", na.rm = T)) %>%
  mutate(Village_Text = substring(Final_Village, 0, nchar(Final_Village) - 6)) %>%
  ungroup() %>%
  dplyr::select(-KEY) %>%
  data.frame()

# xlsx::write.xlsx2(household, outFileFile, showNA = FALSE, append = T, sheetName = "Menages", row.names = F)



# Team Level stats ---------------------------------------------------

team <- doorToDoorRepeatComb %>%
  group_by(team_code) %>%
  summarise("Tested" = n(),
            SD2_Pos = sum(sd2_done == "positive"),
            Coris_Pos = sum(coris_done == "positive", na.rm = T),
            Historical_Case = sum(historical_hat_case == "yes", na.rm = T),
            RDT_Suspect = sum(final_result == "Positif", na.rm = T)) %>%
  data.frame()

#xlsx::write.xlsx2(team, outFileFile, showNA = FALSE, append = T, sheetName = "Equipe", row.names = F)

# Village Level stats ---------------------------------------------------

village_data <- household %>%
  group_by(Final_Village,
           Village_Text,
           reportdate) %>%
  summarise(Laitude = median(test.Latitude),
            Longitude = median(test.Longitude),
            Population = sum(household_members),
            Tested = sum(Tested),
            SD2_Pos = sum(SD2_Pos, na.rm = T),
            Coris_Pos = sum(Coris_Pos, na.rm = T),
            Historical_Case = sum(Historical_Case, na.rm = T),
            RDT_Suspect = sum(RDT_Suspect, na.rm = T)) %>%
  data.frame()

#xlsx::write.xlsx2(village_data, outFileFile, showNA = FALSE, append = T, sheetName = "Village par jour", row.names = F)


# Village level All -------------------------------------------------------


village_data_agg <- household %>%
  group_by(Final_Village) %>%
  summarise(Laitude = median(test.Latitude),
            Longitude = median(test.Longitude),
            Population = sum(household_members),
            Tested = sum(Tested),
            SD2_Pos = sum(SD2_Pos, na.rm = T),
            Coris_Pos = sum(Coris_Pos, na.rm = T),
            Historical_Case = sum(Historical_Case, na.rm = T),
            RDT_Suspect = sum(RDT_Suspect, na.rm = T)) %>%
  data.frame()

#xlsx::write.xlsx2(village_data_agg, outFileFile, showNA = FALSE, append = T, sheetName = "Village resume", row.names = F)

writexl::write_xlsx(list('Equipe' = team, 'Village resume' = village_data_agg, 'village par jour' = village_data, 'menage' = household, 'Deomgraphie' = doorToDoorIndividualExport), path = outFileFile)


demographics <- doorToDoorRepeatComb %>%
  mutate(age = as.integer(age)) %>%
  mutate(pAgeCut = cut(age, breaks = seq(0, 90, by = 10))) %>%
  mutate(pAgeCut = gsub(",", "-", pAgeCut),
         pAgeCut = gsub("\\(|\\]", "", pAgeCut))



# Parasitology ------------------------------------------------------------

