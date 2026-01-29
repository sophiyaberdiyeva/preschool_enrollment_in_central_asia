
retrieve_study_vars <- function(dataset, survey = "MICS", verbose=FALSE){
  # Function to retrieve only necessary variables within the project from any of dataframes per country
  
  # MICS Variable Codes
  mics_vars <- c(
    # Indexing information
    "HH1",       # Cluster number
    "HH2",       # Household number
    "HH7",       # Region
    "LN",       # Line number in child under 5 set
    "round",    # Round of study (only for Uzbekistan)
    "UF1",       # Cluster number
    "UF2",       # Household number
    "UF3",       # Child's line number
    "UF4",       # Mother's/caretaker's line number
    "UF10",      # Consent
    "UF17",      # Result of interview for children under 5
    "HL1",        # Line number in household members set
    # Parent & Child characteristics
    "UB2",        # Age of child (in completed years)
    "HH6",        # Area: Urban(1)/Rural(2)
    "HH48",       # Total number of household members
    "HH51",       # Total number of children under age 5
    "HL13",       # Does child's natural mother live in household (Yes/No)
    "HL17",       # Does child's natural father live in household (Yes/No)
    "ED5",        # Highest level of education attended by caretaker
    "windex5",     # Wealth index quintile
    
    # Household durable goods
    "HC10C",      # Household owns motorcycle or scooter (Yes/No)
    "HC10E",      # Household owns car, truck, or van (Yes/No)
    
    # Learning materials
    "EC1",        # Number of children's books or picture books
    "EC2A",       # Child plays with homemade toys (Yes/No/DK)
    "EC2B",       # Child plays with manufactured toys (Yes/No/DK)
    "EC2C",       # Child plays with household objects or objects found outside (Yes/No/DK)
    
    # Inadequate supervision
    "EC3A",       # Number of days child left alone for more than an hour (past week)
    "EC3B",       # Number of days child left with another child <10 years for more than an hour (past week)
    
    # Parent Values, Beliefs, & Definitions - Child discipline
    "UCD2A",      # Took away privileges/forbade something child liked (Yes/No)
    "UCD2B",      # Explained why child's behavior was wrong (Yes/No)
    "UCD2D",      # Shouted, yelled at, or screamed at child (Yes/No)
    "UCD2E",      # Gave child something else to do (Yes/No)
    "UCD2H",      # Called child dumb, lazy or another name like that (Yes/No)
    
    # Early stimulation and responsive care
    grep("^EC5A", names(dataset), value = TRUE),       # Reading books or looking at picture books with child (Yes/No/DK)
    grep("^EC5B", names(dataset), value = TRUE),      # Telling stories to child (Yes/No/DK)
    grep("^EC5C", names(dataset), value = TRUE),       # Singing songs to or with child, including lullabies (Yes/No/DK)
    grep("^EC5D", names(dataset), value = TRUE),       # Taking child outside the home (Yes/No/DK)
    grep("^EC5E", names(dataset), value = TRUE),       # Playing with child (Yes/No/DK)
    grep("^EC5F", names(dataset), value = TRUE),       # Naming, counting, or drawing things for or with child (Yes/No/DK)
    
    # Social transfers
    "ST1A",       # Monthly social allowance (Yes/No)
    "ST1B",       # One-time grant paid for a birth/maternity benefit (Yes/No)
    "ST1C",       # Monthly allowance for low-income families with children (Yes/No)
    "ST1D",       # Any retirement pension (Yes/No)
    "ST1X",       # Any other external assistance programme (Yes/No)
    
    # ECDI2030 (numeration of variables varies for Uzbekistan and Kyrgyzstan)
    # Health domain
    "EC6",        # Can child walk on uneven surface without falling (Yes/No/DK)
    "EC7",        # Can child jump up with both feet leaving the ground (Yes/No/DK)
    "EC8",        # Can child dress him/herself without help (Yes/No/DK)
    "EC9",        # Can child fasten and unfasten buttons without help (Yes/No/DK)
    
    # ECDI2030 - Learning domain (expressive language)
    "EC10",       # Can child say 10 or more words (Yes/No/DK)
    "EC11",       # Can child speak using sentences of 3+ words (Yes/No/DK)
    "EC12",       # Can child speak using sentences of 5+ words (Yes/No/DK)
    "EC13",       # Can child correctly use "I," "you," "she," or "he" (Yes/No/DK)
    "EC14",       # Can child consistently name familiar objects (Yes/No/DK)
    
    # ECDI2030 - Learning domain (literacy, pre-writing, numeracy)
    "EC15",       # Can child recognise at least 5 letters of the alphabet (Yes/No/DK)
    "EC16",       # Can child write his/her own name (Yes/No/DK)
    "EC17",       # Does child recognise all numbers from 1 to 5 (Yes/No/DK)
    "EC18",       # Can child give correct amount when asked for 3 objects (Yes/No/DK)
    "EC19",       # Can child count 10 objects without mistakes (Yes/No/DK)
    
    # ECDI2030 - Learning domain (executive function)
    "EC20",       # Can child do activity without repeatedly asking for help or giving up (Yes/No/DK)
    
    # ECDI2030 - Psychosocial domain
    "EC21",       # Does child ask about familiar people when they are not there (Yes/No/DK)
    "EC22",       # Does child offer to help someone who seems to need help (Yes/No/DK)
    "EC23",       # Does child get along well with other children (Yes/No/DK)
    "EC24",       # How often does child seem very sad or depressed (Daily/Weekly/Monthly/Few times a year/Never/DK)
    "EC25",       # Compared with same age, how much does child kick, bite, or hit others (Not at all/Less/Same/More/A lot more/DK)
    "EC26", "EC27", "EC28", "EC29", "EC30", "EC31", "EC32", "EC33", "EC34", "EC35", "EC36", "EC37", "EC38", "EC39", "EC40",
    
    # Outcome variables - Childcare arrangement
    "UB6",        # Has child ever attended any early childhood education programme (Yes/No)
    "UB7",        # Did child attend programme since September [year] (Yes/No)
    "UB8",        # Currently attending early childhood education programme
    "UB8A",       # Does child currently attend programme (Yes/No)
    "UB8B"        # Does child currently attend programme (alternative question) (Yes/No)
  )
  
  
  # DHS Variable Codes
  dhs_vars <- c(
    # Indexing information
    "V001",      # Cluster number (PSU) – individual/child recodes
    "V002",      # Household number – individual/child recodes
    "V003",      # Respondent's (mother's) line number in the household schedule. 
    "HV001",     # Cluster number (PSU) – household recode
    "HV002",     # Household number – household recode
    "HVIDX",     # Line number of household member
    "CASEID",    # Woman's individual interview identifier
    "B16",       # Child line number
    
    # Child age
    "B8",        # Age of child in years
    
    # Preschool / early childhood education
    "HV121", #Member attended school during current school year
    
    # Area of residence
    "V024",      # Region of residence
    "HV025",     # Type of place of residence (1 = Urban, 2 = Rural)
    
    # Household size
    "HV009",     # Total number of household members
    
    # Mother's socioeconomic background
    "V106",      # Highest educational level attained by mother
    "V714",      # Woman worked in the last 7 days
    "V731",      # Husband/partner worked in the last 7 days
    
    # Household assets (socioeconomic status)
    "HV270",     # Wealth index combined
    "HV211",     # Household owns a motorcycle or motor scooter
    "HV212",     # Household owns a car or truck
    
    # Child discipline / parental behavior (past month)
    "HCDI3A",     # Took away privileges or forbade something child liked
    "HCDI3B",     # Explained why child's behaviour was wrong
    "HCDI3D",     # Shouted, yelled, or screamed at the child
    "HCDI3E",     # Gave child something else to do
    "HCDI3H"     # Called child dumb, lazy, or similar
  )

  target_vars <- if (survey == "MICS") mics_vars else dhs_vars
  
  # Get column names from dataset
  dataset_cols <- names(dataset)
  matches <- target_vars %in% dataset_cols
  matched_vars <- target_vars[matches]
  actual_col_names <- matched_vars
  
  if (verbose){
  cat("\n=== Variable Selection Report ===\n")
  cat("Survey type:", survey, "\n")
  cat("Total variables in target list:", length(target_vars), "\n")
  cat("Variables found in dataset:", length(actual_col_names), "\n")
  cat("Variables NOT found:", length(target_vars) - length(actual_col_names), "\n\n")
  # Show missing variables
  missing_vars <- target_vars[!matches]
  if (length(missing_vars) > 0) {
    cat("Missing variables:\n")
    cat(paste("-", missing_vars, collapse = "\n"), "\n\n")
  }}
  cleaned_dataset <- dataset %>%
    select(all_of(actual_col_names))
  
  return(cleaned_dataset)
}

filter_target_age <- function(dataset, age_column = "UB2", ages = seq(3,4)) {
  # Filters out children who are not 3 or 4 y.o.
  clean_data <- dataset %>%
    filter(.data[[age_column]] %in% ages)
  return(clean_data)
}

create_indexes <- function(child_set, household_set, house_members_set) {
  # Creates unique index columns in each of the dataframes that are joined later
  
  list(
    child_set = child_set %>%
      mutate(
        line_num_index = paste(HH7, HH1, HH2, UF3, sep="_"), # Region, cluster, household, line number
        house_index = paste(HH7, HH1, HH2, sep="_") # Region, cluster, household
      ),
    
    household_set = household_set %>%
      mutate(
        house_index = paste(HH7, HH1, HH2, sep="_")
      ),
    
    house_members_set = house_members_set %>%
      mutate(
        line_num_index = paste(HH7, HH1, HH2, HL1, sep="_")
      )
  )
}


join_mics_datasets <- function(child_set, household_set, house_members_set){
  # Joins datasets for children, households and house members based on the respective indexes.
  # Output still refers to children 3-4 y.o. per row only 
  joined_set <- child_set %>% 
                    left_join(household_set, 
                              by = "house_index",
                              keep = FALSE) %>% 
                        left_join(house_members_set,
                                  by = "line_num_index",
                                  keep = FALSE)
  
  # Removes duplicating columns
  joined_set <- joined_set %>%
    select(-ends_with(".y"))
  joined_set <- joined_set %>%
    select(-ends_with(".x"))
  
  return(joined_set)
}

join_dhs_dataset <- function(child_set, house_members_set){
  
  child_set <- child_set %>%
    left_join(
      house_members_set %>% select(HV001, HV002, HVIDX, HV121),
      by = c(
        "V001" = "HV001",
        "V002" = "HV002",
        "B16"  = "HVIDX"
      ),
      keep = FALSE
    )
  
  child_set <- child_set %>%
    left_join(
      house_members_set,
      by = c("V001" = "HV001",
             "V002" = "HV002",
             "V003" = "HVIDX"),
      keep = FALSE
    )
  
  # Remove duplicating columns
  child_set <- child_set %>%
    select(-ends_with(".y"))
  
  return(child_set)
}

get_and_preprocess_mics_data <- function(country) {
  data_hh <- haven::read_sav(here::here(paste0(
    "data/raw/", country,"/hh.sav")))
  data_ch <- haven::read_sav(here::here(paste0(
    "data/raw/", country,"/ch.sav")))
  data_hl <- haven::read_sav(here::here(paste0(
    "data/raw/", country,"/hl.sav")))
  
  data_ch <- retrieve_study_vars(data_ch)
  data_ch <- filter_target_age(data_ch)
  
  data_hh <- retrieve_study_vars(data_hh)
  data_hl <- retrieve_study_vars(data_hl)
  
  res <- create_indexes(data_ch, data_hh, data_hl)
  
  data_ch <- res$child_set
  data_hh <- res$household_set
  data_hl <- res$house_members_set
  
  final_joined <- join_mics_datasets(data_ch, data_hh, data_hl)
  
  if(country=="Uzbekistan"){
    final_joined <- final_joined %>% filter(`round`==2)
  }
  
  if ("EC5A" %in% colnames(final_joined)) {
    final_joined <- final_joined %>%
      select(-matches("^EC5.{2,4}$"))
  }

  return(final_joined)
}

preprocess_dhs_data <- function(child_set_path,
                                house_members_path) {
  
  data_kr <- read_sav(here::here(child_set_path))
  data_pr <- read_sav(here::here(house_members_path))
  
  data_kr <- retrieve_study_vars(data_kr, survey = "DHS")
  data_kr <- filter_target_age(data_kr, age_column = "B8")
  
  data_pr <- retrieve_study_vars(data_pr, survey = "DHS")
  
  final_joined <- join_dhs_dataset(data_kr, data_pr)
  
  return(final_joined)
}

# kazakhstan <- get_and_preprocess_mics_data("Kazakhstan")
# kyrgyzstan <- get_and_preprocess_mics_data("Kyrgyzstan")
# turkmenistan <- get_and_preprocess_mics_data("Turkmenistan")
# uzbekistan <- get_and_preprocess_mics_data("Uzbekistan")
# 
# tajikistan <- preprocess_dhs_data(child_set_path = "data/raw/Tajikistan/TJKR81SV_childrens_recode/TJKR81FL.sav",
#                                   house_members_path = "data/raw/Tajikistan/TJPR81SV_household_member_recode/TJPR81FL.sav")

# write.csv(kazakhstan,"data/processed/Kazakhstan_MICS.csv")
# write.csv(kyrgyzstan,"data/processed/Kyrgyzstan_MICS.csv")
# write.csv(turkmenistan,"data/processed/Turkmenistan_MICS.csv")
# write.csv(uzbekistan,"data/processed/Uzbekistan_MICS.csv")
# write.csv(tajikistan,"data/processed/Tajikistan_DHS.csv")