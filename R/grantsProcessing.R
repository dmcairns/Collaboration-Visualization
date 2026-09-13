##############################################################
# Processing grant proposal data               #
##############################################################

# 1. Read proposal data from Maestro (provided by Amny)
# 2. 

###############################################################
# Initialization                                              #
###############################################################
library(dplyr)
keepMembers <- c("TAMU", "TEES", "AL-RSRCH", "TTI", "TAMUG", "TAMHSC")

deptListGrants <- readRDS("./Data/deptList.rds")   |>
  select(all_of(c("DEPT_ABBR", "COLLEGE_CODE")))   |>
  add_row(DEPT_ABBR = "INEN", COLLEGE_CODE = "EN") |>
  add_row(DEPT_ABBR = "ELEN", COLLEGE_CODE = "EN") |>
  add_row(DEPT_ABBR = "GEOL", COLLEGE_CODE = "AT") |>
  add_row(DEPT_ABBR = "CPSC", COLLEGE_CODE = "EN") |>
  add_row(DEPT_ABBR = "METR", COLLEGE_CODE = "AT") |>
  add_row(DEPT_ABBR = "PSYC", COLLEGE_CODE = "AT") |>
  add_row(DEPT_ABBR = "VTAN", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "VLAN", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "VSAM", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "VLAM", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "2010", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "2020", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "2030", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "2060", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "2550", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "8010", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "8030", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "8050", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "8060", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "8061", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "8062", COLLEGE_CODE = "NU") |>
  add_row(DEPT_ABBR = "0400", COLLEGE_CODE = "PH") |>
  add_row(DEPT_ABBR = "0401", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "0402", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "0403", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "0403", COLLEGE_CODE = "VM") |>
  add_row(DEPT_ABBR = "0300", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0301", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0302", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0304", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0305", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0306", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "0307", COLLEGE_CODE = "DN") |>
  add_row(DEPT_ABBR = "MARS", COLLEGE_CODE = "MM") |>
  add_row(DEPT_ABBR = "RPTS", COLLEGE_CODE = "AG") |>
  add_row(DEPT_ABBR = "HLKN", COLLEGE_CODE = "ED") |>
  add_row(DEPT_ABBR = "HORT", COLLEGE_CODE = "AG") |>
  add_row(DEPT_ABBR = "ENTC", COLLEGE_CODE = "EN") |>
  add_row(DEPT_ABBR = "AGEN", COLLEGE_CODE = "AG") |>
  add_row(DEPT_ABBR = "AGED", COLLEGE_CODE = "AG") |>
  add_row(DEPT_ABBR = "3550", COLLEGE_CODE = "CP") |>
  add_row(DEPT_ABBR = "2123", COLLEGE_CODE = "MD") |>
  add_row(DEPT_ABBR = "2040", COLLEGE_CODE = "MD") 
 

###############################################################
# Functions                                                   #
###############################################################


summarize_proposal_collaborations <- function(df, inDepts, college_col = "COLLEGE_ABBR") {
  
  # 1. Join with department lookup table
  df_joined <- df %>%
    left_join(inDepts, by = c("Researcher Department" = "DEPT_ABBR"))
  
  # Helper to filter max N occurrences per group and collapse
  cap_and_collapse <- function(data, group_var, val_var, sep = ":", max_occurrences = 2) {
    data %>%
      filter(!is.na(.data[[val_var]])) %>%
      group_by(`Proposal Number`, .data[[val_var]]) %>%
      filter(row_number() <= max_occurrences) %>%
      group_by(`Proposal Number`) %>%
      summarize(
        aggregated_str = paste(.data[[val_var]], collapse = sep),
        .groups = "drop"
      )
  }
  
  # 2. Process Department Level Collaborations (capped at 2 per dept)
  dept_summary <- cap_and_collapse(df_joined, "Proposal Number", "Researcher Department") %>%
    rename(`Researcher Departments` = aggregated_str)
  
  # 3. Process College Level Collaborations (capped at 2 per college)
  college_summary <- cap_and_collapse(df_joined, "Proposal Number", college_col) %>%
    rename(`Researcher Colleges` = aggregated_str)
  
  # 4. Merge back to get individual proposals with both department & college strings
  df_joined %>%
    select(`Proposal Number`, `Proposal Long Title`) %>%
    distinct(`Proposal Number`, .keep_all = TRUE) %>%
    left_join(dept_summary, by = "Proposal Number") %>%
    left_join(college_summary, by = "Proposal Number")
}
clean_and_sort_colleges <- function(data) {
  data %>%
    mutate(
      `Researcher Colleges` = sapply(`Researcher Colleges`, function(x) {
        if (is.na(x)) return(x)
        
        colleges <- unlist(strsplit(x, split = ":"))
        
        # Deduplicate if 3 or more entries exist
        if (length(colleges) >= 3) {
          colleges <- unique(colleges)
        }
        
        # Sort alphabetically and rejoin
        paste(sort(colleges), collapse = ":")
      })
    )
}

processProposals <- function(inFileName="Proposals_from_Maestro.xls"){
  
  proposalsDataRaw <- readxl::read_excel("./Data/Proposals_from_Maestro.xlsx") |>
    filter(`Primary Member` %in% keepMembers) 
  
  proposalsDataProcessed <- summarize_proposal_collaborations(proposalsDataRaw, deptListGrants, college_col="COLLEGE_CODE")
  
}

###############################################################
# Logic                                                       #
###############################################################

proposals <- processProposals()

proposalsFiltered <- proposals |>
  filter(!is.na(`Researcher Colleges`)) |>
  clean_and_sort_colleges()


