get_random_sample <- function(n, size=20, seed = 123, replace = FALSE) {
  # Set the seed for reproducibility
  set.seed(seed)
  
  # Return a sample of 20 numbers from 1 to n
  return(sample(1:n, size = size, replace = replace))
}
# Example: Processing a list of papers
get_metadata <- function(title=NULL, author=NULL, doi=NULL) {
  tryCatch({
    testQuery <- doi
    res <- rcrossref::cr_works(query = paste(title, author), 
                               limit = 1,
                               mailto = "cairns@tamu.edu")
    return(res$data)
  }, error = function(e) return(NULL))
}

get_metadata_by_doi <- function(doi, uid) {
  
  # Add your mailto and wrap in tryCatch for robustness
  res <- tryCatch(
    rcrossref::cr_works(dois = doi, mailto = "cairns@tamu.edu"),
    error = function(e) NULL
  )
  
  if (is.null(res) || is.null(res$data)) return(NULL)
  
  data <- as_tibble(res$data)
  
  # Check if 'author' column exists; if not, return minimal data or NULL
  if (!"author" %in% names(data)) {
    return(data %>% 
             mutate(uid = uid, 
                    affiliation = NA_character_, 
                    orcid = NA_character_) %>%
             select(any_of(c("title", "doi", "affiliation", "orcid", "uid"))))
  }
  
  expanded_data <- data %>%
    unnest(cols = c(author), names_sep = "_") %>%
    mutate(
      affiliation = if("author_affiliation.name" %in% names(.)) author_affiliation.name else NA_character_,
      orcid = if("author_ORCID" %in% names(.)) author_ORCID else NA_character_,
      uid = uid
    ) %>%
    # Select needed fields
    select(any_of(c("title", "doi", "author_given", "author_family", "affiliation", "orcid", "uid")))
  
  return(expanded_data)
}

get_metadata_for_df_using_crossref <- function(df) {
  # Map over each row, passing both DOI and UID as list elements
  # Using purrr::pmap_df to handle the multi-argument input
  results <- purrr::pmap_df(list(doi = df$DOI, uid = df$UID), function(doi, uid) {
    Sys.sleep(0.5)
    get_metadata_by_doi(doi, uid)
  })
  return(results)
}

find_tamu_authors_and_depts_v2 <- function(author_tibble) {
  # Get all columns containing "affiliation"
  affil_cols <- grep("affiliation", colnames(author_tibble), value = TRUE)
  if (length(affil_cols) == 0) return(NULL)
  
  # Function to extract TAMU affiliation from a single row of affiliation columns
  extract_tamu_dept <- function(row) {
    # Check all columns in this row for "Texas A&M"
    # Find which column contains the match
    matches <- grepl("Texas A&M", as.character(row), ignore.case = TRUE)
    
    if (any(matches)) {
      # Return the content of the first column that matches
      return(as.character(row[which(matches)[1]]))
    }
    return(NA_character_)
  }
  
  # Apply to each author row
  all_depts <- apply(author_tibble[, affil_cols], 1, extract_tamu_dept)
  
  # Create a mask for only authors where TAMU was found
  tamu_mask <- !is.na(all_depts)
  
  if (any(tamu_mask)) {
    return(tibble::tibble(
      name = paste(author_tibble$given[tamu_mask], author_tibble$family[tamu_mask]),
      department = all_depts[tamu_mask]
    ))
  }
  return(NULL)
}

find_tamu_authors <- function(author_tibble) {
  # Identify all columns containing "affiliation"
  affil_cols <- grep("affiliation", colnames(author_tibble), value = TRUE)
  
  if (length(affil_cols) == 0) return(character(0))
  
  # Combine text from all affiliation columns for each row
  affils <- apply(author_tibble[, affil_cols], 1, function(x) paste(x, collapse = " "))
  
  # Check for "Texas A&M" (case insensitive)
  is_tamu <- grepl("Texas A&M", affils, ignore.case = TRUE)
  
  # Extract names for authors that match
  author_names <- paste(author_tibble$given[is_tamu], author_tibble$family[is_tamu])
  return(author_names)
}

check_orcid_for_affiliation <- function(inData){
  # ingests the data from get_metadata_df_from_crossref
  # checks only cases where there is no Affiliation and an Orcid is available
  
  # Define helper to fetch via orcidtr
  fetch_from_orcid <- function(orcid) {
    if (is.na(orcid) || orcid == "") return(NA_character_)
    
    res <- tryCatch(
      orcidtr::orcid_employments(orcid),
      error = function(e) return(NA_character_)
    )
    
    if (!is.null(res) && nrow(res) > 0) {
      # Combine organization and department
      org <- res$organization[1]
      dept <- res$department[1]
      
      # Clean NAs for combining
      dept_str <- if (!is.na(dept)) paste0(" - ", dept) else ""
      return(paste0(org, dept_str))
    }
    return(NA_character_)
  }
  
  # Apply only where affiliation is NA and orcid exists
  inData <- inData %>%
    mutate(
      affiliation = if_else(
        (is.na(affiliation) | affiliation == "") & !is.na(orcid),
        purrr::map_chr(orcid, fetch_from_orcid),
        affiliation
      )
    )
  
  return(inData)
}
collaboration_abpa_kpi <- function(productType="Publications", inYear=2025, 
                                   keepStatus="Completed/Published"){
  #1. Identify a data source for raw publications over a specified time period
  #       Eventually want product type to be publications, awarded grants, grant applications
  #       Over how many years do we want to track collaborations?
  #       Each unique publication is referenced by a unique identifier (UID)
  
  theData <- read.csv("./Data/testPubs2024-2025.csv") |>
    dplyr::filter(Year %in% inYear) |>
    dplyr::filter(Status %in% keepStatus) |>
    dplyr::mutate(numAuthors = stringr::str_count(Authors, ";")+1) |>
    dplyr::filter(numAuthors > 1) |>
    dplyr::mutate(Title = gsub("<.*?>", "", Title)) |>
    dplyr::mutate(cleanTitle = gsub("[^[:alnum:]]", "", tolower(Title)))


  # Filter out any pub for which there is no DOI
  useData <- theData |>
    dplyr::filter(!is.na(DOI))
  # Randomly select some (n) publications for testing
  t.indices <- get_random_sample(nrow(useData), size=200)

  #Retrieve metadata from crossref for publications
  all_metadata <- get_metadata_for_df_using_crossref(useData[t.indices, ])
  expanded_data <- check_orcid_for_affiliation(all_metadata)
  
  tamu_pattern <- "(?i)\\b(?:TAMU|Texas\\s+A\\s*(?:&|&amp;|and)\\s*M(?:\\s+University)?|Texas\\s+Agricultural\\s+(?:&|&amp;|and)\\s+Mechanical|College\\s+Station|tamu\\.edu|77840|77841|77842|77843|77844|77845)\\b"
  expanded_data <- expanded_data %>%
    mutate(TAMU = stringr::str_detect(affiliation, stringr::regex(tamu_pattern, ignore_case = TRUE)))
  
# for any record in all_metadata for which there is an orcid, but no affiliation
# check orcid for the affiliation.
#

#print(all_metadata)
# Use map_df to bind results into one big data frame
#all_metadata <- purrr::map2_df(my_data$Title, my_data$Author, get_metadata)
#all_metadata <- purrr::map2_df(doi=theData$DOI[2:3], get_metadata)
  #2. Determine if authors are TAMU affiliated (limit to TAMU faculty)

# check across all affiliations to see if any include TAMU

# all_metadata <- all_metadata %>%
#   mutate(tamu_authors = purrr::map(author, find_tamu_authors_and_depts_v2))
# 

  dept_pattern <- "(?i)[^,;\n]*?\\b(?:Dept|Dept\\.|Department|Institute|Center|Centre|Division|School|Lab|Laboratory)\\b[^,;\n]*"
  
  expanded_data <- expanded_data %>%
    mutate(
      # Extract matching organizational segments only if TAMU is TRUE
      Department = ifelse(
        TAMU == TRUE,
        str_extract_all(affiliation, dept_pattern) %>% 
          sapply(function(x) {
            # Clean whitespace and remove "College Station" if triggered by "College"
            x <- str_trim(x)
            x <- x[!str_detect(x, "(?i)College Station")]
            if (length(x) > 0) paste(x, collapse = "; ") else NA_character_
          }),
        NA_character_
      )
    ) 
  #3. Identify author groupings for TAMU authors
  
  someData1 <- expanded_data
  
  dept_pattern <- "(?i)[^,;\n]*?\\b(?:Dept|Dept\\.|Department|Institute|Center|Centre|Division|School|Lab|Laboratory)\\b[^,;\n]*"
  
  someData1 <- someData1 %>%
    mutate(
      # Extract matching organizational segments only if TAMU is TRUE
      Department = ifelse(
        TAMU == TRUE,
        stringr::str_extract_all(affiliation, dept_pattern) %>% 
          sapply(function(x) {
            # Clean whitespace and remove "College Station" if triggered by "College"
            x <- stringr::str_trim(x)
            x <- x[!stringr::str_detect(x, "(?i)College Station")]
            if (length(x) > 0) paste(x, collapse = "; ") else NA_character_
          }),
        NA_character_
      )
    )
  
  someData1 <- someData1 %>%
    mutate(
      TAMU_Code = case_when(
        is.na(Department) ~ NA_character_,
        
        # Computer Science & Engineering
        str_detect(Department, regex("Computer Science", ignore_case = TRUE)) ~ "CSCE",
        
        # Electrical & Computer Engineering
        str_detect(Department, regex("Electrical", ignore_case = TRUE)) ~ "ECEN",
        
        # Mechanical Engineering
        str_detect(Department, regex("Mechanical", ignore_case = TRUE)) ~ "MEEN",
        
        # Civil & Environmental Engineering
        str_detect(Department, regex("Civil", ignore_case = TRUE)) ~ "CVEN",
        
        # Chemical Engineering
        str_detect(Department, regex("Chemical", ignore_case = TRUE)) ~ "CHEN",
        
        # Biomedical Engineering
        str_detect(Department, regex("Biomedical", ignore_case = TRUE)) ~ "BMEN",
        
        # Aerospace Engineering
        str_detect(Department, regex("Aerospace", ignore_case = TRUE)) ~ "AERO",
        
        # Industrial & Systems Engineering
        str_detect(Department, regex("Industrial", ignore_case = TRUE)) ~ "ISEN",
        
        # Materials Science
        str_detect(Department, regex("Materials", ignore_case = TRUE)) ~ "MSEN",
        
        # Natural Sciences & Math
        str_detect(Department, regex("Physics|Astronomy", ignore_case = TRUE)) ~ "PHYS",
        str_detect(Department, regex("Chem", ignore_case = TRUE)) ~ "CHEM",
        str_detect(Department, regex("Biol", ignore_case = TRUE)) ~ "BIOL",
        str_detect(Department, regex("Math", ignore_case = TRUE)) ~ "MATH",
        str_detect(Department, regex("Stat", ignore_case = TRUE)) ~ "STAT",
        
        # Liberal Arts & Social Sciences
        str_detect(Department, regex("Psychol|Brain", ignore_case = TRUE)) ~ "PBSI",
        str_detect(Department, regex("Econ", ignore_case = TRUE)) ~ "ECON",
        str_detect(Department, regex("English", ignore_case = TRUE)) ~ "ENGL",
        str_detect(Department, regex("Histor", ignore_case = TRUE)) ~ "HIST",
        str_detect(Department, regex("Polit", ignore_case = TRUE)) ~ "POLS",
        
        # Mays Business School
        str_detect(Department, regex("Account", ignore_case = TRUE)) ~ "ACCT",
        str_detect(Department, regex("Finance", ignore_case = TRUE)) ~ "FINC",
        str_detect(Department, regex("Market", ignore_case = TRUE)) ~ "MKTG",
        str_detect(Department, regex("Manag", ignore_case = TRUE)) ~ "MGMT",
        
        # Institutes & Centers (Non-academic department defaults)
        str_detect(Department, regex("Transportation Institute", ignore_case = TRUE)) ~ "TTII",
        
        # Fallback for unidentified departments
        TRUE ~ "UNKN"
      )
    )
  someData1 <- someData1 %>%
    mutate(
      College = case_when(
        is.na(Department) ~ NA_character_,
        
        # College of Engineering
        str_detect(Department, regex("Computer Science|Electrical|Mechanical|Civil|Chemical Eng|Biomedical|Aerospace|Industrial|Materials Science|Engineering", ignore_case = TRUE)) ~ "College of Engineering",
        
        # College of Arts and Sciences
        str_detect(Department, regex("Physics|Astronomy|Chem|Biol|Math|Stat|Psychol|Brain|Econ|English|Histor|Polit|Sociol|Geoscient|Oceanog|Meteorol", ignore_case = TRUE)) ~ "College of Arts and Sciences",
        
        # Mays Business School
        str_detect(Department, regex("Account|Finance|Market|Manag|Information and Operations|Business", ignore_case = TRUE)) ~ "Mays Business School",
        
        # College of Agriculture and Life Sciences
        str_detect(Department, regex("Agri|Animal Science|Entomol|Horticult|Plant|Soil|Ecology|Forestry|Poultry", ignore_case = TRUE)) ~ "College of Agriculture and Life Sciences",
        
        # School of Architecture
        str_detect(Department, regex("Architect|Landscape|Urban|Construction Science", ignore_case = TRUE)) ~ "School of Architecture",
        
        # School of Public Health
        str_detect(Department, regex("Public Health|Epidemiol|Health Policy", ignore_case = TRUE)) ~ "School of Public Health",
        
        # School of Medicine / Health
        str_detect(Department, regex("Medicine|Cardiology|Pediatrics|Pathology|Internal Medicine|Nursing|Pharmacy", ignore_case = TRUE)) ~ "School of Medicine",
        
        # School of Veterinary Medicine & Biomedical Sciences
        str_detect(Department, regex("Veterinary|Vet", ignore_case = TRUE)) ~ "School of Veterinary Medicine & Biomedical Sciences",
        
        # Bush School of Government & Public Service
        str_detect(Department, regex("Public Service|Bush School|International Affairs", ignore_case = TRUE)) ~ "Bush School of Government & Public Service",
        
        # University Institutes / Interdisciplinary Units
        str_detect(Department, regex("Transportation Institute|Data Science|Institute|Center", ignore_case = TRUE)) ~ "University Institutes & Centers",
        
        # Fallback for unmatched TAMU departments
        TRUE ~ "Other / Unassigned"
      )
    )
  return(someData1)
}


determine_tamu_collaborations <- function(inData, level="Department"){
   # Filter out all non-tamu authors
  outData <- inData %>%
    dplyr::filter(TAMU==TRUE) %>%
    dplyr::filter(!is.na(Department)) %>%
    add_count(uid, name = "uid_count") %>%
    dplyr::filter(uid_count > 1) %>%
    group_by(uid) %>%
    mutate(
      collab = {
        # Extract valid codes for the publication
        codes <- TAMU_Code[!is.na(TAMU_Code) & TAMU_Code != "UNKN"]
        
        if (length(codes) > 0) {
          uniq_codes <- sort(unique(codes))
          
          # If all authors are from the same single department, repeat it twice
          if (length(uniq_codes) == 1) {
            paste(rep(uniq_codes, 2), collapse = ":")
          } else {
            # If multiple distinct departments are involved, list each unique code once
            paste(uniq_codes, collapse = ":")
          }
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup() %>%
    select(all_of(c("uid", "collab"))) %>%
    distinct() %>%
    dplyr::filter(!is.na(collab))

  outData
}
someData <- collaboration_abpa_kpi()

tamuData <- determine_tamu_collaborations(someData)
#someData1 <- check_orcid_for_affiliation(someData)


