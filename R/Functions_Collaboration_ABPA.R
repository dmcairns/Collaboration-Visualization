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

# 1. Define your helper function to take one DOI
# Function to map over a dataframe of DOIs/UIDs
# 1. Define your helper function to take one DOI
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

# Function to map over a dataframe of DOIs/UIDs
get_metadata_for_df <- function(df) {
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
  t.indices <- get_random_sample(nrow(useData), size=20)

  #Retrieve metadata from crossref for publications
  all_metadata <- get_metadata_for_df(useData[t.indices, ])

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

  
  #3. Identify author groupings for TAMU authors
  
  return(all_metadata)
}



someData <- collaboration_abpa_kpi()


