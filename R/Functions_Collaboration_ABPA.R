get_random_sample <- function(n, size=20, seed = lubridate::now(), replace = FALSE) {
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

remove_html <- function(df, col_name = "title") {
  df[[col_name]] <- gsub("<[^>]+>", "", df[[col_name]])
  return(df)
}
library(tidyverse)
library(rcrossref)

expand_authors_by_doi <- function(df) {
  # 1. Validate required input columns
  required_cols <- c("Faculty.ID", "UID", "Last.Name", "Title", "Journal.Title", "DOI", "status")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Input data frame is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Helper function to fetch and format Crossref author data for a single DOI
  get_crossref_authors <- function(doi) {
    if (is.na(doi) || trimws(doi) == "") {
      return(tibble())
    }
    
    tryCatch({
      # Fetch metadata from Crossref
      res <- rcrossref::cr_works(dois = doi)
      
      # Check if author data exists
      if (!"author" %in% names(res$data) || is.null(res$data$author[[1]])) {
        return(tibble())
      }
      
      authors_df <- res$data$author[[1]]
      
      # Extract affiliations into a concatenated string if present
      if ("affiliation" %in% names(authors_df)) {
        authors_df <- authors_df %>%
          rowwise() %>%
          mutate(
            affiliation = if (is.data.frame(affiliation) && "name" %in% names(affiliation)) {
              paste(affiliation$name, collapse = "; ")
            } else if (is.list(affiliation)) {
              paste(unlist(affiliation), collapse = "; ")
            } else {
              NA_character_
            }
          ) %>%
          ungroup()
      } else {
        authors_df$affiliation <- NA_character_
      }
      
      # Standardize column presence
      if (!"ORCID" %in% names(authors_df)) authors_df$ORCID <- NA_character_
      if (!"given" %in% names(authors_df)) authors_df$given <- NA_character_
      if (!"family" %in% names(authors_df)) authors_df$family <- NA_character_
      if (!"sequence" %in% names(authors_df)) authors_df$sequence <- NA_character_
      
      # Select and rename output author fields
      authors_df %>%
        select(
          author_given = given,
          author_family = family,
          author_sequence = sequence,
          author_orcid = ORCID,
          author_affiliation = affiliation
        )
    }, error = function(e) {
      warning(paste("Failed to fetch DOI:", doi, "-", e$message))
      return(tibble())
    })
  }
  
  # 2. Filter valid rows and fetch Crossref author metadata
  valid_matches <- df %>%
    filter(status == "Valid Match") %>%
    mutate(
      author_data = map(DOI, get_crossref_authors)
    ) %>%
    unnest(author_data, keep_empty = TRUE) # Keeps the record even if Crossref has no author data
  
  # 3. Handle non-matching rows (retain them as single rows with NA author details)
  other_matches <- df %>%
    filter(status != "Valid Match" | is.na(status)) %>%
    mutate(
      author_given = NA_character_,
      author_family = NA_character_,
      author_sequence = NA_character_,
      author_orcid = NA_character_,
      author_affiliation = NA_character_
    )
  
  # 4. Combine results and preserve original row ordering strategy
  bind_rows(valid_matches, other_matches)
}
verify_doi_batch <- function(df) {
  # Input df requires columns: uid, doi, title, author
  
  # Status Indicator 1: Start Banner
  cli_alert_info("Starting DOI verification batch for {nrow(df)} record{?s}...")
  
  # Initialize Progress Bar (Status Indicator 2)
  pb <- cli_progress_bar(
    format = "Verifying DOIs [{pb_current}/{pb_total}] |{pb_bar}| {pb_percent} - ETA: {pb_eta}",
    total = nrow(df)
  )
  
  res <- map_dfr(1:nrow(df), function(i) {
    cli_progress_update(id = pb) # Advance progress bar
    
    # Check for UID column flexibility (uid or UID)
    current_uid <- if ("uid" %in% names(df)) df$uid[i] else df$UID[i]
    doi <- df$doi[i]
    target_title <- df$title[i]
    target_author <- df$author[i]
    
    # Check if DOI is NA or blank before querying Crossref
    if (is.na(doi) || trimws(as.character(doi)) == "") {
      return(tibble(
        uid = current_uid,
        doi = NA_character_,
        exists = FALSE,
        metadata_matches = FALSE,
        found_title = NA_character_,
        status = "NO F180 DOI"
      ))
    }
    
    tryCatch({
      # Fetch work metadata from Crossref
      work <- cr_works(dois = doi)$data
      
      # 1. Fuzzy match title (Jaro-Winkler)
      t_sim <- stringdist::stringsim(
        tolower(target_title), 
        tolower(work$title), 
        method = "jw"
      )
      
      # 2. Match author surname
      authors_combined <- paste(work$author[[1]]$family, collapse = " ")
      a_match <- grepl(tolower(target_author), tolower(authors_combined))
      
      is_valid <- (t_sim > 0.85) && a_match
      
      tibble(
        uid = current_uid,
        doi = doi,
        exists = TRUE,
        metadata_matches = is_valid,
        found_title = work$title,
        status = if(is_valid) "Valid Match" else "Metadata Mismatch"
      )
    }, error = function(e) {
      tibble(
        uid = current_uid,
        doi = doi, 
        exists = FALSE, 
        metadata_matches = FALSE, 
        found_title = NA_character_,
        status = "DOI Not Found / API Error"
      )
    })
  })
  
  # Status Indicator 3: Summary Completion Banner
  valid_count <- sum(res$metadata_matches, na.rm = TRUE)
  cli_alert_success("Verification complete! {valid_count}/{nrow(df)} DOIs verified successfully.")
  
  return(res)
}
expand_authors_by_openalex <- function(df, email = NULL, batch_size = 50, sleep_time = 0.1, progress = TRUE) {
  # 1. Configure OpenAlex Polite Pool if email is provided
  if (!is.null(email) && nzchar(email)) {
    old_mailto <- getOption("openalexR.mailto")
    options(openalexR.mailto = email)
    on.exit(options(openalexR.mailto = old_mailto), add = TRUE)
  }
  
  # 2. Validate required input columns
  required_cols <- c("Faculty.ID", "UID", "Last.Name", "Title", "Journal.Title", "DOI", "status")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Input data frame is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  empty_author_cols <- list(
    author_name = NA_character_,
    author_position = NA_character_,
    author_orcid = NA_character_,
    author_openalex_id = NA_character_,
    author_affiliation = NA_character_
  )
  
  # 3. Separate valid records for batch querying
  valid_df <- df %>% filter(status == "Valid Match", !is.na(DOI), trimws(DOI) != "")
  other_df <- df %>% filter(status != "Valid Match" | is.na(DOI) | trimws(DOI) == "")
  
  if (nrow(valid_df) == 0) {
    return(df %>% mutate(!!!empty_author_cols))
  }
  
  # 4. Batch fetch work metadata from OpenAlex in chunks with `cli` progress bar
  clean_dois <- tolower(gsub("^https?://(dx\\.)?doi\\.org/", "", valid_df$DOI))
  unique_dois <- unique(clean_dois)
  
  doi_batches <- split(unique_dois, ceiling(seq_along(unique_dois) / batch_size))
  total_batches <- length(doi_batches)
  
  # Initialize cli progress bar
  if (progress && interactive()) {
    cli_bar <- cli::cli_progress_bar(
      format = "{cli::pb_spin} Fetching OpenAlex data [{cli::pb_current}/{cli::pb_total}] | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = total_batches,
      clear = FALSE
    )
    on.exit(cli::cli_progress_done(id = cli_bar), add = TRUE)
  } else {
    cli_bar <- NULL
  }
  
  works_data_list <- imap(doi_batches, function(batch_dois, idx) {
    res <- tryCatch({
      openalexR::oa_fetch(
        entity = "works",
        doi = batch_dois,
        verbose = FALSE
      )
    }, error = function(e) {
      if (!is.null(cli_bar)) {
        cli::cli_alert_warning(paste("Failed to fetch batch", idx, "from OpenAlex:", e$message))
      } else {
        warning(paste("Failed to fetch batch", idx, "from OpenAlex:", e$message))
      }
      return(NULL)
    })
    
    # Update progress bar step
    if (!is.null(cli_bar)) cli::cli_progress_update(id = cli_bar)
    
    if (sleep_time > 0) Sys.sleep(sleep_time)
    
    return(res)
  })
  
  works_data <- bind_rows(compact(works_data_list))
  
  # Helper function to parse nested author structures
  parse_authors_df <- function(authors_df) {
    if (is.null(authors_df) || !is.data.frame(authors_df) || nrow(authors_df) == 0) {
      return(as_tibble(empty_author_cols)[0, ])
    }
    
    affil_vec <- if ("affiliation_raw" %in% names(authors_df)) {
      map_chr(authors_df$affiliation_raw, function(x) {
        if (is.null(x) || all(is.na(x))) return(NA_character_)
        paste(na.omit(as.character(unlist(x))), collapse = "; ")
      })
    } else if ("institutions" %in% names(authors_df)) {
      map_chr(authors_df$institutions, function(x) {
        if (is.null(x) || all(is.na(x))) return(NA_character_)
        if (is.data.frame(x) && "display_name" %in% names(x)) {
          paste(na.omit(x$display_name), collapse = "; ")
        } else {
          paste(na.omit(as.character(unlist(x))), collapse = "; ")
        }
      })
    } else {
      rep(NA_character_, nrow(authors_df))
    }
    
    name_vec <- if ("author_display_name" %in% names(authors_df)) authors_df$author_display_name
    else if ("display_name" %in% names(authors_df)) authors_df$display_name
    else NA_character_
    
    pos_vec   <- if ("author_position" %in% names(authors_df)) authors_df$author_position else NA_character_
    orcid_vec <- if ("orcid" %in% names(authors_df)) authors_df$orcid else NA_character_
    id_vec    <- if ("id" %in% names(authors_df)) authors_df$id else NA_character_
    
    tibble(
      author_name        = as.character(name_vec),
      author_position    = as.character(pos_vec),
      author_orcid       = gsub("https://orcid.org/", "", as.character(orcid_vec), fixed = TRUE),
      author_openalex_id = gsub("https://openalex.org/", "", as.character(id_vec), fixed = TRUE),
      author_affiliation = affil_vec
    )
  }
  
  # 5. Join OpenAlex data back to original records
  if (!is.null(works_data) && nrow(works_data) > 0) {
    author_col <- if ("authorships" %in% names(works_data)) "authorships" else "author"
    
    if (author_col %in% names(works_data)) {
      parsed_works <- works_data %>%
        mutate(
          DOI_clean = tolower(gsub("^https?://(dx\\.)?doi\\.org/", "", doi)),
          author_data = map(.data[[author_col]], parse_authors_df)
        ) %>%
        select(DOI_clean, author_data) %>%
        distinct(DOI_clean, .keep_all = TRUE)
      
      valid_matches <- valid_df %>%
        mutate(DOI_clean = tolower(gsub("^https?://(dx\\.)?doi\\.org/", "", DOI))) %>%
        left_join(parsed_works, by = "DOI_clean") %>%
        select(-DOI_clean) %>%
        unnest(author_data, keep_empty = TRUE)
    } else {
      valid_matches <- valid_df %>% mutate(!!!empty_author_cols)
    }
  } else {
    valid_matches <- valid_df %>% mutate(!!!empty_author_cols)
  }
  
  # 6. Combine and return full dataset
  other_matches <- other_df %>% mutate(!!!empty_author_cols)
  
  bind_rows(valid_matches, other_matches)
}

parse_affiliation <- function(affil) {
  if (is.null(affil) || is.na(affil)) {
    return(NA_character_)
  }
  
  # If affiliation is a data frame (common when names/ROR IDs are included)
  if (is.data.frame(affil)) {
    if ("name" %in% names(affil)) {
      vals <- na.omit(affil$name)
      return(if (length(vals) > 0) paste(vals, collapse = "; ") else NA_character_)
    }
  }
  
  # If affiliation is a nested list or character vector
  if (is.list(affil) || is.character(affil)) {
    vals <- unlist(affil)
    vals <- vals[vals != "" & !is.na(vals)]
    return(if (length(vals) > 0) paste(vals, collapse = "; ") else NA_character_)
  }
  
  return(NA_character_)
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
  # Create a safe version of your function that returns NULL on 404 / failure
  safe_get_metadata <- purrr::possibly(
    .f = function(doi, uid) {
      Sys.sleep(0.5)
      get_metadata_by_doi(doi, uid)
    },
    otherwise = NULL,
    quiet = TRUE # Suppresses the error message in output
  )
  
  results <- purrr::pmap_dfr(
    list(doi = df$DOI, uid = df$UID), 
    safe_get_metadata,
    .progress = TRUE
  )
  
  return(results)
}

get_acad_dept_and_colleges <- function(){
  #keepColleges <- c("DN", "BA", "MD", "PH", "PR", "EN", "AG", "AT", "AR", "GB", "ED", "PV", "VM", "NU")
  dbConn <- DBFunctionsTAMU::createDBConnection_abpa("sql-rptdata.as.tamu.edu", "WAREHOUSE")
  theData <- DBI::dbGetQuery(dbConn, "SELECT * FROM CURRENT_DEPARTMENTS")
  DBFunctionsTAMU::closeAllDBConnections_abpa()
  theData
}
clean_affiliation_text <- function(affil_raw) {
  if (is.null(affil_raw) || all(is.na(affil_raw))) return(NA_character_)
  
  # Handle nested lists/data frames returned by rcrossref
  if (is.list(affil_raw) || is.data.frame(affil_raw)) {
    if ("name" %in% names(affil_raw)) {
      affil_raw <- paste(affil_raw$name, collapse = " | ")
    } else {
      affil_raw <- paste(unlist(affil_raw), collapse = " | ")
    }
  }
  
  return(as.character(affil_raw))
}


match_phrase_fuzzy <- function(affil_df, deptList, max_distance = 0.3) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("The 'stringdist' package is required for fuzzy matching.")
  }
  
  no_match <- data.frame(
    COLLEGE_CODE     = NA_character_,
    Department       = NA_character_,
    matched_phrase   = NA_character_,
    similarity_score = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (nrow(affil_df) == 0) return(no_match)
  
  # Ensure inputs are unlisted / standard vectors
  affil_vec <- unlist(affil_df$affiliation)
  tamu_vec  <- unlist(affil_df$TAMU)
  
  # Clean pre-processed department names once outside the loop
  dept_names_clean <- gsub("(?i)\\b(department|dept|division|school|college|of)\\b", "", deptList$DEPT_LONG)
  dept_names_clean <- trimws(gsub("\\s+", " ", dept_names_clean))
  
  results <- lapply(seq_len(nrow(affil_df)), function(i) {
    s       <- affil_vec[i]
    is_tamu <- tamu_vec[i]
    
    # Check if TAMU flag is valid and string is non-empty
    if (is.na(is_tamu) || !is_tamu || is.na(s) || nchar(trimws(as.character(s))) == 0) {
      return(no_match)
    }
    
    # Step A: Split into phrases
    phrases <- unlist(strsplit(as.character(s), "[,|;]"))
    phrases <- trimws(phrases)
    phrases <- phrases[nchar(phrases) > 3]
    
    if (length(phrases) == 0) return(no_match)
    
    # Step B: Clean phrases & drop empty results
    phrases_clean <- gsub("(?i)\\b(department|dept|division|school|college|of)\\b", "", phrases)
    phrases_clean <- trimws(gsub("\\s+", " ", phrases_clean))
    
    valid_idx <- nchar(phrases_clean) > 0
    phrases <- phrases[valid_idx]
    phrases_clean <- phrases_clean[valid_idx]
    
    if (length(phrases_clean) == 0) return(no_match)
    
    # Step C: Distance Matrix
    dist_matrix <- stringdist::stringdistmatrix(
      a = tolower(phrases_clean),
      b = tolower(dept_names_clean),
      method = "jaccard",
      q = 3
    )
    
    # Step D: Safe Min Check
    if (all(is.na(dist_matrix))) return(no_match)
    
    min_dist <- min(dist_matrix, na.rm = TRUE)
    
    if (is.finite(min_dist) && min_dist <= max_distance) {
      best_indices    <- which(dist_matrix == min_dist, arr.ind = TRUE)[1, ]
      best_phrase_idx <- best_indices[1]
      best_dept_idx   <- best_indices[2]
      
      return(data.frame(
        COLLEGE_CODE     = deptList$COLLEGE_CODE[best_dept_idx],
        Department       = deptList$DEPT_ABBR[best_dept_idx],
        matched_phrase   = phrases[best_phrase_idx],
        similarity_score = round(1 - min_dist, 3),
        stringsAsFactors = FALSE
      ))
    }
    
    return(no_match)
  })
  
  do.call(rbind, results)
}
process_crossref_affiliations <- function(crossref_df, deptList, max_dist = 0.3) {
  
  results <- crossref_df %>%
    rowwise() %>%
    mutate(
      # Extract raw text string
      #affil_clean = clean_affiliation_text(affiliation),
      
      
      # Run phrase-based fuzzy matching
      #match_data = list(match_phrase_fuzzy(affil_clean, deptList, max_distance = max_dist))
      match_data = list(match_phrase_fuzzy(affiliation, deptList, max_distance = max_dist))
    ) %>%
    ungroup() %>%
    # Expand matched columns into main data frame
    unnest(match_data, names_repair="universal")
  
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
addField_TAMU <- function(inData){
  
    tamu_pattern <- "(?i)\\b(?:TAMU|Texas\\s+A\\s*(?:&|&amp;|and)\\s*M(?:\\s+University)?|Texas\\s+Agricultural\\s+(?:&|&amp;|and)\\s+Mechanical|College\\s+Station|tamu\\.edu|77840|77841|77842|77843|77844|77845)\\b"
    expanded_data <- inData %>%
      mutate(TAMU = stringr::str_detect(affiliation, stringr::regex(tamu_pattern, ignore_case = TRUE)))
    expanded_data
}
check_orcid_for_affiliation <- function(inData, doEval){
  # ingests the data from get_metadata_df_from_crossref
  # checks only cases where there is no Affiliation and an Orcid is available
  if(!doEval) {return()}
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
filterPubsFromF180 <- function(theData, processedPubs, inYear=2025, keepStatus="Completed/Published"){
  # At this point, theData contains ALL publications 
  theData1 <- theData |>
    dplyr::filter(Year %in% inYear) |>
    dplyr::filter(Status %in% keepStatus) 
  
  theData2 <- theData1 |>
    dplyr::mutate(numAuthors = stringr::str_count(Authors, ";")+1) |>
    dplyr::filter(numAuthors > 1) |>
    dplyr::mutate(Title = gsub("<.*?>", "", Title)) |>
    dplyr::mutate(cleanTitle = gsub("[^[:alnum:]]", "", tolower(Title)))
  
  return(list(totalNumPubs=nrow(theData2),
              singleAuthorPubs = nrow(theData1)-nrow(theData2),
              processedPubs = length(unique(processedPubs$uid))))
}
collaboration_abpa_kpi <- function(inDataFile = NULL, previouslyDoneBatch = NULL, productType="Publications", inYear=2025, 
                                   keepStatus="Completed/Published", numPubs=500){
  #1. Identify a data source for raw publications over a specified time period
  #       Eventually want product type to be publications, awarded grants, grant applications
  #       Over how many years do we want to track collaborations?
  #       Each unique publication is referenced by a unique identifier (UID)
  if(is.null(inDataFile)) {
    theData <- read.csv("../Data/testPubs2024-2025.csv")
  } else {
    theData <- readRDS(inDataFile)
  }
  
  # At this point, theData contains ALL publications 
  theData <- theData |>
    dplyr::filter(Year %in% inYear) |>
    dplyr::filter(Status %in% keepStatus) |>
    dplyr::mutate(numAuthors = stringr::str_count(Authors, ";")+1) |>
    dplyr::filter(numAuthors > 1) |>
    dplyr::mutate(Title = gsub("<.*?>", "", Title)) |>
    dplyr::mutate(cleanTitle = gsub("[^[:alnum:]]", "", tolower(Title)))
#browser()

  # Filter out any pub for which there is no DOI
  useData <- theData |>
    dplyr::filter(!is.na(DOI))
  
  
  # previouslyDoneBatch is the data after processed by this function previoiusly
  #
  
  availableData <- anti_join(useData, previouslyDoneBatch, by = c("UID"="uid"))
 
  # 2. Draw sample using your function
  sampledData <- availableData[get_random_sample(n = nrow(availableData), size = numPubs), ]
  
  #Retrieve metadata from crossref for publications
  
  all_metadata <- get_metadata_for_df_using_crossref(sampledData)
  
  #Bind all_metadata to previously processed data
  all_metadata <- rbind(all_metadata, previouslyDoneBatch)
  

  return(all_metadata)
}


link_colleges_to_collaborations <- function(inData){
  
  # !!!!!!!!!! Alphabetize college_collabs before grouping (e.g. AT:AG should be AG:AT)
  outData <- inData |>
    separate_wider_delim(college_collab, delim = ":", cols_remove=FALSE,
                         names_sep=".", too_few="debug") |>
    rename("college_collab" = "college_collab.college_collab" ) |>
    select(-starts_with("college_collab.college_collab")) |>
    #arrange("uid", "collab", "college_collab", all_of(c(starts_with("college_collab."))))
    pivot_longer(names_to="college", cols=starts_with("college_collab.")) |>
    filter(!is.na(value)) |>
    select(-"college") |>
    rename("college" = "value") |>
    distinct() |>
    summarize(n=n(), .by=c("college", "college_collab")) |>
    arrange(college)
    
  
  return(outData)
}

make_edges <- function(inData){
  outData <- inData |>
    # make from TAMU to Colleges
    separate_wider_delim(college_collab, delim = ":", cols_remove=FALSE,
                         names_sep=".", too_few="debug") |>
    rename("college_collab" = "college_collab.college_collab" ) |>
    select(-starts_with("college_collab.college_collab")) |>
    #arrange("uid", "collab", "college_collab", all_of(c(starts_with("college_collab."))))
    pivot_longer(names_to="college", cols=starts_with("college_collab.")) |>
    filter(!is.na(value)) |>
    select(-c("college", "uid", "collab")) |>
    rename("college" = "value") |>
    distinct() |>
    mutate(from="TAMU") |>
    select(-"college_collab") |>
    rename("to"="college") |>
    data.frame() |>
    distinct() |>
    select(any_of(c("from", "to")))
  
  nodes <- link_colleges_to_collaborations(inData) |>
    rename("from"="college", "to"="college_collab") |>
    select(any_of(c("from", "to")))
  
  outData <- outData |>
    rbind(nodes)
    
  
  return(outData)
}


determine_tamu_collaborations <- function(inData){
  #browser()
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
        codes <- Department[!is.na(Department) & Department != "UNKN"]

        if (length(codes) > 0) {
          uniq_codes <- sort(unique(codes))
          #uniq_college_codes <- sort(unique(college_codes))
          #
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
      },
      college_collab = {
        # Extract valid codes for the publication
        college_codes <- COLLEGE_CODE[!is.na(COLLEGE_CODE) & COLLEGE_CODE != "UNKN"]
        
        if (length(college_codes) > 0) {
          uniq_college_codes <- sort(unique(college_codes))
          
          # If all authors are from the same single department, repeat it twice
          if (length(uniq_college_codes) == 1) {
            paste(rep(uniq_college_codes, 2), collapse = ":")
          } else {
            # If multiple distinct departments are involved, list each unique code once
            paste(uniq_college_codes, collapse = ":")
          }
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup() %>%
    select(all_of(c("uid", "collab", "college_collab"))) %>%
    distinct() %>%
    dplyr::filter(!is.na(collab))

  outData
}


############################################################################
# Preparation of data frame before Department and College Determination.   #
############################################################################

makeSkinnyDataFrame <- function(inData){
  # reduce the fields in the raw publications data set to be manageable
  keepFields <- c("Faculty ID", "Last Name", "UID", "Title", "Journal Title", "DOI", "Year...22")
  outData <- inData |>
    select(all_of(keepFields)) |>
    mutate(doi_source = case_when(!is.na(DOI) ~ "F180",
                                  TRUE ~ NA))
           
  outData
}
findNoDOI_Pubs <- function(inData){
  # returns only pubs with no DOI data
  outData <- inData %>%
    filter(is.na(doi_source))
  outData
}


############################################################################
# Code for retrieving DOI from CROSSREF and/or OpenAlex                    #
############################################################################

# ==============================================================================
# 1. GLOBAL CONFIGURATION & BASE REQUESTS
# ==============================================================================

CACHE_DIR <- ".doi_cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

USER_EMAIL <- "cairns@tamu.edu"

# Pre-configured base requests with rate-limiting (10 req/sec)
crossref_base <- request("https://api.crossref.org") |>
  req_user_agent(paste0("DOIBatchLookup/1.0 (mailto:", USER_EMAIL, ")")) |>
  req_throttle(rate = 10 / 1)

openalex_base <- request("https://api.openalex.org") |>
  req_user_agent(paste0("DOIBatchLookup/1.0 (mailto:", USER_EMAIL, ")")) |>
  req_throttle(rate = 10 / 1)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

#' Create a unique MD5 hash string from input metadata
get_cache_key <- function(title, author = "", journal = "") {
  raw_string <- paste(
    tolower(trimws(title %||% "")),
    tolower(trimws(author %||% "")),
    tolower(trimws(journal %||% ""))
  )
  digest::digest(raw_string, algo = "md5")
}

#' Compute title similarity distance using Jaccard q-grams (k=3)
compute_title_distance <- function(target_title, candidate_title) {
  if (is.null(candidate_title) || is.na(candidate_title)) return(1.0)
  
  clean_a <- gsub("[^a-z0-9 ]", "", tolower(target_title))
  clean_b <- gsub("[^a-z0-9 ]", "", tolower(candidate_title))
  
  if (nchar(clean_a) == 0 || nchar(clean_b) == 0) return(1.0)
  
  stringdist(clean_a, clean_b, method = "jaccard", q = 3)
}

#' Single item DOI lookup with local disk caching
get_single_doi_cached <- function(title, author = NA, journal = NA, max_dist = 0.40) {
  
  # Step A: Check Local Cache
  cache_key <- get_cache_key(title, author, journal)
  cache_path <- file.path(CACHE_DIR, paste0(cache_key, ".rds"))
  
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  
  # Clean input values
  author_clean  <- if (is.na(author) || trimws(author) == "") NULL else trimws(author)
  journal_clean <- if (is.na(journal) || trimws(journal) == "") NULL else trimws(journal)
  
  result <- NULL
  
  # --- Step B: Primary Lookup (Crossref) ---
  params_cr <- list(
    `query.title`           = title,
    `query.author`          = author_clean,
    `query.container-title` = journal_clean,
    rows                    = 1,
    select                  = "DOI,title"
  )
  params_cr <- params_cr[!sapply(params_cr, is.null)]
  
  req_cr <- crossref_base |>
    req_url_path_append("works") |>
    req_url_query(!!!params_cr) |>
    req_timeout(5)
  
  resp_cr <- tryCatch({ req_perform(req_cr) }, error = function(e) NULL)
  
  if (!is.null(resp_cr) && resp_status(resp_cr) == 200) {
    res <- resp_body_json(resp_cr)
    items <- res$message$items
    if (length(items) > 0 && !is.null(items[[1]]$DOI)) {
      cand_title <- items[[1]]$title[[1]]
      dist_score <- compute_title_distance(title, cand_title)
      
      if (dist_score <= max_dist) {
        result <- tibble(
          doi = items[[1]]$DOI, 
          source = "Crossref", 
          match_dist = round(dist_score, 3)
        )
      }
    }
  }
  
  # --- Step C: Fallback Lookup (OpenAlex) ---
  if (is.null(result)) {
    search_terms <- paste(c(title, author_clean, journal_clean), collapse = " ")
    
    req_oa <- openalex_base |>
      req_url_path_append("works") |>
      req_url_query(search = search_terms, per_page = 1, select = "doi,title") |>
      req_timeout(5)
    
    resp_oa <- tryCatch({ req_perform(req_oa) }, error = function(e) NULL)
    
    if (!is.null(resp_oa) && resp_status(resp_oa) == 200) {
      res <- resp_body_json(resp_oa)
      results <- res$results
      if (length(results) > 0 && !is.null(results[[1]]$doi)) {
        cand_title <- results[[1]]$title
        dist_score <- compute_title_distance(title, cand_title)
        
        if (dist_score <= max_dist) {
          clean_doi <- gsub("^https?://doi\\.org/", "", results[[1]]$doi)
          result <- tibble(
            doi = clean_doi, 
            source = "OpenAlex", 
            match_dist = round(dist_score, 3)
          )
        } else {
          result <- tibble(
            doi = NA_character_, 
            source = "Low Match Quality", 
            match_dist = round(dist_score, 3)
          )
        }
      }
    }
  }
  
  # --- Step D: Default if no match found ---
  if (is.null(result)) {
    result <- tibble(
      doi = NA_character_, 
      source = "Not Found", 
      match_dist = NA_real_
    )
  }
  
  # Step E: Save to disk cache
  saveRDS(result, cache_path)
  
  return(result)
}

# ==============================================================================
# 3. MAIN BATCH FUNCTION & UTILITIES
# ==============================================================================

#' Batch Process a Data Frame of Publications
#'
#' @param df Data frame containing 'Title', and optionally 'Last Name' and 'Journal Title'
#' @param max_dist Maximum acceptable Jaccard distance score (0.0 = perfect, 1.0 = distinct)
#' @return Data frame augmented with `found_doi`, `doi_source`, and `title_dist`
batch_get_dois <- function(df, max_dist = 0.40) {
  
  if (!"Title" %in% names(df)) stop("Input data frame must have a 'Title' column.")
  if (!"Last Name" %in% names(df)) df$`Last Name` <- NA_character_
  if (!"Journal Title" %in% names(df)) df$`Journal Title` <- NA_character_
  
  results <- pmap_dfr(
    list(df$Title, df$`Last Name`, df$`Journal Title`),
    function(t, a, j) {
      get_single_doi_cached(title = t, author = a, journal = j, max_dist = max_dist)
    },
    .progress = TRUE
  )
  
  df |>
    mutate(
      found_doi = results$doi,
      doi_source = results$source,
      title_dist = results$match_dist
    )
}

#' Utility: Clear local cache folder
clear_doi_cache <- function() {
  files <- list.files(CACHE_DIR, full.names = TRUE)
  file.remove(files)
  message("Cleared ", length(files), " cached entries.")
}
library(httr2)
library(dplyr)
library(purrr)
library(digest)
library(stringdist)

# Base request for PubMed (Entrez API)
pubmed_base <- request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils") |>
  req_user_agent(paste0("DOIBatchLookup/1.0 (mailto:", USER_EMAIL, ")")) |>
  req_throttle(rate = 3 / 1)

#' Single item DOI lookup with local disk caching and service selection
get_single_doi_cached <- function(title, author = NA, journal = NA, max_dist = 0.40, doi_source = "all") {
  
  # Normalize source parameter (case-insensitive)
  doi_source <- tolower(doi_source)
  if (doi_source == "crossref") doi_source <- "crossref"
  if (doi_source %in% c("openalex", "open_alex")) doi_source <- "openalex"
  if (doi_source %in% c("pubmed", "pub_med")) doi_source <- "pubmed"
  
  # Step A: Check Local Cache (include source in cache key to avoid collisions)
  raw_string <- paste(
    doi_source,
    tolower(trimws(title %||% "")),
    tolower(trimws(author %||% "")),
    tolower(trimws(journal %||% ""))
  )
  cache_key <- digest::digest(raw_string, algo = "md5")
  cache_path <- file.path(CACHE_DIR, paste0(cache_key, ".rds"))
  
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  
  # Clean input values
  author_clean  <- if (is.na(author) || trimws(author) == "") NULL else trimws(author)
  journal_clean <- if (is.na(journal) || trimws(journal) == "") NULL else trimws(journal)
  
  result <- NULL
  
  # Helper: Try CrossRef lookup
  try_crossref <- function() {
    params_cr <- list(
      `query.title`           = title,
      `query.author`          = author_clean,
      `query.container-title` = journal_clean,
      rows                    = 1,
      select                  = "DOI,title"
    )
    params_cr <- params_cr[!sapply(params_cr, is.null)]
    
    req_cr <- crossref_base |>
      req_url_path_append("works") |>
      req_url_query(!!!params_cr) |>
      req_timeout(5)
    
    resp_cr <- tryCatch({ req_perform(req_cr) }, error = function(e) NULL)
    
    if (!is.null(resp_cr) && resp_status(resp_cr) == 200) {
      res <- resp_body_json(resp_cr)
      items <- res$message$items
      if (length(items) > 0 && !is.null(items[[1]]$DOI)) {
        cand_title <- items[[1]]$title[[1]]
        dist_score <- compute_title_distance(title, cand_title)
        
        if (dist_score <= max_dist) {
          return(tibble(
            doi = items[[1]]$DOI, 
            source = "Crossref", 
            match_dist = round(dist_score, 3)
          ))
        }
      }
    }
    return(NULL)
  }
  
  # Helper: Try OpenAlex lookup
  try_openalex <- function() {
    search_terms <- paste(c(title, author_clean, journal_clean), collapse = " ")
    
    req_oa <- openalex_base |>
      req_url_path_append("works") |>
      req_url_query(search = search_terms, per_page = 1, select = "doi,title") |>
      req_timeout(5)
    
    resp_oa <- tryCatch({ req_perform(req_oa) }, error = function(e) NULL)
    
    if (!is.null(resp_oa) && resp_status(resp_oa) == 200) {
      res <- resp_body_json(resp_oa)
      results <- res$results
      if (length(results) > 0 && !is.null(results[[1]]$doi)) {
        cand_title <- results[[1]]$title
        dist_score <- compute_title_distance(title, cand_title)
        
        if (dist_score <= max_dist) {
          clean_doi <- gsub("^https?://doi\\.org/", "", results[[1]]$doi)
          return(tibble(
            doi = clean_doi, 
            source = "OpenAlex", 
            match_dist = round(dist_score, 3)
          ))
        }
      }
    }
    return(NULL)
  }
  
  # Helper: Try PubMed lookup (via Entrez E-utilities)
  try_pubmed <- function() {
    term_str <- paste0('"', title, '"[Title]')
    if (!is.null(author_clean)) term_str <- paste0(term_str, ' AND ', author_clean, '[Author]')
    
    req_pm_search <- pubmed_base |>
      req_url_path_append("esearch.fcgi") |>
      req_url_query(
        db = "pubmed",
        term = term_str,
        retmode = "json",
        retmax = 1
      ) |>
      req_timeout(5)
    
    resp_pm <- tryCatch({ req_perform(req_pm_search) }, error = function(e) NULL)
    
    if (!is.null(resp_pm) && resp_status(resp_pm) == 200) {
      res <- resp_body_json(resp_pm)
      id_list <- res$esearchresult$idlist
      
      if (length(id_list) > 0) {
        pmid <- id_list[[1]]
        
        # Fetch summary details to retrieve the DOI
        req_pm_summary <- pubmed_base |>
          req_url_path_append("esummary.fcgi") |>
          req_url_query(
            db = "pubmed",
            id = pmid,
            retmode = "json"
          ) |>
          req_timeout(5)
        
        resp_sum <- tryCatch({ req_perform(req_pm_summary) }, error = function(e) NULL)
        if (!is.null(resp_sum) && resp_status(resp_sum) == 200) {
          sum_res <- resp_body_json(resp_sum)
          doc <- sum_res$result[[pmid]]
          cand_title <- doc$title
          dist_score <- compute_title_distance(title, cand_title)
          
          # Extract DOI from articleids
          doi_val <- NA_character_
          if (!is.null(doc$articleids)) {
            for (aid in doc$articleids) {
              if (aid$idtype == "doi") {
                doi_val <- aid$value
                break
              }
            }
          }
          
          if (!is.na(doi_val) && dist_score <= max_dist) {
            return(tibble(
              doi = doi_val, 
              source = "PubMed", 
              match_dist = round(dist_score, 3)
            ))
          }
        }
      }
    }
    return(NULL)
  }
  
  # --- Step B: Execute requested lookup strategy ---
  if (doi_source == "crossref") {
    result <- try_crossref()
  } else if (doi_source == "openalex") {
    result <- try_openalex()
  } else if (doi_source == "pubmed") {
    result <- try_pubmed()
  } else {
    # Default fallback chain ("all"): Crossref -> OpenAlex -> PubMed
    result <- try_crossref()
    if (is.null(result)) result <- try_openalex()
    if (is.null(result)) result <- try_pubmed()
  }
  
  # --- Step C: Default if no match found ---
  if (is.null(result)) {
    result <- tibble(
      doi = NA_character_, 
      source = "Not Found", 
      match_dist = NA_real_
    )
  }
  
  # Step D: Save to disk cache
  saveRDS(result, cache_path)
  
  return(result)
}

#' Batch Process a Data Frame of Publications
#'
#' @param df Data frame containing 'Title', and optionally 'Last Name' and 'Journal Title'[cite: 1]
#' @param max_dist Maximum acceptable Jaccard distance score (0.0 = perfect, 1.0 = distinct)[cite: 1]
#' @param doi_source Preferred service to search: "crossRef", "openAlex", "pubMed", or "all"
#' @return Data frame augmented with `found_doi`, `doi_source`, and `title_dist`[cite: 1]
batch_get_dois <- function(df, max_dist = 0.40, doi_source = "all") {
  
  if (!"Title" %in% names(df)) stop("Input data frame must have a 'Title' column.")[cite: 1]
  if (!"Last Name" %in% names(df)) df$`Last Name` <- NA_character_[cite: 1]
  if (!"Journal Title" %in% names(df)) df$`Journal Title` <- NA_character_[cite: 1]
  
  results <- pmap_dfr(
    list(df$Title, df$`Last Name`, df$`Journal Title`),
    function(t, a, j) {
      get_single_doi_cached(
        title = t, 
        author = a, 
        journal = j, 
        max_dist = max_dist, 
        doi_source = doi_source
      )
    },
    .progress = TRUE
  )
  
  df |>
    mutate(
      found_doi = results$doi,
      doi_source = results$source,
      title_dist = results$match_dist
    )
}

add_new_doi_values <- function(masterList, newList){
  resolved <- newList |>
    mutate(DOI=case_when(!is.na(found_doi) ~ found_doi,
                         TRUE ~ DOI)) |>
    filter(!is.na(DOI)) |>
    select(-any_of(c("found_doi", "title_dist", "doi_s2", "matched_title"))) 
  
  outData <- masterList |>
    rbind(resolved)
}
get_s2_doi <- function(title, exact = TRUE, api_key = NULL) {
  
  # Select endpoint based on search strategy
  endpoint <- if (exact) {
    "https://api.semanticscholar.org/graph/v1/paper/search/match"
  } else {
    "https://api.semanticscholar.org/graph/v1/paper/search"
  }
  
  # Prepare the request
  req <- request(endpoint) %>%
    req_url_query(
      query = title,
      fields = "title,externalIds,paperId",
      limit = 1
    ) %>%
    req_headers(`User-Agent` = "R-DOI-Lookup/1.0")
  
  # Attach API Key header if provided
  if (!is.null(api_key)) {
    req <- req %>% req_headers(`x-api-key` = api_key)
  }
  
  # Execute the request safely
  resp <- tryCatch({
    req_perform(req)
  }, error = function(e) {
    message("HTTP Request failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(resp)) return(NULL)
  
  body <- resp_body_json(resp)
  
  # Parse match vs general search responses
  paper <- if (exact) {
    body$data[[1]]
  } else {
    body$data[[1]]
  }
  
  if (is.null(paper)) {
    message("No paper matching query was found on Semantic Scholar.")
    return(NULL)
  }
  
  doi <- paper$externalIds$DOI
  
  if (is.null(doi)) {
    message("Paper found, but no DOI is assigned in Semantic Scholar metadata.")
  }
  
  return(list(
    doi = doi,
    title = paper$title,
    paperId = paper$paperId,
    externalIds = paper$externalIds
  ))
}
#' Add Semantic Scholar DOIs to a Data Frame
#'
#' @param df Data frame containing a column named 'Title'.
#' @param exact Logical; passed to `get_s2_doi`.
#' @param api_key Optional character string for Semantic Scholar API key.
#' @param sleep_sec Delay in seconds between API calls to respect rate limits (default: 1 sec without key, 0.1 sec with key).
#' @return The original data frame augmented with `doi_s2` and `matched_title` columns.
add_s2_dois <- function(df, exact = TRUE, api_key = NULL, sleep_sec = if (is.null(api_key)) 1 else 0.1) {
  
  if (!"Title" %in% names(df)) {
    stop("Input data frame must contain a column named 'Title'.")
  }
  
  # Map over each title sequentially
  results <- map(df$Title, function(t) {
    
    # Skip execution for missing or blank titles
    if (is.na(t) || trimws(t) == "") {
      return(list(doi = NA_character_, matched_title = NA_character_))
    }
    
    # Pause to prevent rate limiting
    Sys.sleep(sleep_sec)
    
    res <- get_s2_doi(title = t, exact = exact, api_key = api_key)
    
    list(
      doi_s2 = if (!is.null(res$doi)) res$doi else NA_character_,
      matched_title = if (!is.null(res$title)) res$title else NA_character_
    )
  })
  
  # Bind results back into the data frame
  df_results <- bind_rows(results)
  
  bind_cols(df, df_results)
}

#' Safe Bulk DOI Extractor with Progress Bar & Checkpointing
#'
#' @param df Data frame containing a 'Title' column.
#' @param exact Passed to get_s2_doi.
#' @param api_key Semantic Scholar API key.
#' @param checkpoint_file File path (.rds) to save progress periodically.
#' @param batch_size Number of items to process before saving progress (default 10).
#' @param sleep_sec Delay in seconds between calls.
add_s2_dois_safe <- function(df, 
                             exact = TRUE, 
                             api_key = NULL, 
                             checkpoint_file = "doi_progress_checkpoint.rds",
                             batch_size = 10,
                             sleep_sec = if (is.null(api_key)) 1 else 0.1) {
  
  if (!"Title" %in% names(df)) {
    cli_abort("Input data frame must contain a column named {.val Title}.")
  }
  
  # 1. Check for existing checkpoint to resume
  if (file.exists(checkpoint_file)) {
    cli_alert_info("Found existing checkpoint file {.file {checkpoint_file}}. Resuming...")
    results_list <- readRDS(checkpoint_file)
  } else {
    cli_alert_info("Starting fresh run on {.val {nrow(df)}} items.")
    results_list <- vector("list", nrow(df))
  }
  
  # 2. Setup progress bar hook
  p <- progressor(steps = nrow(df))
  
  # Determine indices that still need processing
  unprocessed_idx <- which(map_lgl(results_list, is.null))
  
  if (length(unprocessed_idx) == 0) {
    cli_alert_success("All records have already been processed!")
  } else {
    # Advance progress bar for already-completed items
    already_done <- nrow(df) - length(unprocessed_idx)
    if (already_done > 0) p(amount = already_done)
  }
  
  # 3. Process loop
  for (i in unprocessed_idx) {
    t <- df$Title[i]
    
    # Handle NA/empty titles
    if (is.na(t) || trimws(t) == "") {
      results_list[[i]] <- tibble(doi_s2 = NA_character_, matched_title = NA_character_)
    } else {
      # Retries for spotty network connections
      res <- tryCatch({
        Sys.sleep(sleep_sec)
        get_s2_doi(title = t, exact = exact, api_key = api_key)
      }, error = function(e) {
        cli_alert_warning("Network error on row {i}: {e$message}. Retrying in 3 seconds...")
        Sys.sleep(3)
        tryCatch(get_s2_doi(title = t, exact = exact, api_key = api_key), error = function(e2) NULL)
      })
      
      results_list[[i]] <- tibble(
        doi_s2 = if (!is.null(res$doi)) res$doi else NA_character_,
        matched_title = if (!is.null(res$title)) res$title else NA_character_
      )
    }
    
    # Update progress bar
    p(sprintf("Row %d/%d", i, nrow(df)))
    
    # Save checkpoint every N batch steps or at the end
    if (i %% batch_size == 0 || i == nrow(df)) {
      saveRDS(results_list, file = checkpoint_file)
    }
  }
  
  cli_alert_success("Finished processing all titles!")
  
  # Bind rows and merge back with original dataframe
  out_results <- bind_rows(results_list)
  out_df <- bind_cols(df, out_results)
  
  # Optional: Clean up checkpoint file after success
  if (file.exists(checkpoint_file)) file.remove(checkpoint_file)
  
  return(out_df)
}


library(httr2)
library(jsonlite)
library(urltools)

#' Retrieve DOIs from Scholars@TAMU by Publication Title
#'
#' @param title_query Character string of the article title or search keywords.
#' @param limit Maximum number of matching results to check (default: 5).
#' @return A data frame containing matching titles and their corresponding DOIs.
get_tamu_doi <- function(title_query, limit = 5) {
  
  # Base API URL for Scholars Discovery Search endpoint
  base_url <- "https://api.library.tamu.edu/scholars-discovery/documents/search"
  
  # Build and execute HTTP GET request
  req <- request(base_url) %>%
    req_url_query(
      q = title_query,
      size = limit
    ) %>%
    req_headers(`Accept` = "application/json")
  
  resp <- req_perform(req)
  
  # Parse JSON response
  content <- resp_body_string(resp) %>%
    fromJSON(flatten = TRUE)
  
  # Extract document items safely
  if ("content" %in% names(content) && length(content$content) > 0) {
    results <- content$content
    
    # Ensure doi column exists (returns NA if missing in API payload)
    if (!"doi" %in% names(results)) {
      results$doi <- NA_character_
    }
    
    # Return clean data frame
    out <- results[, c("title", "doi")]
    return(out)
  } else {
    message("No matches found for title query: ", title_query)
    return(data.frame(title = character(0), doi = character(0)))
  }
}
################################################################################
# Functions                                                                    #
################################################################################

make_status_card_html <- function(inData, displayCountsForField = "Status") {
  
  inData <- as.data.frame(inData)
  statuses <- inData[[displayCountsForField]]
  counts   <- inData[["n"]]
  
  cards_html <- purrr::map2_chr(statuses, counts, function(status_name, count) {
    # Notice HTML tags start with NO leading indentation inside glue
    glue::glue('<div style="flex: 1; min-width: 180px; background-color: #f8f9fa; border: 2px solid #e9ecef; border-left: 6px solid #500000; border-radius: 8px; padding: 20px 24px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);">'
               , '<div style="font-size: 0.9rem; font-weight: 600; color: #495057; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.5px;">{status_name}</div>'
               , '<div style="font-size: 2.5rem; font-weight: 800; color: #500000; line-height: 1;">{format(count, big.mark = ",")}</div>'
               , '</div>')
  })
  
  # Ensure wrapper tag starts at column 0 without leading spaces
  wrapper_html <- sprintf('<div style="display: flex; gap: 16px; flex-wrap: wrap; margin: 15px 0; font-family: sans-serif;">\n%s\n</div>', 
                          paste(cards_html, collapse = "\n"))
  
  return(wrapper_html)
}

verify_dois_openalex <- function(df, 
                                 title_col = "title", 
                                 author_col = "author", 
                                 doi_col = "doi",
                                 email = "cairns@tamu.edu", 
                                 batch_size = 50,
                                 similarity_threshold = 0.85) {
  
  # Ensure column names exist
  required_cols <- c(title_col, author_col, doi_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in input data frame:", paste(missing_cols, collapse = ", ")))
  }
  
  # Clean DOIs to standard format (10.xxxx/xxxx)
  clean_doi_fn <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
    x <- trimws(x)
    return(x)
  }
  
  df_prep <- df %>%
    mutate(
      .row_id = row_number(),
      .clean_doi = clean_doi_fn(.data[[doi_col]])
    )
  
  # Separate records that have DOIs from those that don't
  valid_doi_df <- df_prep %>% filter(.clean_doi != "")
  no_doi_df    <- df_prep %>% filter(.clean_doi == "") %>%
    mutate(
      openalex_id = NA_character_,
      openalex_title = NA_character_,
      openalex_authors = NA_character_,
      title_similarity = NA_real_,
      author_match_flag = NA,
      doi_verified = FALSE,
      verification_status = "Missing DOI in Source"
    )
  
  if (nrow(valid_doi_df) == 0) {
    warning("No valid DOIs found in the provided data frame.")
    return(df_prep %>% select(-.row_id, -.clean_doi))
  }
  
  # Unique DOIs to minimize unnecessary API hits
  unique_dois <- unique(valid_doi_df$.clean_doi)
  doi_chunks <- split(unique_dois, ceiling(seq_along(unique_dois) / batch_size))
  
  num_batches <- length(doi_chunks)
  
  # Function to query OpenAlex for a batch of up to 50 DOIs
  fetch_openalex_batch <- function(doi_vec) {
    doi_filter <- paste0("https://doi.org/", doi_vec, collapse = "|")
    
    req <- request("https://api.openalex.org/works") %>%
      req_url_query(
        filter = paste0("doi:", doi_filter),
        select = "id,doi,title,authorships",
        per_page = length(doi_vec),
        mailto = email
      ) %>%
      req_retry(max_tries = 3, backoff = ~ 2)
    
    resp <- tryCatch({
      req_perform(req)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(resp) || resp_status(resp) != 200) {
      return(tibble(
        .clean_doi = character(), openalex_id = character(), 
        openalex_title = character(), openalex_authors = character()
      ))
    }
    
    body <- resp_body_json(resp)
    results <- body$results
    
    if (length(results) == 0) {
      return(tibble(
        .clean_doi = character(), openalex_id = character(), 
        openalex_title = character(), openalex_authors = character()
      ))
    }
    
    map_dfr(results, function(work) {
      authors_list <- map_chr(work$authorships, ~ .x$author$display_name %||% "")
      author_str <- paste(authors_list, collapse = "; ")
      work_doi <- clean_doi_fn(work$doi %||% "")
      
      tibble(
        .clean_doi = work_doi,
        openalex_id = work$id %||% NA_character_,
        openalex_title = work$title %||% NA_character_,
        openalex_authors = author_str
      )
    })
  }
  
  # Initialize CLI Progress Bar
  cli_progress_bar(
    name = "Verifying DOIs via OpenAlex",
    total = num_batches,
    format = "{cli::pb_spin} {cli::pb_name} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )
  
  # Process all batches
  api_results_list <- vector("list", num_batches)
  for (i in seq_len(num_batches)) {
    api_results_list[[i]] <- fetch_openalex_batch(doi_chunks[[i]])
    cli_progress_update()
    Sys.sleep(0.1) # Polite pause
  }
  
  cli_progress_done()
  
  api_results <- bind_rows(api_results_list) %>%
    distinct(.clean_doi, .keep_all = TRUE)
  
  # Join back to original valid DOI records
  verified_df <- valid_doi_df %>%
    left_join(api_results, by = ".clean_doi") %>%
    mutate(
      orig_title_clean = tolower(gsub("[^[:alnum:] ]", "", .data[[title_col]])),
      oa_title_clean   = tolower(gsub("[^[:alnum:] ]", "", openalex_title)),
      
      title_similarity = ifelse(
        !is.na(oa_title_clean) & nchar(orig_title_clean) > 0,
        1 - stringdist(orig_title_clean, oa_title_clean, method = "jw"),
        0
      ),
      
      author_match_flag = map2_lgl(.data[[author_col]], openalex_authors, function(orig_auth, oa_auth) {
        if (is.na(orig_auth) || is.na(oa_auth) || orig_auth == "" || oa_auth == "") return(FALSE)
        tokens <- unlist(strsplit(tolower(orig_auth), "[^[:alpha:]]"))
        tokens <- tokens[nchar(tokens) > 2]
        if (length(tokens) == 0) return(FALSE)
        
        any(sapply(tokens, function(tok) grepl(tok, tolower(oa_auth), fixed = TRUE)))
      }),
      
      doi_verified = (title_similarity >= similarity_threshold) & author_match_flag,
      
      verification_status = case_when(
        is.na(openalex_id) ~ "DOI Not Found in OpenAlex",
        doi_verified ~ "Verified Match",
        title_similarity < similarity_threshold & !author_match_flag ~ "Title & Author Mismatch",
        title_similarity < similarity_threshold ~ "Title Mismatch",
        !author_match_flag ~ "Author Mismatch",
        TRUE ~ "Unverified"
      )
    ) %>%
    select(-orig_title_clean, -oa_title_clean)
  
  # Combine and restore original row order
  final_df <- bind_rows(verified_df, no_doi_df) %>%
    arrange(.row_id) %>%
    select(-.row_id, -.clean_doi)
  
  return(final_df)
}

resolve_unverified_dois_openalex <- function(df,
                                             title_col = "title",
                                             author_col = "author",
                                             doi_col = "doi",
                                             email = Sys.getenv("OPENALEX_MAILTO", unset = "cairns@tamu.edu"),
                                             similarity_threshold = 0.85,
                                             delay_sec = 0.12) {
  
  # Ensure necessary columns exist
  required_cols <- c(title_col, author_col, doi_col, "doi_verified")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns. Make sure input comes from `verify_dois_openalex()`. Missing:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  df_prep <- df %>% mutate(.row_id = row_number())
  
  # Filter for rows needing resolution
  needs_resolution <- df_prep %>%
    filter((is.na(doi_verified) | doi_verified == FALSE) & 
             !is.na(.data[[title_col]]) & 
             trimws(.data[[title_col]]) != "")
  
  if (nrow(needs_resolution) == 0) {
    cli::cli_alert_success("All rows are already verified or lack valid titles to search!")
    return(df_prep %>% select(-.row_id))
  }
  
  n_target <- nrow(needs_resolution)
  cli::cli_alert_info("Found {n_target} unverified records. Resolving via OpenAlex search API...")
  
  clean_str <- function(s) {
    if (is.na(s) || is.null(s)) return("")
    tolower(gsub("[^[:alnum:] ]", "", s))
  }
  
  cli::cli_progress_bar(
    name = "Searching OpenAlex",
    total = n_target,
    format = "{cli::pb_spin} {cli::pb_name} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )
  
  resolved_results_list <- vector("list", n_target)
  
  for (i in seq_len(n_target)) {
    row_data <- needs_resolution[i, ]
    title_val  <- row_data[[title_col]]
    author_val <- row_data[[author_col]]
    
    search_query <- trimws(title_val)
    
    # Extract first significant author token if available
    has_author <- !is.na(author_val) && trimws(author_val) != ""
    if (has_author) {
      author_tokens <- unlist(strsplit(author_val, "[^[:alpha:]]"))
      author_token <- author_tokens[nchar(author_tokens) > 2][1]
      if (!is.na(author_token)) {
        search_query <- paste(search_query, author_token)
      }
    }
    
    req <- httr2::request("https://api.openalex.org/works") %>%
      httr2::req_url_query(
        search = search_query,
        select = "id,doi,title,authorships",
        per_page = 3,
        mailto = email
      )
    
    # Safe request handling
    resp <- tryCatch({
      httr2::req_perform(req)
    }, httr2_http_429 = function(e) {
      Sys.sleep(10)
      tryCatch(httr2::req_perform(req), error = function(e2) NULL)
    }, error = function(e) {
      NULL
    })
    
    cli::cli_progress_update()
    
    if (!is.null(resp) && httr2::resp_status(resp) == 200) {
      body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      
      if (!is.null(body) && length(body$results) > 0) {
        orig_title_clean <- clean_str(title_val)
        
        best_match <- NULL
        highest_sim <- 0
        
        for (work in body$results) {
          oa_title <- work$title %||% ""
          oa_title_clean <- clean_str(oa_title)
          
          sim <- if (nchar(orig_title_clean) > 0 && nchar(oa_title_clean) > 0) {
            1 - stringdist::stringdist(orig_title_clean, oa_title_clean, method = "jw")
          } else {
            0
          }
          
          authors_list <- purrr::map_chr(work$authorships, ~ .x$author$display_name %||% "")
          oa_authors <- paste(authors_list, collapse = "; ")
          
          # Author validation logic
          if (has_author) {
            tokens <- unlist(strsplit(tolower(author_val), "[^[:alpha:]]"))
            tokens <- tokens[nchar(tokens) > 2]
            auth_match <- if (length(tokens) > 0) {
              any(sapply(tokens, function(tok) grepl(tok, tolower(oa_authors), fixed = TRUE)))
            } else {
              TRUE
            }
          } else {
            # If no author input, bypass author check
            auth_match <- TRUE
          }
          
          # Evaluate match quality
          if (sim >= similarity_threshold && auth_match && sim > highest_sim) {
            highest_sim <- sim
            
            found_doi <- work$doi %||% NA_character_
            if (!is.na(found_doi)) {
              found_doi <- gsub("^https?://(dx\\.)?doi\\.org/", "", found_doi, ignore.case = TRUE)
            }
            
            best_match <- dplyr::tibble(
              .row_id = row_data$.row_id,
              openalex_id = work$id %||% NA_character_,
              discovered_doi = found_doi,
              openalex_title = oa_title,
              openalex_authors = oa_authors,
              title_similarity = sim,
              author_match_flag = auth_match,
              doi_verified = TRUE,
              verification_status = "Resolved via Search"
            )
          }
        }
        
        if (!is.null(best_match)) {
          resolved_results_list[[i]] <- best_match
        }
      }
    }
    
    Sys.sleep(delay_sec)
  }
  
  cli::cli_progress_done()
  
  resolved_df <- dplyr::bind_rows(resolved_results_list)
  
  if (nrow(resolved_df) == 0) {
    cli::cli_alert_warning("No additional DOIs could be resolved from search results.")
    return(df_prep %>% dplyr::select(-.row_id))
  }
  
  # Ensure target metadata columns exist on df_prep before joining to prevent errors
  target_cols <- c("openalex_id", "openalex_title", "openalex_authors", 
                   "title_similarity", "author_match_flag", "verification_status")
  for (col in target_cols) {
    if (!col %in% names(df_prep)) df_prep[[col]] <- NA
  }
  
  # Merge resolved records back
  final_df <- df_prep %>%
    dplyr::left_join(resolved_df, by = ".row_id", suffix = c("", "_new")) %>%
    dplyr::mutate(
      !!dplyr::sym(doi_col) := dplyr::if_else(!is.na(discovered_doi), discovered_doi, .data[[doi_col]]),
      openalex_id = dplyr::coalesce(openalex_id_new, openalex_id),
      openalex_title = dplyr::coalesce(openalex_title_new, openalex_title),
      openalex_authors = dplyr::coalesce(openalex_authors_new, openalex_authors),
      title_similarity = dplyr::coalesce(title_similarity_new, title_similarity),
      author_match_flag = dplyr::coalesce(author_match_flag_new, author_match_flag),
      doi_verified = dplyr::coalesce(doi_verified_new, doi_verified),
      verification_status = dplyr::coalesce(verification_status_new, verification_status)
    ) %>%
    dplyr::select(-dplyr::ends_with("_new"), -dplyr::any_of("discovered_doi"), -.row_id)
  
  cli::cli_alert_success("Successfully resolved and updated {nrow(resolved_df)} records!")
  
  return(final_df)
}

resolve_unverified_dois_openalex_parallel <- function(df, 
                                                      title_col = "title", 
                                                      author_col = "author", 
                                                      doi_col = "doi", 
                                                      email = "cairns@tamu.edu", 
                                                      similarity_threshold = 0.85, 
                                                      batch_size = 15,
                                                      delay_sec = 0.25,
                                                      max_tries = 5) {
  
  # Ensure required input columns exist
  required_cols <- c(title_col, author_col, doi_col, "doi_verified")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns.",
      "i" = "Make sure input comes from {.fn verify_dois_openalex}.",
      "x" = "Missing columns: {.val {missing_cols}}"
    ))
  }
  
  # Guarantee tracking columns exist in input frame
  target_cols <- c("openalex_id", "openalex_title", "openalex_authors", 
                   "title_similarity", "author_match_flag", "verification_status")
  
  for (col in target_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  df_prep <- df %>% dplyr::mutate(.row_id = dplyr::row_number())
  
  # Filter for records needing search resolution
  needs_resolution <- df_prep %>% 
    dplyr::filter((is.na(.data$doi_verified) | .data$doi_verified == FALSE) & 
                    !is.na(.data[[title_col]]) & 
                    trimws(.data[[title_col]]) != "")
  
  if (nrow(needs_resolution) == 0) {
    cli::cli_alert_success("All rows are already verified or lack valid titles. No search needed.")
    return(df_prep %>% dplyr::select(-".row_id"))
  }
  
  n_target <- nrow(needs_resolution)
  
  # Split needs_resolution into a list of batch dataframes
  needs_resolution$batch_id <- ceiling(seq_len(n_target) / batch_size)
  batch_list <- split(needs_resolution, needs_resolution$batch_id)
  n_batches <- length(batch_list)
  
  # Check current future plan to alert user if parallel execution isn't enabled
  current_plan <- future::plan()
  if (inherits(current_plan, "sequential")) {
    cli::cli_alert_info("Running in sequential mode. For parallel processing, run `future::plan(future::multisession, workers = 3)` beforehand.")
  } else {
    cli::cli_alert_info("Running concurrently across active background workers...")
  }
  
  cli::cli_alert_info("Processing {n_target} record{?s} across {n_batches} batch{?es}...")
  
  clean_str <- function(s) {
    if (is.null(s) || is.na(s)) return("")
    tolower(gsub("[^[:alnum:] ]", "", s))
  }
  
  # Execute parallel batch processing using furrr
  resolved_df_raw <- furrr::future_map_dfr(
    batch_list,
    function(batch_rows) {
      batch_results <- list()
      
      # Extract titles and sanitize for search query parameter filter
      title_terms <- sapply(batch_rows[[title_col]], function(t) {
        words <- unlist(strsplit(clean_str(t), "\\s+"))
        words <- words[nchar(words) > 2]
        paste(head(words, 6), collapse = " ")
      })
      
      valid_terms <- title_terms[nchar(title_terms) > 0]
      
      if (length(valid_terms) > 0) {
        batch_search_filter <- paste0("title.search:", paste(valid_terms, collapse = "|"))
        
        req <- httr2::request("https://api.openalex.org/works") %>% 
          httr2::req_url_query(
            filter = batch_search_filter,
            select = "id,doi,title,authorships",
            per_page = min(50, length(valid_terms) * 5),
            mailto = email
          ) %>% 
          httr2::req_user_agent(paste0("DOI-Resolver-R/1.0 (mailto:", email, ")")) %>% 
          httr2::req_timeout(15) %>%  # Prevents hung sockets
          httr2::req_retry(
            max_tries = 3,           # Prevents workers hanging in retry loops
            is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
            backoff = function(attempt) min((2 ^ attempt) + stats::runif(1, 0, 1), 8) # Cap backoff
          )
        # req <- httr2::request("https://api.openalex.org/works") %>% 
        #   httr2::req_url_query(
        #     filter = batch_search_filter,
        #     select = "id,doi,title,authorships",
        #     per_page = min(50, length(valid_terms) * 5),
        #     mailto = email
        #   ) %>% 
        #   httr2::req_user_agent(paste0("DOI-Resolver-R/1.0 (mailto:", email, ")")) %>% 
        #   # Robust Rate-Limiting & Backoff Configuration
        #   httr2::req_retry(
        #     max_tries = max_tries,
        #     is_transient = function(resp) {
        #       status <- httr2::resp_status(resp)
        #       status %in% c(429, 500, 502, 503, 504)
        #     },
        #     backoff = function(attempt) {
        #       # Exponential backoff (2^attempt seconds) + random jitter up to 1s
        #       (2 ^ attempt) + stats::runif(1, 0, 1)
        #     }
        #   )
        
        resp <- tryCatch({
          httr2::req_perform(req)
        }, error = function(e) NULL)
        
        if (!is.null(resp) && httr2::resp_status(resp) == 200) {
          body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
          
          if (!is.null(body) && length(body$results) > 0) {
            
            for (i in seq_len(nrow(batch_rows))) {
              row_data <- batch_rows[i, ]
              orig_title <- row_data[[title_col]]
              orig_author <- row_data[[author_col]]
              orig_title_clean <- clean_str(orig_title)
              
              best_match <- NULL
              highest_sim <- 0
              
              for (work in body$results) {
                oa_title <- rlang::`%||%`(work$title, "")
                oa_title_clean <- clean_str(oa_title)
                
                sim <- if (nchar(orig_title_clean) > 0 && nchar(oa_title_clean) > 0) {
                  1 - stringdist::stringdist(orig_title_clean, oa_title_clean, method = "jw")
                } else {
                  0
                }
                
                authors_list <- purrr::map_chr(work$authorships, ~ rlang::`%||%`(.x$author$display_name, ""))
                oa_authors <- paste(authors_list, collapse = "; ")
                
                # Check author match
                if (is.na(orig_author) || trimws(orig_author) == "") {
                  auth_match <- TRUE
                } else {
                  auth_match <- FALSE
                  tokens <- unlist(strsplit(tolower(orig_author), "[^[:alpha:]]"))
                  tokens <- tokens[nchar(tokens) > 2]
                  
                  if (length(tokens) > 0) {
                    auth_match <- any(sapply(tokens, function(tok) {
                      grepl(tok, tolower(oa_authors), fixed = TRUE)
                    }))
                  }
                }
                
                if (sim >= similarity_threshold && auth_match && sim > highest_sim) {
                  highest_sim <- sim
                  found_doi <- rlang::`%||%`(work$doi, NA_character_)
                  
                  if (!is.na(found_doi)) {
                    found_doi <- gsub("^https?://(dx\\.)?doi\\.org/", "", found_doi, ignore.case = TRUE)
                  }
                  
                  best_match <- tibble::tibble(
                    .row_id = row_data$.row_id,
                    openalex_id = rlang::`%||%`(work$id, NA_character_),
                    discovered_doi = found_doi,
                    openalex_title = oa_title,
                    openalex_authors = oa_authors,
                    title_similarity = sim,
                    author_match_flag = auth_match,
                    doi_verified = TRUE,
                    verification_status = "Resolved via Parallel Search"
                  )
                }
              }
              
              if (!is.null(best_match)) {
                batch_results[[length(batch_results) + 1]] <- best_match
              }
            }
          }
        }
      }
      
      Sys.sleep(delay_sec)
      dplyr::bind_rows(batch_results)
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("dplyr", "httr2", "stringdist", "purrr", "rlang")
    ),
    .progress = TRUE
  )
  
  if (nrow(resolved_df_raw) == 0) {
    cli::cli_alert_warning("No additional DOIs could be resolved from parallel search results.")
    return(df_prep %>% dplyr::select(-".row_id"))
  }
  
  # Deduplicate matched items by highest similarity score
  resolved_df <- resolved_df_raw %>% 
    dplyr::group_by(.data$.row_id) %>% 
    dplyr::slice_max(order_by = .data$title_similarity, n = 1, with_ties = FALSE) %>% 
    dplyr::ungroup()
  
  # Dynamic Symbol evaluation for safe mutating
  doi_sym <- rlang::sym(doi_col)
  
  final_df <- df_prep %>% 
    dplyr::left_join(resolved_df, by = ".row_id", suffix = c("", "_new")) %>% 
    dplyr::mutate(
      !!doi_sym := dplyr::if_else(
        !is.na(.data$discovered_doi), 
        .data$discovered_doi, 
        as.character(.data[[doi_col]])
      ),
      openalex_id = dplyr::coalesce(.data$openalex_id_new, .data$openalex_id),
      openalex_title = dplyr::coalesce(.data$openalex_title_new, .data$openalex_title),
      openalex_authors = dplyr::coalesce(.data$openalex_authors_new, .data$openalex_authors),
      title_similarity = dplyr::coalesce(.data$title_similarity_new, .data$title_similarity),
      author_match_flag = dplyr::coalesce(.data$author_match_flag_new, .data$author_match_flag),
      doi_verified = dplyr::coalesce(.data$doi_verified_new, .data$doi_verified),
      verification_status = dplyr::coalesce(.data$verification_status_new, .data$verification_status)
    ) %>% 
    dplyr::select(
      -dplyr::ends_with("_new"), 
      -dplyr::any_of(c("discovered_doi", ".row_id", "batch_id"))
    )
  
  cli::cli_alert_success("Successfully resolved and updated {nrow(resolved_df)} record{?s}!")
  
  return(final_df)
}