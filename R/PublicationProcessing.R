# Testing verification of DOI with OpenAlex



# verified_data <- verify_dois_openalex(
#   df = testPubsFiltered3, 
#   title_col = "title",
#   author_col = "author",
#   doi_col = "doi",
#   email = "cairns@tamu.edu"
# )
# 
# 
# usePubsSkinny <- usePubs_verified_doi |>
#   filter(status != "Valid Match") |>
#   left_join(testPubsFiltered3, by="uid") |>
#   rename("doi"="doi.y") |>
#   mutate(doi_verified = FALSE)
#   
# 
# fully_resolved_data <- resolve_unverified_dois_openalex(
#   df = usePubsSkinny,
#   title_col = "title",
#   author_col = "author",
#   doi_col = "doi",
#   email = "cairns@tamu.edu"
# )
# 
# 
# 
# library(future)
# library(furrr)
# 
# # Keep workers capped at 3 to prevent API saturation
# plan(multisession, workers = 3)
# 
# # Run resolution
# results <- resolve_unverified_dois_openalex_parallel(
#   df = usePubsSkinny[1000,],
#   email = "cairns@tamu.edu",
#   batch_size = 15,
#   delay_sec = 0.25
# )
# 
# # Return plan to single thread
# plan(sequential)

USER_EMAIL <- Sys.getenv("DOI_USER_EMAIL", unset = "cairns@tamu.edu")
# Make skinnyData
testPubsFiltered <- remove_html(testPubsFiltered, col_name="Title")
skinny1 <- makeSkinnyDataFrame(testPubsFiltered) 
resolvedPubs <- skinny1 |>
  filter(!is.na(DOI))


# Find the no_DOI pubs
skinny1_noDOI <- findNoDOI_Pubs(skinny1)

skinny2 <- batch_get_dois(skinny1_noDOI, source="crossref")    #Takes 30 mins or more to run with 2724 unknown DOIs. Results are cached though, so subsequent runs are faster.

resolvedPubs <- add_new_doi_values(resolvedPubs, skinny2)

skinny2_noDOI <- skinny2 |>
  mutate(DOI=found_doi) |>
  select(-any_of(c("found_doi", "title_dist"))) |>
  filter(is.na(DOI))

skinny3 <- batch_get_dois(skinny2_noDOI, doi_source="crossref")

resolvedPubs <- add_new_doi_values(resolvedPubs, skinny3)

skinny3_noDOI <- skinny3 |>
  mutate(DOI=found_doi) |>
  select(-any_of(c("found_doi", "title_dist"))) |>
  filter(is.na(DOI))

skinny4 <- batch_get_dois(skinny3_noDOI, doi_source="pubmed")

resolvedPubs <- add_new_doi_values(resolvedPubs, skinny4)

skinny5 <- add_s2_dois(skinny4, api_key=s2_api_key, sleep_sec=1.1)
skinny5 <- skinny5 |>
  mutate(found_doi = doi_s2) |>
  rename("Year...22"="Year")

resolvedPubs <- add_new_doi_values(resolvedPubs, skinny5)
skinny5_noDOI <- skinny5 |>
  mutate(DOI=found_doi) |>
  select(-any_of(c("found_doi", "title_dist"))) |>
  filter(is.na(DOI))

# *******Alternate way of getting s2 dois ************** #
handlers(global = TRUE)
handlers("cli") 

# 3. Run safely
with_progress({
  final_df <- add_s2_dois_safe(
    df = my_df,
    checkpoint_file = "my_doi_checkpoint.rds",
    batch_size = 2
  )
})



