
## KEYWORD EXTRACTION SCRIPT FOR ROSSI

# Note that this script will output a dataframe saved as an .RData file
# which will have nrow = number of articles,
# 1 Identifier column "analysis_id" and 
# 1 column for each unique "label" in the search-query-keywords.xlsx file, 
## which will have a 0 (no match) or 1 (match)




## Load libraries and functions
library(dplyr)

## Make sure these files are uploaded from the github onto the rossi working directory
# following the same directory structure
source(here::here("R/utils.R")) 
source(here::here("R/clean_string.R")) 
source(here::here("R/bool_detect.R")) 
searchQueries <- readxl::read_excel(here::here("data/raw-data/search-query-keywords.xlsx"),
                                    sheet = "search_query")

## Load data
# here replace the filePath with the path to an .RData file for an object "df"
# which is a data frame with the following column names:
# analysis_id = the unique identifier for the article
# title = the title of the article
# abstract = the abstract of the article
# keywords = the keywords of the article

# For now, create example data frame -- comment out when running the actual code, and 
# fill in the info on lines 44 and 45 instead**
df <- data.frame(
  analysis_id = seq(1:3),
  title = c("functional diversity","harry potter and the philosopher's stone","Genetic diversity"),
  abstract = do.call(paste,Map(stringi::stri_rand_strings, n=3, length=c(5, 4, 1),
                               pattern = c('[A-Z]', '[A-Z]', '[A-Z]'))),
  keywords = do.call(paste,Map(stringi::stri_rand_strings, n=3, length=c(5, 4, 1),
                               pattern = c('[A-Z]', '[A-Z]', '[A-Z]')))
)

# # Fill this in instead**
# filePath <- here::here("exampleDataFrame.RData") 
# load(filePath) # loads object "df"



## Parallelize keyword matching
results = parallel::mclapply(1:nrow(df), function(i){
  
  # Clean text and collapse into one element
  text <- clean_string(paste(df$title[i], 
                             df$abstract[i], 
                             df$keywords[i], 
                             collapse = " "))
  
  # match for different queries -- if you have a ton of queries you can 
  # parallelize this loop too but I don't think it's needed -- it runs pretty fast already
  qry_match <- matrix(nrow=1, ncol = nrow(searchQueries))
  for(q in 1:nrow(searchQueries)){
    qry_match[1,q] <- bool_detect2(text, searchQueries$boolean_search_query[q])
  }
  
  
  ## Bind both together into dataframe to return
  matches <- as.data.frame(cbind(df$analysis_id[i], qry_match))
  colnames(matches) <- c("analysis_id", searchQueries$label)
  
  return(matches)

  
}, mc.cores = 10) ## End of mclapply -- choose desired number of cores here


## Bind results together
matches_all <- do.call(rbind.data.frame, results)

## Save
save(matches_all, file = here::here("data/derived-data/search-query-keywords-matches.RData"))




