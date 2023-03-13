## Descarga de todos los datos para Montevideo, Canelones, Maldonado y Rocha

getAllObservations <- function(place_id){
  start.time <- Sys.time()
  start.time
  
  total_results = NULL
  page = 1 
  delay = 1.2
  results = tibble()
  last_id = ''
  
  while(is.null(total_results) || nrow(results) < total_results) {
    call_url <- str_glue('https://api.inaturalist.org/v1/observations?',
                         'place_id={place_id}',
                         '&captive=false&geoprivacy=open',
                         '&order_by=id&order=asc&order_by=created_at',
                         '&per_page=100&page={page}',
                         '&id_above={last_id}')
    
    get_json_call <- GET(url = call_url) %>% content(as = "text") %>% fromJSON(flatten = TRUE)
    if (!is.null(get_json_call)) {
      if (is.null(total_results)) {
        total_results = get_json_call$total_results
      }
      results_i <- as_tibble(get_json_call$results) %>% 
        select(taxon.name, taxon.rank, identifications_count, observed_on, 
               geojson.coordinates, positional_accuracy,
               user.login, user.id, user.name, user.observations_count,
               user.identifications_count, user.activity_count, 
               license_code, num_identification_agreements) %>%
        unnest_wider(geojson.coordinates, names_sep = "_") %>%
        rename(longitude=geojson.coordinates_1, latitude=geojson.coordinates_2)
      results <- rbind(results, results_i)
      page <- page + 1
      Sys.sleep(delay)
    }
    # last_id = tail(get_json_call$results$id, n=1)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  return(results)
}

# place_id
montevideo_place_id=12416
canelones_place_id=12410 
maldonado_place_id=12415
rocha_place_id=12420

# montevideoObservations <- getAllObservations(montevideo_place_id)
# canelonesObservations <- getAllObservations(canelones_place_id)
maldonadoObservations <- getAllObservations(maldonado_place_id)
rochaObservations <- getAllObservations(rocha_place_id)

allObservations <- bind_rows(montevideoObservations, 
                             canelonesObservations,
                             maldonadoObservations,
                             rochaObservations)

# saveRDS(allObservations, '../data/allObservations.rds')

# allObservations <- read_csv('data/observations-302170.csv', guess_max = 30000)


# when using several place_id use `place_id_string` in the function
# get the place_id string
# i = 1
# p <- length(place_id)
# place_id_string <- str_glue('{place_id[i]}')
# while(p!=1){
#   i = i+1
#   place_id_string <- str_glue('{place_id_string}%2C{place_id[i]}')
#   p = p-1
# }
# place_id <- c(12416, 12410, 12415, 12420)
