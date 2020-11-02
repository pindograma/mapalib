# mapalib.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

get_map_data = function(year, position, cities, epsg, aggregate_fun, ..., source_order = NULL, districts = NULL) {
  purrr::map_dfr(cities, function(city_id) {
    ibge_data = download_ibge_data(city_id)

    tracts = download_tracts(city_id, epsg) %>%
      dplyr::mutate(rn = dplyr::row_number())

    if (!is.null(districts)) {
      tracts = tracts %>%
        dplyr::filter(name_district %in% districts)
    }

    geocoded_sections = download_geocoded_sections(year, city_id) %>%
      append_chosen_latlon_to_geocoded_sections()

    votes_sum = download_section_votes(year, position, city_id) %>%
      aggregate_fun(position, ...)

    data = append_sections_to_tracts(tracts, dplyr::filter(geocoded_sections, ano == year), epsg) %>%
      dplyr::inner_join(votes_sum, by = c('zona' = 'NUM_ZONA', 'secao' = 'NUM_SECAO')) %>%
      dplyr::inner_join(ibge_data, by = c('code_tract' = 'Cod_setor'))

    spt = data %>%
      dplyr::group_by(code_tract) %>%
      dplyr::summarize(rn = dplyr::first(rn), cand = mean(cand)) %>%
      dplyr::filter(!is.na(cand)) %>%
      dplyr::mutate(main = rn)

    mapwalk(spt, tracts, spt$main, epsg) %>%
      dplyr::inner_join(ibge_data, by = c('code_tract' = 'Cod_setor')) %>%
      dplyr::group_by(main) %>%
      dplyr::summarize(cand = mean(cand, na.rm = T), renda = mean(basico_V009))
  })
}

get_map = function(...) {
  get_map_data(..., gen_map = T)
}

get_majoritarian_map_data = function(year, cities, epsg, position, turno, party_number) {
  get_map_data(year, cities, epsg, aggregate_majoritarian, position, turno, party_number)
}

get_minoritarian_map = function(year, cities, position, candidate_or_party_number, source_order) {
  NULL
}

get_minoritarian_ideology_map = function(year, cities, positions = c(6, 7), source_order) {
  NULL
}
