# mapper.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

gen_tract = function(cid, d, s, c) {
  paste0(
    cid,
    stringr::str_pad(d, 2, pad = '0'),
    stringr::str_pad(s, 2, pad = '0'),
    stringr::str_pad(c, 4, pad = '0'))
}

append_chosen_latlon_to_geocoded_sections = function(geocoded_secoes) {
  geocoded_secoes %>%
    dplyr::mutate(
      lat = dplyr::case_when(
        !is.na(comp_tse_lat) ~ comp_tse_lat,
        !is.na(tse_lat) ~ tse_lat,
        !is.na(google_lat) ~ google_lat,
        !is.na(inep_lat) ~ inep_lat,
        !is.na(places_lat) ~ places_lat,
        !is.na(google_approx_lat) ~ google_approx_lat,
        !is.na(ibge_approx_lat) ~ ibge_approx_lat,
        T ~ NA_real_),
      lon = dplyr::case_when(
        !is.na(comp_tse_lon) ~ comp_tse_lon,
        !is.na(tse_lon) ~ tse_lon,
        !is.na(google_lon) ~ google_lon,
        !is.na(inep_lon) ~ inep_lon,
        !is.na(places_lon) ~ places_lon,
        !is.na(google_approx_lon) ~ google_approx_lon,
        !is.na(ibge_approx_lon) ~ ibge_approx_lon,
        T ~ NA_real_)) %>%
  dplyr::mutate(code_tract = dplyr::case_when(
    !is.na(rural_Distrito) & !is.na(rural_Subdistrito) & !is.na(rural_CodSetor) ~
      gen_tract(codigo_ibge, rural_Distrito, rural_Subdistrito, rural_CodSetor),
    !is.na(ad_Distrito) & !is.na(ad_Subdistrito) & !is.na(ad_CodSetor) ~
      gen_tract(codigo_ibge, ad_Distrito, ad_Subdistrito, ad_CodSetor),
    !is.na(pl_Distrito) & !is.na(pl_Subdistrito) & !is.na(pl_CodSetor) ~
      gen_tract(codigo_ibge, pl_Distrito, pl_Subdistrito, pl_CodSetor),
    !is.na(approx_ad_Distrito) & !is.na(approx_ad_Subdistrito) & !is.na(approx_ad_CodSetor) ~
      gen_tract(codigo_ibge, approx_ad_Distrito, approx_ad_Subdistrito, approx_ad_CodSetor),
    T ~ NA_character_))
}

append_sections_to_tracts = function(tracts, geocoded_secoes, epsg) {
  if (!('lat' %in% colnames(geocoded_secoes))) {
    stop('append_sections_to_tracts() requires that one call append_chosen_latlon_to_geocoded_sections(), or do something equivalent, before')
  }

  points = geocoded_secoes %>%
    dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_transform(epsg) %>%
    dplyr::rename(geom = geometry)

  tracts_joined_1 = sf::st_join(tracts, points, sf::st_contains) %>%
    dplyr::select(-code_tract.y) %>%
    dplyr::rename(code_tract = code_tract.x)

  tracts_joined_2 = tracts %>%
    dplyr::inner_join(geocoded_secoes %>% dplyr::filter(is.na(lat) | is.na(lon)), by = 'code_tract') %>%
    dplyr::select(-lat, -lon)

  rbind(
    tracts_joined_1,
    tracts_joined_2
  )
}

get_section_points = function(geocoded_secoes, tracts, epsg) {
	if (!('lat' %in% colnames(geocoded_secoes))) {
		stop('get_section_points() requires that one call append_chosen_latlon_to_geocoded_sections(), or do something equivalent, before')
	}

  points1 = geocoded_secoes %>%
    dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_transform(epsg) %>%
    dplyr::rename(geom = geometry)

  points2 = tracts %>%
  	dplyr::inner_join(geocoded_secoes %>% dplyr::filter(is.na(lat) | is.na(lon)), 'code_tract') %>%
  	sf::st_centroid() %>%
  	dplyr::select(-lat, -lon)

  rbind(points1, points2)
}
