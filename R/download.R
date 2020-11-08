# download.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

download_geocoded_sections = function(year, city_id) {
  readr::read_csv(paste0('http://pindograma-dados.s3.amazonaws.com/mapa_locais_2/mapa_', city_id, '_', year, '.csv'),
                  col_types = readr::cols(
                    comp_tse_lat = readr::col_double(), comp_tse_lon = readr::col_double(),
                    tse_lat = readr::col_double(), tse_lon = readr::col_double(),
                    inep_lat = readr::col_double(), inep_lon = readr::col_double(),
                    google_lat = readr::col_double(), google_lon = readr::col_double(),
                    places_lat = readr::col_double(), places_lon = readr::col_double(),
                    rural_Distrito = readr::col_double(), rural_Subdistrito = readr::col_double(), rural_CodSetor = readr::col_double(),
                    pl_Distrito = readr::col_double(), pl_Subdistrito = readr::col_double(), pl_CodSetor = readr::col_double(),
                    ad_Distrito = readr::col_double(), ad_Subdistrito = readr::col_double(), ad_CodSetor = readr::col_double(),
                    approx_ad_Distrito = readr::col_double(), approx_ad_Subdistrito = readr::col_double(), approx_ad_CodSetor = readr::col_double(),
                    google_approx_lat = readr::col_double(), google_approx_lon = readr::col_double(),
                    ibge_approx_lat = readr::col_double(), ibge_approx_lon = readr::col_double()
  ))
}

download_section_votes = function(year, position, city_id) {
  state = ibge_tse_correspondence %>%
    dplyr::filter(codigo_ibge == city_id) %>%
    dplyr::pull(uf)

  cepespR::get_votes(year, position, 'Electoral Section', state = state,
                     cached = T, blank_votes = T, null_votes = T) %>%
    dplyr::filter(COD_MUN_IBGE == city_id)
}

download_ibge_data = function(city_id) {
  normalize_names = function(x, prefix) {
    paste0(prefix, '_', x)
  }

  state = ibge_tse_correspondence %>%
    dplyr::filter(codigo_ibge == city_id) %>%
    dplyr::pull(uf)

  if (state == 'SP') {
    if (city_id == '3550308') { state = 'SP1' }
    else { state = 'SP2' }
  }

  suppressWarnings(
   readr::read_csv2(paste0('http://pindograma-dados.s3.amazonaws.com/ibge_basico/Basico_', state, '.csv'),
                    locale = readr::locale(encoding = 'ISO-8859-1'),
                    col_types = readr::cols(Cod_setor = readr::col_character())) %>%
   dplyr::rename_with(normalize_names, starts_with('V'), 'basico')
  )
}

download_tracts = function(city_id, epsg) {
  geobr::read_census_tract(as.numeric(city_id), simplified = F) %>%
    sf::st_transform(epsg) %>%
    dplyr::mutate(Distrito = as.numeric(stringr::str_sub(code_district, start = -2))) %>%
    dplyr::mutate(Subdistrito = as.numeric(stringr::str_sub(code_subdistrict, start = -2))) %>%
    dplyr::mutate(CodSetor = as.numeric(stringr::str_sub(code_tract, start = -4)))
}

