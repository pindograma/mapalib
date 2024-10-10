# download.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

download_geocoded_sections = function(year, city_id) {
  if (year == 2024) {
    return(readr::read_rds('http://pindograma-dados.s3.amazonaws.com/locais_votacao_2024.rds') %>%
      dplyr::mutate(ano = 2024, CD_MUNICIPIO = as.double(CD_MUNICIPIO)) %>%
      dplyr::left_join(ibge_tse_correspondence, c('CD_MUNICIPIO'='codigo_tse')) %>%
      dplyr::distinct(ano, SG_UF, codigo_ibge, NR_ZONA, NR_SECAO, NR_LATITUDE, NR_LONGITUDE) %>%
      dplyr::rename(uf = SG_UF, zona = NR_ZONA, secao = NR_SECAO, tse_lat = NR_LATITUDE, tse_lon = NR_LONGITUDE) %>%
      dplyr::mutate(comp_tse_lat = tse_lat, comp_tse_lon = tse_lon, inep_lat = NA, inep_lon = NA,
                    ad_lat = NA, ad_lon = NA, pl_lat = NA, pl_lon = NA, google_lat = NA, google_lon = NA,
                    local_lat = NA, local_lon = NA, places_lat = NA, places_lon = NA, pl_Distrito = NA,
                    pl_Subdistrito = NA, pl_CodSetor = NA, ad_Distrito = NA, ad_Subdistrito = NA,
                    ad_CodSetor = NA, rural_Distrito = NA, rural_Subdistrito = NA, rural_CodSetor = NA,
                    approx_ad_Distrito = NA, approx_ad_Subdistrito = NA, approx_ad_CodSetor = NA,
                    google_approx_lat = NA, google_approx_lon = NA, ibge_approx_lat = NA, ibge_approx_lon = NA,
                    results = 1))
  }

  if (year == 2022) {
    return(readr::read_rds('http://pindograma-dados.s3.amazonaws.com/locais_votacao_2022.rds') %>%
             dplyr::rename(ano = ano.x))
  }

  readr::read_csv(paste0('http://pindograma-dados.s3.amazonaws.com/mapa_locais_3/mapa_', city_id, '_', year, '.csv'),
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
  )) %>%
    dplyr::rename(ano = ano.x)
}

download_section_votes = function(year, position, city_id) {
  state = ibge_tse_correspondence %>%
    dplyr::filter(codigo_ibge == city_id) %>%
    dplyr::pull(uf)

  tse_id = ibge_tse_correspondence %>%
    dplyr::filter(codigo_ibge == city_id) %>%
    dplyr::pull(codigo_tse) %>%
    stringr::str_pad(5, pad = '0')

  if (year == 2024) {
    readr::read_csv(paste0('https://pindograma-dados.s3.amazonaws.com/secoes_eleitorais_2024/secoes_', tse_id, '.csv')) %>%
      dplyr::rename(NUM_TURNO = NR_TURNO) %>%
      dplyr::rename(CODIGO_CARGO = CD_CARGO) %>%
      dplyr::rename(NUMERO_CANDIDATO = NR_VOTAVEL) %>%
      dplyr::rename(NUM_ZONA = NR_ZONA, NUM_SECAO = NR_SECAO, QTDE_VOTOS = QT_VOTOS)
  } else if (year == 2022) {
    read_delim('/Users/daniel/Downloads/votacao_secao_2022_RJ/votacao_secao_2022_BR.csv', delim = ';', locale = locale(encoding = 'Latin1')) %>%
      dplyr::rename(NUM_TURNO = NR_TURNO) %>%
      dplyr::rename(CODIGO_CARGO = CD_CARGO) %>%
      dplyr::rename(NUMERO_CANDIDATO = NR_VOTAVEL) %>%
      dplyr::rename(NUM_ZONA = NR_ZONA, NUM_SECAO = NR_SECAO, QTDE_VOTOS = QT_VOTOS) %>%
      filter(CD_MUNICIPIO == tse_id)
  } else if (year == 2020) {
    readr::read_csv(paste0('https://pindograma-dados.s3.amazonaws.com/secoes_eleitorais_2020/secoes_', tse_id, '.csv')) %>%
      dplyr::rename(NUM_TURNO = NR_TURNO) %>%
      dplyr::rename(CODIGO_CARGO = CD_CARGO) %>%
      dplyr::rename(NUMERO_CANDIDATO = NR_VOTAVEL) %>%
      dplyr::rename(NUM_ZONA = NR_ZONA, NUM_SECAO = NR_SECAO, QTDE_VOTOS = QT_VOTOS)
  } else {
    cepespR::get_votes(year, position, 'Electoral Section', state = state,
                       cached = T, blank_votes = T, null_votes = T) %>%
      dplyr::filter(COD_MUN_IBGE == city_id)
  }
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

