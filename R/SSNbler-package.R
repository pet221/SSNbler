#' @keywords internal
"_PACKAGE"
#'
#' @importFrom sf st_agr st_as_sf st_as_text st_buffer st_cast st_coordinates st_crosses
#' st_crs st_crs<- st_drop_geometry st_equals st_geometry st_geometry<- st_geometry_type
#' st_intersection st_intersects st_is_empty st_is_longlat st_join st_length st_line_sample
#' st_linestring st_nearest_feature st_nearest_points st_read st_sfc st_write st_zm write_sf
#' @importFrom stats filter
#' @importFrom utils read.csv read.table write.table
#' @importFrom pdist pdist
#' @importFrom dplyr distinct left_join pull
#' @importFrom igraph decompose graph_from_data_frame reverse_edges shortest_paths subcomponent vertex_attr
#' @importFrom RSQLite dbConnect dbDisconnect dbExistsTable dbListTables dbReadTable dbRemoveTable dbWriteTable SQLite
#' @importFrom withr local_dir
#' @importFrom SSN2 copy_lsn_to_temp create_netgeom ssn_get_netgeom ssn_import
## usethis namespace: start
## usethis namespace: end
NULL

# SSN2 just needs to be installed for the print/summary generic to work. I
# am leaving it as "imported" right now, but we could replace it with
# importFrom SSN2 any_function_name_from_SSN2
# usually, you would need to import the generic, but since summary and print
# are in base, this is not necessary
