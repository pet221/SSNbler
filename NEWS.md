# SSNbler (development version)

## Major updates

* `lines_to_lsn()` now uses a new, much faster method to build the LSN. Previously, distance matrices between all line end nodes were computed, so memory use grew with the square of the number of line features, and parallel processing was required for more than 46340 line features. Line ends `<= snap_tolerance` apart are now found using a spatial grid, so memory use and computation time grow roughly linearly with the number of line features and there is no limit on the number of line features. For example, an LSN with 34,468 line features was built in 4.7 seconds (previously 4.9 minutes), and one with 509,906 line features in 60 seconds.
* `snap_tolerance` and `topo_tolerance` in `lines_to_lsn()` no longer have defaults (previously both 0). `snap_tolerance` must always be provided (use `snap_tolerance = 0` to join only line ends with identical coordinates). `topo_tolerance` must be provided when `check_topology = TRUE`, and must then be greater than `snap_tolerance`, and therefore greater than 0. With the previous default `topo_tolerance = 0`, the Dangling Node check could not find any errors. Code that relied on the defaults must be updated.
* The `use_parallel` and `no_cores` arguments have been removed from `lines_to_lsn()`, because parallel processing is no longer needed. Code that sets these arguments must be updated. 'pdist', 'foreach' and 'doParallel' are no longer dependencies.

## Minor updates

* Added a `remove_ZM` argument to `sites_to_lsn()` (default `FALSE`). If `sites` or `edges` have Z and/or M dimensions, `sites_to_lsn()` now stops with an error unless `remove_ZM = TRUE`, in which case the Z and/or M dimensions are removed. Previously they were not checked, and Z or M coordinates could be included in the calculation of `ratio`.
* Added descriptions of each topological error type to the `lines_to_lsn()` documentation.
* `lines_to_lsn()` now checks that `snap_tolerance` and `topo_tolerance` are each a single, finite number `>= 0` (not missing or infinite), and stops with an error naming the argument otherwise. The documentation now states that `snap_tolerance = 0` means no snapping: only line ends with identical coordinates are joined into a node.
* `lines_to_lsn()` now creates `lsn_path` only after the input checks have passed, so a failed check no longer leaves an empty directory.
* `lines_to_lsn()` now stops with an error if `lsn_path` is an existing file rather than a folder. Previously this gave an error from GDAL when `edges.gpkg` was written.
* When `overwrite = FALSE`, `lines_to_lsn()` now checks for existing output files (`edges.gpkg`, `nodes.gpkg`, `noderelationships.csv`, `relationships.csv`, and `node_errors.gpkg` when `check_topology = TRUE`) before anything is written, and the error lists every file that exists. Previously each file was checked just before it was written, so a partly filled `lsn_path` could be left with new and old files mixed, and `relationships.csv` was always overwritten.
* When `overwrite = TRUE`, `lines_to_lsn()` now deletes its output files from an earlier run (`edges.gpkg`, `nodes.gpkg`, `noderelationships.csv`, `relationships.csv`, `node_errors.gpkg` and `nodexy.csv`) before anything is written, so a run that fails part way no longer leaves new and old files mixed in `lsn_path`. If a file can't be deleted (e.g. a GeoPackage open in GIS software on Windows), `lines_to_lsn()` stops with an error naming the file, before the LSN is built. Previously this was only found when writing that file, part way through the run.
* `lines_to_lsn()` now adds the `nodecat` column (Source, Outlet, Confluence or Pseudonode) to `nodes.gpkg` whether or not `check_topology = TRUE`. Previously it was only added when `check_topology = TRUE`. When `verbose = TRUE`, the final message now gives the number of outlets whether or not `check_topology = TRUE`.
* `lines_to_lsn()` no longer writes `nodexy.csv` to `lsn_path`. It was not used by any other 'SSNbler' function, and the node identifiers and coordinates are also stored in `nodes.gpkg`. When `overwrite = TRUE`, a `nodexy.csv` file from an earlier run is deleted.
* `lines_to_lsn()` now checks the geometry type of every feature in `streams`, and stops with an error if any feature is not a LINESTRING (e.g. a mix of LINESTRING and MULTILINESTRING features). Previously only the first feature was checked.
* `lines_to_lsn()` now stops with a clear error if `streams` has no features (zero rows). Previously this gave the R error "subscript out of bounds".
* `lines_to_lsn()` now stops with a clear error if `streams` has no coordinate reference system. Previously this gave the R error "missing value where TRUE/FALSE needed".
* `lines_to_lsn()` now stops with an error if any line feature in `streams` has fewer than two vertices, before anything is written to `lsn_path`.
* `node_errors.gpkg` now has two additional columns, `up_rid` and `down_rid`, which give the rids of the edges flowing into and out of each node as comma-separated text (e.g. "4, 7"), so flow relationships at each potential error can be checked quickly. `up_rid` is `NA` for a node with no inflowing edges (e.g. a Source) and `down_rid` is `NA` for a node with no outflowing edges (e.g. an Outlet). Both are `NA` for Intersection Without Node errors, which are not at a node.

## Bug fixes

* `lines_to_lsn()` computed distances between line end nodes with `pdist::pdist()`, which converts coordinates to single precision before subtracting. At projected coordinates (e.g. ~1e6 m), points a few cm apart could be read as 0 m apart, giving wrong edge relationships and false Unsnapped Node and Downstream Divergence errors for very short line features, and `snap_tolerance` values of a few cm or less could not be honoured. Distances are now computed in double precision (#8).
* `lines_to_lsn()` merged line end nodes by rounding their coordinates to a grid that can be coarser than `snap_tolerance` (e.g. 0.1 m when `snap_tolerance = 0.01`, and 10 m when `snap_tolerance = 0`, the default, whether or not `check_topology = TRUE`), but built edge relationships from distances. Both ends of a short line feature could round to the same node, so `noderelationships.csv` recorded the line as starting and ending at the same node, and edges ending at two separate nodes were treated as flowing into one confluence. When `relationships.csv` was still correct, no topology error was reported, but `afv_edges()` calculated wrong segment PIs at that node. This gave wrong AFVs for the edges and sites upstream of it (`afv_sites()`, `ssn_assemble()`), which `ssn_check()` does not detect. Nodes, `noderelationships.csv` and `relationships.csv` are now built from the same rule (line ends `<= snap_tolerance` apart are merged), and the two ends of a line feature are never merged into one node. AFVs calculated from existing LSNs that contain very short line features should be checked (#8).
* When the two ends of a line feature were `<= snap_tolerance` apart (e.g. a very short line feature, a line that nearly forms a loop, or a short line feature whose ends were read as 0 m apart because of the `pdist::pdist()` precision problem described above), `lines_to_lsn()` recorded the line as flowing into itself (`fromedge == toedge`) in `relationships.csv`, even when `noderelationships.csv` was correct. This could give false Downstream Divergence, Complex Confluence and Unsnapped Node errors at nearby nodes, and `updist_edges()`, `afv_edges()` and `accum_edges()` stopped with a downstream divergence error (from version 1.1.0; earlier versions did not check for this). The two ends of a line feature are now kept as separate nodes, so a line feature can no longer flow into itself (#8).
* When `use_parallel = TRUE`, `lines_to_lsn()` split the nodes into chunks and returned the Unsnapped Node flags found where a line end was 0 m from more than one node as row numbers within each chunk, but applied them to the full node table. These Unsnapped Node errors were therefore reported at the wrong nodes (one of the first few hundred nodes, sometimes a Source node), and were not reported at the nodes where they occurred. The parallel code has been removed (#8).
* `lines_to_lsn()` could report an Intersection Without Node at the same location as another error. When a line ended a few nanometres past another line, or two lines snapped into the same node crossed just short of it, the crossing point was not exactly on a line end node and was reported as well as the error at the node. Crossings within rounding error of a line end node, and crossings between two lines connected at a node within `snap_tolerance` of that node, are no longer reported as Intersection Without Node.
* `lines_to_lsn()` did not report an Intersection Without Node where two line features crossed more than once, or crossed and also shared a node or overlapped, because `sf::st_intersection()` returns these crossings as a single MULTIPOINT or GEOMETRYCOLLECTION, and only single points were kept. Each crossing is now checked and reported separately.
* `lines_to_lsn()` with `overwrite = TRUE` left `node_errors.gpkg` from an earlier run in `lsn_path` when the new run found no errors, so old errors could be mistaken for errors in the new LSN. It is now deleted.

## Behaviour changes

* `lines_to_lsn()` now merges line end nodes by distance: ends `<= snap_tolerance` apart are snapped into one node, and this is transitive, so a chain of ends that are each within `snap_tolerance` of the next forms one node. The node takes the coordinates of the first line end assigned to it, so node coordinates in `nodes.gpkg` are no longer rounded.
* Because distances are now exact, small gaps between line ends that were previously read as 0 m (connected, and sometimes flagged as Unsnapped Node) are now measured correctly. If such a gap is larger than `snap_tolerance`, the line ends are no longer snapped together, so a network may now split at that location. Gaps `<= topo_tolerance` are usually flagged as potential errors (e.g. Dangling Node), but this is not guaranteed. Check outputs from `lines_to_lsn()` for networks with very small gaps between line features.
* In `lines_to_lsn()`, Unsnapped Node now means that a line end `<= snap_tolerance` from another line end was not snapped to it, because this would have put both ends of a line feature into one node. It is not reported when the two line ends have the same coordinates as the two ends of the line feature that prevented the snap.
* `lines_to_lsn()` now gives a warning, rather than stopping with an error, if `snap_tolerance > 0` and any line feature has a length `<= snap_tolerance`. This check now applies whether or not `check_topology = TRUE`.
* When `check_topology = FALSE`, `lines_to_lsn()` now gives a warning if any line ends within `snap_tolerance` of another line end were not snapped because this would have put both ends of a line feature into one node.
* `lines_to_lsn()` reports a new error type, Chained Node, for a node containing line end nodes more than `snap_tolerance` apart. Because snapping is transitive, this happens when end nodes are linked by a chain of end nodes, each within `snap_tolerance` of the next. A `node_width` column in `node_errors.gpkg` gives the largest distance between two end nodes in each Chained Node (`NA` for other error types). When `check_topology = FALSE`, a warning gives the number of Chained Nodes instead.
* `lines_to_lsn()` reports a new error type, Confluence Without Node, where a line end node touches the interior of another line feature without a node (e.g. a T junction where the other line feature was not split). These were previously reported as Dangling Node, which now refers only to an outlet within `topo_tolerance` of another line feature. Confluence Without Node errors now have the `pointid` and `nodecat` of the node the line end node belongs to (previously `NA` and Confluence), so `up_rid` and `down_rid` are filled in. An outlet that touches another line feature is reported only as a Confluence Without Node, not also as a Dangling Node. Confluence Without Node is now reported for every node with a line end node on the interior of a line feature it is not connected to, including nodes where two or more line end nodes meet (e.g. a Pseudonode or Confluence on another line feature), which were previously not reported at all. The error is located at the node.
* In `node_errors.gpkg`, `nodecat` is now `NA` for Intersection Without Node errors, which are not nodes. Previously it was Confluence.
* In `lines_to_lsn()`, a Dangling Node is now an outlet within `topo_tolerance` of a line feature in a different network. Previously, it was an outlet whose `topo_tolerance` buffer was crossed by more than one line feature. That rule could flag an outlet whose last line feature is shorter than `topo_tolerance` when two line features upstream of it both crossed the buffer, and could miss a short line feature lying wholly inside the buffer of an outlet (e.g. an isolated line feature near another network). Line features in the outlet's own network are now ignored, because the outlet cannot connect to them without creating a loop. As a result, a short line feature leaving a Downstream Divergence and ending in an outlet is no longer also reported as a Dangling Node; the Downstream Divergence error lists it in `down_rid`.
* `lines_to_lsn()` reports a new error type, Network Without Outlet, for a network with no outlet: a loop of line features (e.g. digitised in a circle), with or without line features flowing into it. Every node on the loop is reported. Previously these networks were not reported, and `ssn_assemble()` cannot assign binary IDs to them.
* `lines_to_lsn()` reports a new error type, Self-loop, for a line feature whose upstream and downstream ends have identical coordinates. These line features are not included in `relationships.csv`.
* `lines_to_lsn()` now stops with an error if `check_topology = TRUE` and `topo_tolerance <= snap_tolerance`.
* `lines_to_lsn()` now stops with an error for XYZ geometries unless `remove_ZM = TRUE`, as it already did for XYM and XYZM geometries. Previously, Z coordinates were kept.
* In `nodes.gpkg`, `nodecat` is now Source for every node with no inflowing edges, and Outlet for every node with inflowing edges but no outflowing edges. Previously, a node with no inflowing edges and two or more outflowing edges (a Downstream Divergence) and a node with three or more inflowing edges and no outflowing edges (a Complex Confluence) were both categorised as Confluence. These nodes are still reported as errors in `node_errors.gpkg`. As before, a node with three or more inflowing edges and no outflowing edges is not checked for Dangling Node errors.

# SSNbler 1.1.2

## Minor updates

* Added informative error messages to lines_to_lsn(), ensuring that use_parallel = TRUE and no_cores > 0 when the number of line features in streams is > 46340.
* Minor updates to the Introduction vignette.
* Updates to Topology Editing and Delineating RCA tutorials.

## Bug fixes

* Fixed typo in lines_to_lsn() error message.

# SSNbler 1.1.1

## Minor updates

* Updated unit testing infrastructure.
* Updates to Topology Editing and Delineating RCA tutorials.

## Bug fixes

* Fixed bug in `lines_to_lsn`, which prevented tonodes from snapping when the distance between them was snap_tolerance/1e4 < distance < snap_tolerance. Now, nodes are snapped if the distance between them is < snap_tolerance.

# SSNbler 1.1.0

## Major updates

* Modified `lines_to_lsn()` and added various internal helper functions to support topology checking in parallel. Overcomes a 32-bit matrix indexing limitation in R (2^31-1), which in practice had limited the number of input `streams` LINESTRING features to 46340.

## Bug fixes

* Fixed bug in `updist_sites()` when `length_col already exists.
* Fixed bugs in `updist_sites()` and `afv_sites()` which caused error when skipping sf object and `length(sites) == 1`.
* Fixed bug in `updist_sites()` which occurred when the sf object didn't exist in `lsn_path` and arguments `save_local = TRUE` and `overwrite = FALSE`.
* Fixed bug in `lines_to_lsn()`, which caused the function to fail when `check_topology = FALSE`.
* Fixed bug in `lines_to_lsn()`, which caused some outlets to flagged as dangling node errors when they were not within `topo_tolerance` of another edge vertex or end node.
* Fixed logic error in `lines_to_lsn()`, where outlets with three features flowing in were not being flagged as toplogical errors.
* In `lines_to_lsn()`, added a check whether line features in streams have a length shorter than `snap_tolerance`. This prevents the end nodes of a short line feature from being snapped together (forming a loop) when `check_topology = TRUE`. When this occurs an informative error message is returned. 


# SSNbler 1.0.1

* Initial package version.
