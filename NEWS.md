# SSNbler (development version)

This update focuses on improving the computational efficiency of `lines_to_lsn()` and addresses several bugs.

## Major updates

* `lines_to_lsn()` now uses a new, faster method to build the LSN. Previously, distance matrices between all line end nodes were computed, so memory use grew with the square of the number of line features, and parallel processing was required for more than 46340 line features. Line end nodes `<= snap_tolerance` apart are now found using a spatial grid, so memory use and computation time grow roughly linearly with the number of line features and there is no limit on the number of line features. 

* `snap_tolerance` and `topo_tolerance` in `lines_to_lsn()` no longer have defaults (previously both 0). `snap_tolerance` must always be set by the user, and `topo_tolerance` must be set when `check_topology = TRUE`.

* The `use_parallel` and `no_cores` arguments have been deprecated in `lines_to_lsn()`, because parallel processing is no longer needed. The deprecated arguments are ignored, with a warning, rather than removed. Code that sets these arguments should be updated. 'pdist', 'foreach' and 'doParallel' are no longer dependencies. 

* `lines_to_lsn()` reports four new error types in `node_errors.gpkg` when `check_topology = TRUE`:
  * **Chained Node**: a snapped node containing multiple line end nodes, with some more than `snap_tolerance` apart. Snapping is transitive, so this happens when end nodes are linked by a chain of end nodes, each within `snap_tolerance` of the next. A new `node_width` column gives the largest distance between two line end nodes associated with each Chained Node (`NA` for other error types). When `check_topology = FALSE`, a warning gives the number of Chained Nodes instead.
  * **Confluence Without Node**: a line end node on the interior of another line feature it is not connected to (e.g. a T junction where a line feature is not split). These were previously reported as Dangling Node, or not at all when two or more line end nodes met at that location. Dangling Node now refers only to an outlet within `topo_tolerance` of a line feature in a different network.
  * **Network Without Outlet**: a network with no outlet (e.g. a loop of line features digitised in a circle), with or without line features flowing into it. Every node on the loop is reported as an error. These networks were previously not reported and would cause features to be silently dropped in downstream functions.
  * **Self-loop**: a line feature whose upstream and downstream end nodes have identical coordinates. These line features are not included in `relationships.csv`.


## Minor updates

* Added a `remove_ZM` argument to `sites_to_lsn()` (default `FALSE`). Previously Z and/or M dimensions were not checked in `sites` and `edges`, and Z or M coordinates could be included in the calculation of `ratio`. `sites_to_lsn()` now stops with an error for Z/M dimensions unless `remove_ZM = TRUE`.

* Improvements to documentation in several places.

* More extensive checks of inputs in `lines_to_lsn()` with informative error messages.

* When `overwrite = TRUE`, `lines_to_lsn()` now deletes its output files from an earlier run before anything is written, so a run that fails part way no longer leaves new and old files mixed in `lsn_path`. If a file can't be deleted, `lines_to_lsn()` stops with an informative error.

* `lines_to_lsn()` now adds the `nodecat` column (Source, Outlet, Confluence or Pseudonode) to `nodes.gpkg` whether or not `check_topology = TRUE`. When `verbose = TRUE`, the final message now gives the number of outlets whether or not `check_topology = TRUE`.

* `lines_to_lsn()` no longer writes `nodexy.csv` to `lsn_path`. It was not used by any other 'SSNbler' function, and the node identifiers and coordinates are also stored in `nodes.gpkg`. When `overwrite = TRUE`, a `nodexy.csv` file from an earlier run is deleted.

* `lines_to_lsn()` now checks the geometry type of every feature in `streams`, and stops with an error if any feature is not a LINESTRING. Previously only the first feature was checked.


## Bug fixes

* `lines_to_lsn()` computed distances between line end nodes with `pdist::pdist()`, which converts coordinates to single precision before subtracting. At projected coordinates (e.g. ~1e6 m), points a few cm apart could be read as 0 m apart, leading to incorrect edge relationships and false Unsnapped Node and Downstream Divergence errors for very short line features, and `snap_tolerance` values of a few cm or less could not be honoured. Distances are now computed in double precision (#8).

* In previous versions, `lines_to_lsn()` used two different methods to build the LSN. Nodes (`nodes.gpkg`, `noderelationships.csv`) were found by rounding line end node coordinates, while edge relationships (`relationships.csv`) were found by measuring the distance between line end nodes. The two methods could disagree. For example:
  * Both end nodes of a short line feature could be rounded into one node. This was not reported as a topology error unless the line feature was shorter than `snap_tolerance`.
  * Line end nodes more than `snap_tolerance` apart could be rounded into one node. This was never reported as a topology error. With `snap_tolerance = 0` (the previous default), coordinates were rounded to the nearest 10 m in versions 1.1.1 and 1.1.2, and to the nearest 1 m in earlier versions.
  * Line end nodes within `snap_tolerance` of each other could be rounded into separate nodes, splitting a confluence in two. These were reported as Unsnapped Node errors.

	When this happened, `relationships.csv` was usually still correct, but `afv_edges()` calculated incorrect segment PIs at the affected nodes. This gave incorrect AFVs for the edges and sites upstream. `ssn_check()` does not detect this problem.

	Nodes and edge relationships are now built using the same method: line end nodes `<= snap_tolerance` apart are merged into one node, and the two end nodes of a line feature are never merged. AFVs calculated from LSNs built with earlier versions should be checked, especially if `snap_tolerance = 0` was used or the streams contain very short line features (#8).

* When the two end nodes of a line feature were `<= snap_tolerance` apart (e.g. a very short line feature, a line that nearly forms a loop, or a short line feature whose end nodes were read as 0 m apart because of the `pdist::pdist()` precision problem described above), `lines_to_lsn()` recorded the line as flowing into itself (`fromedge == toedge`) in `relationships.csv`, even when `noderelationships.csv` was correct. This could give false Downstream Divergence, Complex Confluence and Unsnapped Node errors at nearby nodes, and `updist_edges()`, `afv_edges()` and `accum_edges()` stopped with a downstream divergence error (from version 1.1.0; earlier versions did not check for this). The two end nodes of a line feature are now kept as separate nodes, so a line feature can no longer flow into itself (#8).

* When `use_parallel = TRUE`, `lines_to_lsn()` split the nodes into chunks and returned the Unsnapped Node flags found where a line end node was 0 m from more than one node as row numbers within each chunk, but applied them to the full node table. These Unsnapped Node errors were therefore reported at the wrong nodes instead of at the nodes where they occurred. The parallel code has been removed (#8).

* `lines_to_lsn()` could report an Intersection Without Node at the same location as another error. When a line ended a few nanometres past another line, or two lines snapped into the same node crossed just short of it, the crossing point was not exactly on a line end node and was reported in addition to the error at the node. Edge intersections within rounding error of a line end node, and intersections between two lines connected at a node within `snap_tolerance` of that node, are no longer reported as Intersection Without Node.

* `lines_to_lsn()` did not report an Intersection Without Node where two line features crossed more than once, or crossed and also shared a node or overlapped, because `sf::st_intersection()` returns these crossings as a single MULTIPOINT or GEOMETRYCOLLECTION, and only single points were kept. Each crossing is now checked and reported separately.

* `lines_to_lsn()` now stops with an error for XYZ geometries unless `remove_ZM = TRUE`, as it already did for XYM and XYZM geometries. Previously, Z coordinates were kept.

* `lines_to_lsn()` with `overwrite = TRUE` left `node_errors.gpkg` from an earlier run in `lsn_path` when the new run found no errors. It is now deleted.


## Behaviour changes

* In `lines_to_lsn()`, Unsnapped Node now means that a line end node `<= snap_tolerance` from another line end node was not snapped to it, because this would have put both end nodes of a line feature into one node. It is not reported when the two line end nodes have the same coordinates as the two end nodes of the line feature that prevented the snap.

* If `snap_tolerance > 0` and any line feature has a length `<= snap_tolerance`, `lines_to_lsn()` now gives a warning, rather than stopping with an error. This check now applies whether or not `check_topology = TRUE`.

* In `lines_to_lsn()`, `topo_tolerance` must now be greater than `snap_tolerance`. 

* When `check_topology = FALSE`, `lines_to_lsn()` now gives a warning if any line end nodes within `snap_tolerance` of another line end node were not snapped because this would have put both end nodes of a line feature into one node.

* `node_errors.gpkg` now has three additional columns: `up_rid` and `down_rid`, which give the rids of the edges flowing into and out of each node as comma-separated text. `node_width` records the maximum distance between line end nodes associated with a Chained Node. 

* In `node_errors.gpkg`, `nodecat` is now `NA` for Intersection Without Node errors, which are not nodes. Previously it was Confluence.

* In `lines_to_lsn()`, a Dangling Node is now an outlet within `topo_tolerance` of a line feature in a different network. Previously, it was an outlet whose `topo_tolerance` buffer was crossed by more than one line feature. An outlet that touches another line feature is now reported only as a Confluence Without Node, not also as a Dangling Node. 

* In `nodes.gpkg`, `nodecat` is now Source for every node with no inflowing edges, and Outlet for every node with inflowing edges but no outflowing edges. Previously, a node with no inflowing edges and two or more outflowing edges (a Downstream Divergence) and a node with three or more inflowing edges and no outflowing edges (a Complex Confluence) were both categorised as Confluence. These nodes are still reported as errors in `node_errors.gpkg`. 

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
