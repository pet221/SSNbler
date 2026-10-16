# SSNbler (development version)

## Bug fixes

* `lines_to_lsn()` computed distances between line end nodes with `pdist::pdist()`, which converts coordinates to single precision before subtracting. At projected coordinates (e.g. ~1e6 m), points a few cm apart could be read as 0 m apart, giving wrong edge relationships and false Unsnapped Node and Downstream Divergence errors for very short line features, and `snap_tolerance` values of a few cm or less could not be honoured. Distances are now computed in double precision using `proxy::dist()`, and 'pdist' is no longer a dependency (#8).

## Behaviour changes

* Because distances are now exact, cm-scale gaps between line ends that were previously read as 0 m (connected, and sometimes flagged as Unsnapped Node) are now measured correctly. If such a gap is larger than `snap_tolerance`, the lines are no longer connected and no error is reported, so a network may now split at that location. Check outputs from `lines_to_lsn()` for networks with very small gaps between line features.

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
