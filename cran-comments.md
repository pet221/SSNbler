
This is a major revision. `lines_to_lsn()` now uses a new, faster method to build the landscape network, reports new topology error types and fixes several bugs. There are breaking changes: `snap_tolerance` and `topo_tolerance` in `lines_to_lsn()` no longer have defaults, and the `use_parallel` and `no_cores` arguments are deprecated (ignored, with a warning). 'pdist', 'foreach' and 'doParallel' are no longer dependencies.

## R CMD check results SSNbler 1.1.2.9000

0 errors | 0 warnings | 0 notes

R CMD check succeeded