# =============================================================================
# viewCoordinator — cross-storage bridging for view + space resolution
# =============================================================================
#
# DESIGN NOTES (sketch, 2026-05-28)
# ---------------------------------
# A `viewCoordinator` is the dispatch tag for HOW a giottoView + giottoSpace
# recipe gets executed across the gobject's storage backings. It is NOT a
# strategy class for picking an execution engine — the engine is owned by
# the storage (GiottoDisk parquet stores expose lazy ops + storeRead output
# modes that already select the engine at materialization time). The
# coordinator's role is to provide a common protocol for moving IDs and
# effecting joins between storages so they can collaborate during view
# resolution.
#
# Pattern mirrors GiottoClass's other strategy generics (`processData`,
# `analyzeData`, etc.) where the entry-point generic dispatches on both the
# data class and the strategy class. Here the entry point is
# `resolveSubobject(subobj, gobject, view, space, coordinator, ...)`.
#
# Protocol methods the coordinator provides:
#   prepareIds(coordinator, ids, ...)
#     Promote an R-memory cell_ID character vector into the form the
#     coordinator's preferred backend wants. For dataTableCoordinator the
#     identity transform; for duckDBCoordinator an ephemeral duckdb-
#     registered table; for sedonaCoordinator a sedona view. The "prepared"
#     form is then usable as the right-hand side of a JOIN / IN filter
#     against any subobject the coordinator handles.
#
# Future-facing protocol (deferred until first cross-backing case lands):
#   applyIdsFilter(coordinator, subobj, prepared_ids, id_col)
#     Apply prepared IDs as a row filter on a subobject. Dispatches on
#     (coordinator, subobj_class) so each combination knows whether it's
#     an in-memory %in%, a backed JOIN, or an ephemeral-register-then-JOIN.
#   translatePredicate(coordinator, predicate, target_subobj)
#     Map an R predicate into the backend's filter form (delegating to the
#     store's existing `subset()` + `.r_expr_to_sql()` for parquet stores).
#
# Concrete coordinators:
#   dataTableCoordinator   — all in-memory; IDs as R character vector;
#                            apply via [cell_ID %in% ids]. Lives in
#                            GiottoClass. Default for non-disk gobjects;
#                            also the always-works in-memory fallback when
#                            mixed gobjects need promotion.
#   duckDBCoordinator      — ID promotion via duckdb_register_arrow /
#                            ephemeral table; apply via JOIN. Lives in
#                            GiottoDisk; registered when loaded.
#   sedonaCoordinator      — sedona view registration; full spatial
#                            vocabulary at apply time. Lives in GiottoDisk.
#
# Default selection from `gobject@source`: no source → dataTableCoordinator;
# parquet/sedona/duckdb-backed sources → respective concrete coordinator
# via the S3 hook `defaultViewCoordinator.<source_class>` registered from
# GiottoDisk. See `.default_view_coordinator()` in methods-resolver.R.
# =============================================================================


#' @title viewCoordinator virtual class
#' @description Base class for view + space resolution coordinators —
#' polymorphic dispatch surface that brokers IDs and joins between storage
#' backings during view resolution. The execution engine itself is owned by
#' the storage (see [GiottoDisk::storeRead] and its `output` modes).
#'
#' Concrete subclasses include [dataTableCoordinator-class] in GiottoClass and
#' (registered when loaded) `duckDBCoordinator` / `sedonaCoordinator` in
#' GiottoDisk.
#' @slot misc `list` for backend-specific options.
#' @returns `viewCoordinator`-inheriting object
#' @export
#' @exportClass viewCoordinator
setClass(
    "viewCoordinator",
    contains = "VIRTUAL",
    slots = list(misc = "list"),
    prototype = list(misc = list())
)


#' @title dataTableCoordinator
#' @description In-memory coordinator. Carries surviving cell_IDs as a plain
#' R character vector and applies them via `[cell_ID %in% ids]`-style
#' filtering. The reference implementation and always-works fallback for
#' any gobject regardless of backing (at the cost of materializing backed
#' subobjects into R memory).
#'
#' @returns `dataTableCoordinator`
#' @examples
#' dataTableCoordinator()
#' @export
#' @exportClass dataTableCoordinator
setClass("dataTableCoordinator", contains = "viewCoordinator")

#' @rdname dataTableCoordinator-class
#' @export
dataTableCoordinator <- function() new("dataTableCoordinator")
