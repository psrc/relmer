build_query <- function(feature_dataset = NULL, feature_class = NULL) {

  tryCatch({
    feature_class_filter <- build_feature_class_filter(feature_class)
    feature_dataset_filter <- build_feature_dataset_filter(feature_dataset)
    sql <- glue::glue("select ",
      	"replace(d.PhysicalName, 'ELMERGEO.DBO.', '') as layer_name, ",
      	"replace(o.[Name],'ElmerGeo.DBO.', '') as feature_dataset, ",
      	"d.Definition.value('(/DEFeatureClassInfo/ShapeType/node())[1]', 'nvarchar(max)') as geometry_type ",
      "from SDE.GDB_ITEMS o ",
        "	left join  SDE.GDB_ITEMRELATIONSHIPS rel on rel.OriginID = o.UUID ",
        "	left join SDE.GDB_ItemTypes o_types on o.[Type] = o_types.UUID ",
        "	left join SDE.GDB_ITEMS d on rel.DestID = d.UUID ",
        "	left join SDE.GDB_ItemTypes d_types on d.[Type] = d_types.UUID ",
      "where ",
        "	d_types.[name] = 'Feature Class' ",
        "	and o_types.[name] = 'Feature Dataset' ",
        feature_class_filter,
        feature_dataset_filter,
      " order by o.[Name], d.[name]")
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_query: {e}"))
    stop(e)
  })
}

build_feature_dataset_filter <- function(feature_dataset = NULL) {

  tryCatch({
    filter <- ""
    if (!is.null(feature_dataset)) {
      filter <- glue::glue(" AND o.[Name] = 'ElmerGeo.DBO.{feature_dataset}' ")
    }
    return(filter)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_feature_dataset_filter: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_feature_dataset_filter: {e}"))
    stop(e)
  })
}


build_feature_class_filter <- function(feature_class = NULL) {

  tryCatch({
    filter <- ""
    if (!is.null(feature_class)) {
      filter <- glue::glue(" AND d.PhysicalName = 'ELMERGEO.DBO.{feature_class}'")
    }
    return(filter)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_feature_class_filter: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_feature_class_filter: {e}"))
    stop(e)
  })
}



#' list_feature_classes(feature_dataset, feature_class)
#'
#' list the feature classes available in ElmerGeo
#'
#' @param feature_dataset String.  If supplied, this filters the list to just those feature classes in the feature dataset
#' @param feature_class String.  If supplied, this filters the list to just the named one.  This can be useful if you know the name of the feature class but not the geography type or the feature dataset that it resides in.
#' @returns A dataframe listing the layer names along with the corresponding feature datasets and geometry types..  Defaults to NULL.
#'
#' @examples
#' list_feature_classes()
#' list_feature_classes(feature_dataset = "equity")
#'
#' @export
list_feature_classes <- function(feature_dataset = NULL, feature_class = NULL) {

  tryCatch({
    db_name <- 'ElmerGeo'
    conn <- get_conn(dbname = db_name)
    sql <- build_query(feature_dataset = feature_dataset, feature_class = feature_class)
    df <- DBI::dbGetQuery(conn, DBI::SQL(sql))
    DBI::dbDisconnect(conn)
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in list_feature_classes: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in list_feature_classes: {e}"))
    stop(e)
  })
}
