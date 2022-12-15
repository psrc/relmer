build_query <- function(feature_dataset = NULL) {
  
  tryCatch({
    if (is.null(feature_dataset)) {
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
        "order by o.[Name], d.[name]") 
    } else {
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
          " and o.[Name] = 'ElmerGeo.DBO.{feature_dataset}' ",
        "order by o.[Name], d.[name]") 
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_query: {e}"))
    stop(e)
  })
}



#' list_layers(feature_dataset)
#' 
#' list the layers available in ElmerGeo
#' 
#' @param feature_dataset String.  If supplied, this filters the list to just those layers in the feature dataset
#' @returns A dataframe listing the layer names along with the corresponding feature datasets and geometry types..  Defaults to NULL.
#' 
#' @examples 
#' list_layers()
#' list_layers(feature_dataset = "equity")
#' 
#' @export
list_layers <- function(feature_dataset = NULL) {
  
  tryCatch({
    db_name <- 'ElmerGeo'
    conn <- get_conn(dbname = db_name)
    sql <- build_query(feature_dataset = feature_dataset)
    df <- DBI::dbGetQuery(conn, DBI::SQL(sql))
    DBI::dbDisconnect(conn)
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in list_layers: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in list_layers: {e}"))
    stop(e)
  })
}