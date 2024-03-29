#' @title stitchFieldCoordinates
#' @name stitchFieldCoordinates
#' @description Helper function to stitch field coordinates together to form one complete picture
#' @param location_file location dataframe with X and Y coordinates
#' @param offset_file dataframe that describes the offset for each field (see details)
#' @param cumulate_offset_x (boolean) Do the x-axis offset values need to be cumulated?
#' @param cumulate_offset_y (boolean) Do the y-axis offset values need to be cumulated?
#' @param field_col column that indicates the field within the location_file
#' @param X_coord_col column that indicates the x coordinates
#' @param Y_coord_col column that indicates the x coordinates
#' @param reverse_final_x (boolean) Do the final x coordinates need to be reversed?
#' @param reverse_final_y (boolean) Do the final y coordinates need to be reversed?
#' @return Updated location dataframe with new X \['X_final'\] and Y \['Y_final'\] coordinates
#' @details Stitching of fields:
#' \itemize{
#'   \item{1. have cell locations: }{at least 3 columns: field, X, Y}
#'   \item{2. create offset file: }{offset file has 3 columns: field, x_offset, y_offset}
#'   \item{3. create new cell location file by stitching original cell locations with stitchFieldCoordinates}
#'   \item{4. provide new cell location file to \code{\link{createGiottoObject}}}
#' }
#'
#' @export
stitchFieldCoordinates <- function(
        location_file,
        offset_file,
        cumulate_offset_x = F,
        cumulate_offset_y = F,
        field_col = "Field of View",
        X_coord_col = "X",
        Y_coord_col = "Y",
        reverse_final_x = F,
        reverse_final_y = T) {
    # data.table variables
    x_offset_final <- x_offset <- y_offset_final <- y_offset <- field <- NULL


    # cumulate offset values or not for offset file
    if (cumulate_offset_x == TRUE) {
        offset_file[, x_offset_final := cumsum(x_offset)]
    } else {
        offset_file[, x_offset_final := x_offset]
    }

    if (cumulate_offset_y == TRUE) {
        offset_file[, y_offset_final := cumsum(y_offset)]
    } else {
        offset_file[, y_offset_final := y_offset]
    }

    copy_loc_file <- data.table::copy(location_file)

    new_x_coord <- rep(0, nrow(copy_loc_file))
    new_y_coord <- rep(0, nrow(copy_loc_file))

    for (row in 1:nrow(copy_loc_file)) {
        myrow <- copy_loc_file[row, ]

        field_select <- myrow[[field_col]]
        X_select <- myrow[[X_coord_col]]
        Y_select <- myrow[[Y_coord_col]]

        X_offset <- offset_file[field == field_select][["x_offset_final"]]
        Y_offset <- offset_file[field == field_select][["y_offset_final"]]

        final_x <- X_select + X_offset
        final_y <- Y_select + Y_offset

        new_x_coord[row] <- final_x
        new_y_coord[row] <- final_y
    }

    if (reverse_final_x == TRUE) new_x_coord <- new_x_coord * -1
    if (reverse_final_y == TRUE) new_y_coord <- new_y_coord * -1

    copy_loc_file <- data.table(copy_loc_file)

    copy_loc_file[, c("X_final", "Y_final") := list(new_x_coord, new_y_coord)]

    return(copy_loc_file)
}


#' @title stitchTileCoordinates
#' @name stitchTileCoordinates
#' @description Helper function to stitch tile coordinates together to form one complete picture
#' @param location_file location dataframe with X and Y coordinates
#' @param Xtilespan numerical value specifying the width of each tile
#' @param Ytilespan numerical value specifying the height of each tile
#' @export
stitchTileCoordinates <- function(
        location_file,
        Xtilespan,
        Ytilespan) {
    # data.table variables
    Xcoord <- X.X <- XtileIndex <- Ycoord <- Y.Y <- YtileIndex <- NULL

    if (is.null(location_file$X.X)) {
        print("X coordinates missing in input file.")
    } else if (is.null(location_file$Y.Y)) {
        print("Y coordinates missing in input file.")
    } else if (is.null(location_file$XtileIndex)) {
        print("X tile index missing in input file.")
    } else if (is.null(location_file$YtileIndex)) {
        print("Y tile index missing in input file.")
    } else {
        copy_loc_file <- data.table::copy(location_file)
        copy_loc_file[, Xcoord := X.X + Xtilespan * (XtileIndex - 1)]
        copy_loc_file[, Ycoord := Y.Y + Ytilespan * (YtileIndex - 1)]
        return(copy_loc_file)
    }
}
