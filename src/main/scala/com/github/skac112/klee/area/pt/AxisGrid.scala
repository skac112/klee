package com.github.skac112.klee.area.pt

import com.github.skac112.klee.area.img.{AxisRect, ImgArea}
import com.github.skac112.vgutils.{Bounds, Point}

import scala.math._
import cats.data.State

//object AxisRect {
//
//}

/**
  * Point area of regular (rectangular) grid of points. Points form horizontal and vertical lines. ImgArea
  * of this point area is an AxisRect with coordinates having offset of (.5*dx, .5*dy) size due to
  * actual range of points. This solution made tiling easier. One can also imagine this grid as a grid of size (nx * ny)
  * (horizontally / vertically) rectangles width sides of length dx and dy with point placed in a center of each
  * rectangle.
  * @param leftTop left top coordinate of image area (not left-top point which has offset of (.5*dx, .5*dy)
  * @param nx number of columns of points
  * @param ny number of rows of points
  * @param dx distance between consecutive columns
  * @param dy  distance between consecutive rows
  */
case class AxisGrid(leftTop: Point, nx: Int, ny: Int, dx: Double, dy: Double) extends PtArea {
  override lazy val points = {
    // coordinates of left top point (with offset from leftTop)
    val real_lt = leftTop + Point(.5*dx, .5*dy)
    (0 until nx*ny) map { i: Int => real_lt + Point((i % nx) * dx, (i / nx) * dy) }
  }

  def pointRow(rowNum: Int): Seq[Point] = points.slice(rowNum * nx, (rowNum * nx) + 1)

  def pointCol(colNum: Int): Seq[Point] = (0 until ny) map { row: Int => points(row * nx + colNum) }

  override lazy val area = AxisRect(leftTop, nx * dx, ny * dy)

  def this(rect: AxisRect, dx: Double, dy: Double) = this(rect.leftTop, floor(rect.width / dx).toInt + 1,
    floor(rect.height / dy).toInt + 1, dx, dy)

  override def partByIntersect[T](imgArea: ImgArea): (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) = {
    imgArea.bounds match {
      case Some(b) => partByIntersectFromBounds[T](b)
      case None => super.partByIntersect(imgArea)
    }
  }

  private def partByIntersectFromBounds[T](imgBounds: Bounds): (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) = {
    def addBlockIf(cond: => Boolean, curr: scala.collection.Seq[PtArea], lCol: Int, rCol: Int, tRow: Int, bRow: Int): scala.collection.Seq[PtArea] = if (cond) {
      val new_block = AxisGrid(leftTop + Point(lCol*dx, tRow*dy), rCol - lCol + 1, bRow - tRow + 1, dx, dy)
      curr :+ new_block
    } else {
      curr
    }

    // index of left splitting column - largest column of points in grid such that corresponding right edge
    // of axis grid where this column is the rightmost lies on the left of given image bounds
    // value -1 means that there is no such axis grid
    val l_col = max(floor((imgBounds.tl.x - leftTop.x) / dx - 1).toInt, -1)
    // index of right splitting column - smallest column of points in grid such that corresponding left edge
    // of axis grid where this column is the leftmost lies on the right of given image bounds
    val r_col = min(ceil((imgBounds.br.x - leftTop.x) / dx).toInt, nx)
    // index of top splitting row
    val t_row = max(floor((imgBounds.tl.y - leftTop.y) / dy - 1).toInt, -1)
    val b_row = min(ceil((imgBounds.br.y - leftTop.y) / dy).toInt, ny)
    if (l_col >= nx - 1 || r_col <= 0 || t_row >= ny - 1 || b_row <= 0) {
      // imgBounds outside axis grid - only outside part is not null
      partOutside
    } else {
      // Axis grid is partitioned into five potential axis grid areas - 4 belong to unknown area
      // (as parts of multi-part area) and 1 is an unknown area. Each of 4 outside axis grids could eventually not exist
      // according to position of unknown area bounds in axis grid, but always at least 1 will be exist.
      // Outside point area is a multi-part area containing axis grid blocks around unknown area. The block as ordered
      // as follow:
      // - top block - above unknown area spanning all axis grid (this) width
      // - left block - on the left of unknown area, left edge lies on the left edge of axis grid (this), right edge
      // on the edge of unknown area, height same as unknown area
      // - right block - on the right of unknown area with bounds analogous to these for left block
      // - bottom block - below unknown area spanning all axis grid (this) width
      lazy val out_blocks_creation = for {
        // possible adding of top block (above unknown area) - rectangle of full point area width above given imgBounds
        _ <- State[scala.collection.Seq[PtArea], Unit] { curr: scala.collection.Seq[PtArea] => (addBlockIf(t_row >= 0, curr, 0, nx - 1, 0,
          t_row), ()) }
        // possible adding of left block (on the left of unknown area) with height of unknown area
        res <- State[scala.collection.Seq[PtArea], Unit] { curr: scala.collection.Seq[PtArea] => (addBlockIf(l_col >= 0 && t_row + 1 <= b_row - 1, curr,
          0, l_col, t_row + 1, b_row - 1), ()) }
        // possible adding of right block (on the right of uknown area) with height of unknonw  area
        res <- State[scala.collection.Seq[PtArea], Unit] { curr: scala.collection.Seq[PtArea] => (addBlockIf(r_col <= nx && t_row + 1 <= b_row - 1, curr,
          r_col, nx - 1, t_row + 1, b_row - 1), ()) }
        // possible adding of bottom block (below unknown area) - rectangle of full point area width below given
        // imgBounds
        res <- State[scala.collection.Seq[PtArea], Unit] { curr: scala.collection.Seq[PtArea] => (addBlockIf(b_row < ny, curr, 0, nx - 1, b_row,
          ny - 1), ()) }
      } yield res

      val out_blocks: scala.collection.Seq[PtArea] = out_blocks_creation.runS(scala.collection.Seq[PtArea]()).value
      val out_area = MultiPartArea(out_blocks)
      // number of point columns of unknown area - and also of left and right block
      val unknown_cols =  r_col - l_col - 1
      // number of point rows of unknown area - and also of left and right block
      val unknown_rows = b_row - t_row - 1
      val unknown_area = AxisGrid(leftTop + Point((l_col + 1)*dx, (t_row + 1)*dy), unknown_cols, unknown_rows, dx, dy)

      val join_fun = (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => {
        // number of point columns of left block of outside area
        val left_cols = l_col + 1
        // number of points (possibly 0) in left block of outside area
        val left_pts_cnt = left_cols * unknown_rows
        // number of point columns of right block of outside area
        val right_cols = nx - r_col
        // number of points (possibly 0) in right block of outside area
        val right_pts_cnt = right_cols * unknown_rows
        // number of points (possibly 0) in top block of outside area
        val top_pts_cnt = nx * (t_row + 1)
        // number of points (possibly 0) in left block of outside area
        // part of outside seq belonging to top block
        val top_pts = outside.slice(0, top_pts_cnt)
        // part of outside seq belonging to bottom block
        val bottom_pts = outside.slice(top_pts_cnt + left_pts_cnt + right_pts_cnt, outside.size)
        // points belonging to left block (of outside area), unknown area and right block (of outside area) have to be
        // treated row by row - 3 corresponding rows (horizontal lines of points) are concatenated
        val middle_pts = (0 until unknown_rows) map { row: Int =>
          outside.slice(top_pts_cnt + row * left_cols, top_pts_cnt + (row + 1) * left_cols) ++
          unknown.slice(row * unknown_cols, (row + 1) * unknown_cols) ++
          outside.slice(top_pts_cnt + left_pts_cnt + row * right_cols, top_pts_cnt + left_pts_cnt + (row + 1) * right_cols)} reduce { _ ++ _ }

        top_pts ++ middle_pts ++ bottom_pts
      }

      (EmptyArea(), out_area, unknown_area, join_fun)
    }
  }
}
