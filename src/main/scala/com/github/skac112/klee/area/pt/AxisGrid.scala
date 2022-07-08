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
  * actual range of points. This solution made tiling easier.
  * @param leftTop
  * @param nx
  * @param ny
  * @param dx
  * @param dy
  */
case class AxisGrid(leftTop: Point, nx: Int, ny: Int, dx: Double, dy: Double) extends PtArea {
  override lazy val points = {
    // coordinates of left top point (with offset from leftTop)
    val real_lt = leftTop + Point(.5*dx, .5*dy)
    (0 until nx*ny) map {i: Int => real_lt + Point((i % nx) * dx, (i / nx) * dy)}
  }

  override lazy val area = AxisRect(leftTop, (nx + 1) * dx, (ny + 1) * dy)

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
    val r_col = min(ceil((imgBounds.br.x - leftTop.x) / dx - 1).toInt, nx)
    // index of top splitting row
    val t_row = max(floor((imgBounds.tl.y - leftTop.y) / dy - 1).toInt, -1)
    val b_row = min(ceil((imgBounds.br.y - leftTop.y) / dy - 1).toInt, ny)
    if (l_col >= nx - 1 || r_col <= 0 || t_row >= ny - 1 || b_row <= 0) {
      // imgBounds outside axis grid
      partOutside
    } else {
      // outside point area is a multi part area containing axis grid blocks around unknown area.
      // unknown area is also an axis grid
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
      val unknown_area = AxisGrid(leftTop + Point((l_col + 1)*dx, (t_row + 1)*dy), r_col - l_col - 1, b_row - t_row - 1,
        dx, dy)
      val top_pts_cnt = nx * (t_row + 1)
      val left_pts_cnt = (l_col + 1) * (b_row - t_row - 1)

      val join_fun = (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => {
        // number of points of top, left, right and bottom parts of outside area points
        val top_left_pts = outside.slice(0, top_pts_cnt + left_pts_cnt)
        val right_bottom_pts = outside.slice(top_pts_cnt + left_pts_cnt, outside.size)
        top_left_pts ++ unknown ++ right_bottom_pts
      }

      (EmptyArea(), out_area, unknown_area, join_fun)
    }
  }
}
