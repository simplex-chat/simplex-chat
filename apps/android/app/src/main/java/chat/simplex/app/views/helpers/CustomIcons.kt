package chat.simplex.app.views.helpers

import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Bolt
import androidx.compose.material.icons.materialPath
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.vector.*
import androidx.compose.ui.unit.dp

private inline fun materialIcon(
  name: String,
  block: ImageVector.Builder.() -> ImageVector.Builder
): ImageVector = ImageVector.Builder(
  name = name,
  defaultWidth = MaterialIconDimension.dp,
  defaultHeight = MaterialIconDimension.dp,
  viewportWidth = MaterialIconDimension,
  viewportHeight = MaterialIconDimension
).block().build()

/**
 * Adds a vector path to this icon with Material defaults.
 *
 * @param fillAlpha fill alpha for this path
 * @param strokeAlpha stroke alpha for this path
 * @param pathFillType [PathFillType] for this path
 * @param pathBuilder builder lambda to add commands to this path
 */
private inline fun ImageVector.Builder.materialPath(
  fillAlpha: Float = 1f,
  strokeAlpha: Float = 1f,
  pathFillType: PathFillType = DefaultFillType,
  pathBuilder: PathBuilder.() -> Unit
) =
// TODO: b/146213225
// Some of these defaults are already set when parsing from XML, but do not currently exist
  // when added programmatically. We should unify these and simplify them where possible.
  path(
    fill = SolidColor(Color.Black),
    fillAlpha = fillAlpha,
    stroke = null,
    strokeAlpha = strokeAlpha,
    strokeLineWidth = 1f,
    strokeLineCap = StrokeCap.Butt,
    strokeLineJoin = StrokeJoin.Bevel,
    strokeLineMiter = 1f,
    pathFillType = pathFillType,
    pathBuilder = pathBuilder
  )

// All Material icons (currently) are 24dp by 24dp, with a viewport size of 24 by 24.
@PublishedApi
internal const val MaterialIconDimension = 24f


val AccountCircleFilled: ImageVector
  get() {
    Icons.Filled.Bolt
    if (_accountCircleFilled != null) {
      return _accountCircleFilled!!
    }
    _accountCircleFilled = materialIcon(name = "Filled.AccountCircle") {
      materialPath {
        moveTo(12.0f, 2.0f)
        curveTo(6.48f, 2.0f, 2.0f, 6.48f, 2.0f, 12.0f)
        reflectiveCurveToRelative(4.48f, 10.0f, 10.0f, 10.0f)
        reflectiveCurveToRelative(10.0f, -4.48f, 10.0f, -10.0f)
        reflectiveCurveTo(17.52f, 2.0f, 12.0f, 2.0f)
        close()
        moveTo(12.0f, 5.0f)
        curveToRelative(1.66f, 0.0f, 3.0f, 1.34f, 3.0f, 3.0f)
        reflectiveCurveToRelative(-1.34f, 3.0f, -3.0f, 3.0f)
        reflectiveCurveToRelative(-3.0f, -1.34f, -3.0f, -3.0f)
        reflectiveCurveToRelative(1.34f, -3.0f, 3.0f, -3.0f)
        close()
        moveTo(12.0f, 19.2f)
        curveToRelative(-2.5f, 0.0f, -4.71f, -1.28f, -6.0f, -3.22f)
        curveToRelative(0.03f, -1.99f, 4.0f, -3.08f, 6.0f, -3.08f)
        curveToRelative(1.99f, 0.0f, 5.97f, 1.09f, 6.0f, 3.08f)
        curveToRelative(-1.29f, 1.94f, -3.5f, 3.22f, -6.0f, 3.22f)
        close()
      }
    }
    return _accountCircleFilled!!
  }

private var _accountCircleFilled: ImageVector? = null

val SupervisedUserCircleFilled: ImageVector
  get() {
    if (_supervisedUserCircleFilled != null) {
      return _supervisedUserCircleFilled!!
    }
    _supervisedUserCircleFilled = materialIcon(name = "Filled.SupervisedUserCircle") {
      materialPath {
        moveTo(11.99f, 2.0f)
        curveToRelative(-5.52f, 0.0f, -10.0f, 4.48f, -10.0f, 10.0f)
        reflectiveCurveToRelative(4.48f, 10.0f, 10.0f, 10.0f)
        reflectiveCurveToRelative(10.0f, -4.48f, 10.0f, -10.0f)
        reflectiveCurveToRelative(-4.48f, -10.0f, -10.0f, -10.0f)
        close()
        moveTo(15.6f, 8.34f)
        curveToRelative(1.07f, 0.0f, 1.93f, 0.86f, 1.93f, 1.93f)
        curveToRelative(0.0f, 1.07f, -0.86f, 1.93f, -1.93f, 1.93f)
        curveToRelative(-1.07f, 0.0f, -1.93f, -0.86f, -1.93f, -1.93f)
        curveToRelative(-0.01f, -1.07f, 0.86f, -1.93f, 1.93f, -1.93f)
        close()
        moveTo(9.6f, 6.76f)
        curveToRelative(1.3f, 0.0f, 2.36f, 1.06f, 2.36f, 2.36f)
        curveToRelative(0.0f, 1.3f, -1.06f, 2.36f, -2.36f, 2.36f)
        reflectiveCurveToRelative(-2.36f, -1.06f, -2.36f, -2.36f)
        curveToRelative(0.0f, -1.31f, 1.05f, -2.36f, 2.36f, -2.36f)
        close()
        moveTo(9.6f, 15.89f)
        verticalLineToRelative(3.75f)
        curveToRelative(-2.4f, -0.75f, -4.3f, -2.6f, -5.14f, -4.96f)
        curveToRelative(1.05f, -1.12f, 3.67f, -1.69f, 5.14f, -1.69f)
        curveToRelative(0.53f, 0.0f, 1.2f, 0.08f, 1.9f, 0.22f)
        curveToRelative(-1.64f, 0.87f, -1.9f, 2.02f, -1.9f, 2.68f)
        close()
        moveTo(11.99f, 20.0f)
        curveToRelative(-0.27f, 0.0f, -0.53f, -0.01f, -0.79f, -0.04f)
        verticalLineToRelative(-4.07f)
        curveToRelative(0.0f, -1.42f, 2.94f, -2.13f, 4.4f, -2.13f)
        curveToRelative(1.07f, 0.0f, 2.92f, 0.39f, 3.84f, 1.15f)
        curveToRelative(-1.17f, 2.97f, -4.06f, 5.09f, -7.45f, 5.09f)
        close()
      }
    }
    return _supervisedUserCircleFilled!!
  }

private var _supervisedUserCircleFilled: ImageVector? = null

val BoltFilled: ImageVector
  get() {
    if (_boltFilled != null) {
      return _boltFilled!!
    }
    _boltFilled = materialIcon(name = "Filled.Bolt") {
      materialPath {
        moveTo(11.0f, 21.0f)
        horizontalLineToRelative(-1.0f)
        lineToRelative(1.0f, -7.0f)
        horizontalLineTo(7.5f)
        curveToRelative(-0.58f, 0.0f, -0.57f, -0.32f, -0.38f, -0.66f)
        curveToRelative(0.19f, -0.34f, 0.05f, -0.08f, 0.07f, -0.12f)
        curveTo(8.48f, 10.94f, 10.42f, 7.54f, 13.0f, 3.0f)
        horizontalLineToRelative(1.0f)
        lineToRelative(-1.0f, 7.0f)
        horizontalLineToRelative(3.5f)
        curveToRelative(0.49f, 0.0f, 0.56f, 0.33f, 0.47f, 0.51f)
        lineToRelative(-0.07f, 0.15f)
        curveTo(12.96f, 17.55f, 11.0f, 21.0f, 11.0f, 21.0f)
        close()
      }
    }
    return _boltFilled!!
  }

private var _boltFilled: ImageVector? = null