package chat.simplex.app

import chat.simplex.common.views.database.mtrErrorDescription
import chat.simplex.common.views.helpers.MTRError
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class SettingsDatabaseErrorUnitTest {

  @Test
  fun noDownMigrationNamesTheMigration() {
    assertEquals(
      String.format(generalGetString(MR.strings.mtr_error_no_down_migration), "20261001_new_table"),
      mtrErrorDescription(MTRError.NoDown(listOf("20261001_new_table")))
    )
  }

  @Test
  fun noDownMigrationsAreCommaJoined() {
    val text = mtrErrorDescription(MTRError.NoDown(listOf("m1", "m2", "m3")))
    assertEquals(String.format(generalGetString(MR.strings.mtr_error_no_down_migration), "m1, m2, m3"), text)
    assertTrue(text.contains("m1, m2, m3"), text)
  }

  @Test
  fun differentMigrationNamesAppThenDatabase() {
    val text = mtrErrorDescription(MTRError.Different(appMigration = "app_migration", dbMigration = "db_migration"))
    assertEquals(String.format(generalGetString(MR.strings.mtr_error_different), "app_migration", "db_migration"), text)
    assertTrue(text.contains("app_migration") && text.contains("db_migration"), text)
  }
}
