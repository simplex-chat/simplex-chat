package chat.simplex.app

import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import java.lang.reflect.Modifier
import kotlin.test.Test
import kotlin.test.assertTrue

class UiKeyTagsTest {
  @Test
  fun constantUiKeyTagsAreUnique() {
    val keys = mutableListOf<Pair<String, UiKey>>()
    fun collect(cls: Class<*>, path: String) {
      for (f in cls.declaredFields) {
        if (f.type != UiKey::class.java || !Modifier.isStatic(f.modifiers)) continue
        f.isAccessible = true
        keys += "$path.${f.name}" to f.get(null) as UiKey
      }
      cls.declaredClasses.forEach { collect(it, "$path.${it.simpleName}") }
    }
    collect(Ui::class.java, "Ui")
    assertTrue(keys.size > 100, "found only ${keys.size} constant keys in Ui")
    val shared = keys.groupBy({ it.second.tag }, { it.first }).filterValues { it.size > 1 }
    assertTrue(shared.isEmpty(), "constant UI keys with the same tag: " + shared.entries.joinToString { (tag, names) -> "$tag: ${names.joinToString()}" })
  }
}
