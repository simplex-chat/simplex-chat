package chat.simplex.common.model

/**
 * Paying for a name.
 *
 * This is the seam where the app store goes. The real implementation asks
 * StoreKit (iOS) or Play Billing (Android) to charge for a product and hands
 * back the receipt; the core forwards that receipt to the names service, which
 * validates it against Apple's or Google's server API before registering
 * anything. Nothing outside this file interprets a payment.
 *
 * Until billing is integrated, [purchase] returns a development token that the
 * mock service accepts. Replacing this one function with a real billing call is
 * the whole of the client-side work — the command, the request shape and the
 * service interface already carry a payment proof.
 */
object NamePayment {
  /** Product identifiers, mirroring what would be configured in the stores. */
  fun productId(years: Int): String = "chat.simplex.name.${years}y"

  data class Result(val token: String)

  data class Failure(val reason: String)

  /**
   * Charge for [years] of a name and return a proof of payment.
   *
   * Returns null if the user cancelled or the charge failed. A real
   * implementation must be idempotent: a receipt is consumed once by the
   * service, and a retry after a network failure must not charge twice.
   */
  suspend fun purchase(years: Int): Result? {
    // DEVELOPMENT ONLY: no charge is made and nothing is verified.
    // The mock names service accepts any token; the real one will not.
    return Result("dev-mock-payment-${productId(years)}")
  }

  /** Whether real billing is wired up. False means purchases are simulated. */
  val isLive: Boolean = false
}
