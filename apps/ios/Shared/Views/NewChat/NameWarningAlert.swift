import Foundation
import SimpleXChat

enum NameWarningAction {
    case renew
    case register
    case reRegister
    case connectToTeam
}

struct NameWarningAlert {
    var title: String
    var message: String
    var action: NameWarningAction?
    var chat: ChatInfo?
}

private func namePrice(_ price: NamePrice) -> String {
    let dollars = "$\(price.amount / 100)" + (price.amount % 100 == 0 ? "" : String(format: ".%02lld", price.amount % 100))
    return String.localizedStringWithFormat(NSLocalizedString("%1$@ for %2$d years", comment: "name price"), dollars, price.years)
}

func nameWarningAlert(domain: SimplexDomain, warning: NameWarning, own: Bool, chat: ChatInfo?) -> NameWarningAlert {
    let nameStr = domain.fullDomainName
    func alert(_ title: String, _ message: String, action: NameWarningAction? = nil) -> NameWarningAlert {
        NameWarningAlert(title: title, message: message, action: action, chat: chat)
    }
    switch warning {
    case let .expired(expiredAt, graceUntil) where !own:
        let message = if let graceUntil {
            String.localizedStringWithFormat(NSLocalizedString("%1$@ expired on %2$@. Its owner can renew it until %3$@.", comment: "alert message"), nameStr, badgeDateText(expiredAt), badgeDateText(graceUntil))
        } else {
            String.localizedStringWithFormat(NSLocalizedString("%1$@ expired on %2$@.", comment: "alert message"), nameStr, badgeDateText(expiredAt))
        }
        return alert(NSLocalizedString("Name expired", comment: "alert title"), message)
    case let .expired(expiredAt, graceUntil):
        let message = if let graceUntil {
            String.localizedStringWithFormat(NSLocalizedString("Your name %1$@ expired on %2$@. Renew it before %3$@.", comment: "alert message"), nameStr, badgeDateText(expiredAt), badgeDateText(graceUntil))
        } else {
            String.localizedStringWithFormat(NSLocalizedString("Your name %1$@ expired on %2$@.", comment: "alert message"), nameStr, badgeDateText(expiredAt))
        }
        return alert(NSLocalizedString("Your name has expired", comment: "alert title"), message, action: .renew)
    case let .available(price) where !own:
        return alert(
            chat == nil ? NSLocalizedString("Name not registered", comment: "alert title") : NSLocalizedString("Name no longer registered", comment: "alert title"),
            String.localizedStringWithFormat(NSLocalizedString("%1$@ is available for registration for %2$@.", comment: "alert message"), nameStr, namePrice(price)),
            action: .register
        )
    case let .available(price):
        return alert(
            NSLocalizedString("Your name has expired", comment: "alert title"),
            String.localizedStringWithFormat(NSLocalizedString("Your name %1$@ is no longer registered. It is available for registration for %2$@.", comment: "alert message"), nameStr, namePrice(price)),
            action: .reRegister
        )
    case .reservedForCommunity:
        return alert(
            NSLocalizedString("Name not registered", comment: "alert title"),
            String.localizedStringWithFormat(NSLocalizedString("%@ is reserved for community. Please contact SimpleX Chat if you are interested in using it.", comment: "alert message"), nameStr),
            action: .connectToTeam
        )
    case .notRegistered:
        return alert(NSLocalizedString("Name not registered", comment: "alert title"), NSLocalizedString("This SimpleX name is not registered. Please check the name.", comment: ""))
    }
}

func planNameWarningAlert(_ planSimplexName: SimplexNameInfo?, _ plan: ConnectionPlan) -> NameWarningAlert? {
    if case let .some(.lapsed(nameWarning)) = plan.nameChange, let nameDomain = planSimplexName?.nameDomain {
        nameWarningAlert(domain: nameDomain, warning: nameWarning, own: plan.isOwnLink, chat: plan.planChat)
    } else {
        nil
    }
}
