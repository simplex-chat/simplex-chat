# Deduplicate contact requests

1. add nullable fields `via_contact_uri_hash` and `xcontact_id` to `connections`
2. when joining (Connect -> SCMContact)
    - generate and save random `xcontact_id`
    - save hash of `AConnectionRequestUri` when joining via contact uri
      (AConnectionRequestUri -> ConnectionRequestUri -> CRContactUri)
    - send random identifier in `XContact` as `Maybe XContactId`
    - check for repeat join - if connection with such `via_contact_uri_hash` has contact notify user
    - check for repeat join - check in connections if such contact uri exists, if yes use same identifier; the rest of request can (should) be regenerated, e.g. new server, profile
      can be required
3. add nullable field `xcontact_id` to `contact_requests` and to `contacts` (* for auto-acceptance)
4. on contact request (processUserContactRequest)
    - save identifier
    - \* check if `xcontact_id` is in `contacts` - then notify this contact already exists
    - when saving check if contact request with such identifier exists, if yes update `contact_request`
      (`invId`, new profile)
    - ? remove old invitation - probably not necessarily, to be done in scope of connection expiration
    - return from Store whether request is new or updated (Bool?), new chat response for update or same response
