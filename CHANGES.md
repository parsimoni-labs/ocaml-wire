## unreleased

- Fix C stub generator: use the EverParse-normalised `<Name>Fields`
  type name. Schemas with a name segment of 2+ leading capitals
  (e.g. `IPv4`, `EP_Header`, `MC_Status_Reply`) previously emitted C
  that referenced an undefined struct.

## 0.9.0

Initial release.
