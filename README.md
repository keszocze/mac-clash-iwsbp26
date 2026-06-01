# Supported variations
* inline full adder / module full adder
* Index-based counter / one-hot encoding-based counter
* rotation-based indexing
* Mealy machine based abstraction
* BitVector as the storage

# Todo

- [ ] Create function to generate the MAC from the config
  - [x] works for the adder
  - [ ] das Config Objekt sollte dann in ein spezielleres config objekt (was dann funktionen enthält) überführt werden
- [ ] Testfälle bauen
  - [ ] hier gucken, wie/ob ich das statefull verwenden kann. das ist sicherlich lesbarer -> aber eventuell nicht so einfach
  - [X] exhaustive für kleine Werte
  - [X] random
