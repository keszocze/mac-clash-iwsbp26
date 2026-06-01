# Supported variations
* inline full adder / module full adder
* Index-based counter / one-hot encoding-based counter
* rotation-based indexing
* Mealy machine based abstraction
* BitVector as the storage

# Todo

- [ ] Create function to generate the MAC from the config
  - [x] inline adder vs. module adder
  - [x] OneHotCounter vs. Index
  - [ ] rotating vs. indexing
  - [ ] state vs. Mealy
  - [ ] Vec vs BitVector
- [x] Enum für OneHotCounter (aber wieso wollte ich das überhaupt haben?)
- [ ] Testfälle bauen
  - [ ] hier gucken, wie/ob ich das statefull verwenden kann. das ist sicherlich lesbarer -> aber eventuell nicht so einfach
  - [X] exhaustive für kleine Werte
  - [X] random
  - [ ] Enum OneHotCounter
