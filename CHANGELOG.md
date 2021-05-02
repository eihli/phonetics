# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.1.3] - 2021-05-02
### Fixed
- Fixed bug sylalbifying words that begin with consonants that don't adhere to sonority heirarchy.
  - "Steel", for example. "T" is less sonorous than "S" and typically wouldn't be included in an onset, but since there are no vowels preceding the "ST" then both *should* be included in the onset.

## [0.1.2] - 2021-04-22
### Fixed
- Fixed bug when getting phones from CMULexicon because the word wasn't found in the CMU dictionary. (Missing parens)
- Comment out warn-on-reflection code that was just being used to find performance gains.

## 0.1.1
### Added

Initial release

- Phonetics and syllabification utilities 

[Unreleased]: https://github.com/com.owoga/phonetics/compare/0.1.2...HEAD
[0.1.1]: https://github.com/com.owoga/phonetics/compare/0.1.1...0.1.2
