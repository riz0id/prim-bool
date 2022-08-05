# Revision history for unlifted-bool

## 0.1.1.1 -- 2022-08-22

* Added module `Data.Bool.Unlifted.Core`, a core module defining the `Bool#` type.
* Added instance `Lift Bool#`.
* Added instance `Typeable Bool#`.
* Added `toBool#`, a conversion from `Bool#` to `Bool`.

### Added `Bool#` Comparisons

Added `eq#`, `ne#`, `lt#`, `le#`, `gt#`, and `ge#` comparisons for `Bool`.

### Removed `Int#`, `Word#`, `Double#`, and `Float#` Comparisons 

Removed all comparisons not relating to the `Bool#` type. These comparisons will be redefined in new packages specific to their types.

### Name Changes
* Renamed `fromBool` to `fromBool#`.
* Renamed `andB#` to `and#`.
* Renamed `orB#` to `or#`.
* Renamed `xor#` to `xor#`.
* Renamed `notB#` to `not#`.
* Renamed `intToBool#` to `fromInt#`.
* Renamed `boolToInt#` to `toInt#`.

### Fixity Changes
* Change fixity for logical `andB#` from `4` to `7`.
* Change fixity for logical `orB#` from `4` to `5`.
* Added fixity of `5` for logical `xorB#`.

## 0.1.1.0 -- 2022-06-05

* Removes the `Int#` comparisons `ltInt#`, `leInt#`, `eqInt#`, `neInt#`, `gtInt#`, and `geInt#`.
* Removes the `Int8#` comparisons `ltInt8#`, `leInt8#`, `eqInt8#`, `neInt8#`, `gtInt8#`, and `geInt8#`.
* Removes the `Int16#` comparisons `ltInt16#`, `leInt16#`, `eqInt16#`, `neInt16#`, `gtInt16#`, and `geInt16#`.
* Removes the `Int32#` comparisons `ltInt32#`, `leInt32#`, `eqInt32#`, `neInt32#`, `gtInt32#`, and `geInt32#`.

## 1.0.0 -- 2022-03-11

* First version. Released on an unsuspecting world.

