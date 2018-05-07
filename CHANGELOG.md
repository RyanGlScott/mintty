### next [????.??.??]
* Only use the `Win32`-provided version of `isMinTTY` if building against
  `Win32-2.5.3` to be certain that one avoids Trac #13431.
* Don't enable `Safe` on GHC 7.2.

### 0.1.1 [2017.03.17]
* Work around a serious bug on 32-bit Windows GHC that causes linker errors
  when mintty is used together with code that uses certain `msvcrt` functions,
  such as `atan`

## 0.1 [2017.01.30]
* Initial release
