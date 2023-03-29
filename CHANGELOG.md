# [Unreleased]

## Added

- The [`MkSingleton'`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:MkSingleton-39-) type synonym
can be used
to type-annotate a singleton view.
- The utility [`Mks`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:Mks) record and [`init`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:init) function
can be used in place of the regular ones
in order that the partial configured view generators are available
inside the former.
- The [`BlkElem'`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:BlkElem-39-), [`Elem'`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:Elem-39-), and [`MkBlkElem'`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:MkBlkElem-39-) type synonyms
can be used to avoid corresponding view generator types spelling.
- The view Bem class decorations can be configured
by applying the [`init`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#v:init) function
to an appropriate [`Cfg`](https://monadosquito.github.io/bem/Bem-Cfg-Cfg.html#t:Cfg) record
and assigning the resulting [`Mks`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#t:Mks) record
of configured view generators a name
in order that the view generators can be referenced
with it.

# [1.0.0] - 2023-07-01

## Added

- The [`blkNoModsElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:blkNoModsElem),
[`mkBlkNoModsElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:mkBlkNoModsElem),
[`mkNoModsBlkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:mkNoModsBlkElem),
[`mkNoModsBlkNoModsElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:mkNoModsBlkNoModsElem),
[`noModsBlkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:noModsBlkElem),
[`noModsBlkNoModsElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:noModsBlkNoModsElem),
and [`noModsElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:noModsElem)
partial view generators
can be used
in place
of
the regular [`blkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:blkElem),
[`elem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:elem),
and [`mkBlkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:mkBlkElem)
ones
to omit corresponding needless modifiers.
- Views
having Bem classes can be made
using
the [`blkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:blkElem),
[`elem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:elem),
and [`mkBlkElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:mkBlkElem) functions.
