# Description

The `miso-bem` library is to adapt the `bem` library for the `miso` library.

## Features

- to make *views* having Bem classes more easily

## Terms

- A **view** is a `miso` HTML element
whose `class_` attribute is set to a Bem class.
- An **element view** is a *view*
whose `class_` attribute contains a Bem element.
- A **block view** is an *element view*
whose `class_` attribute contains a Bem block.
- A **view generator** is a Bem generator into a *view*.
- A **mix view generator** is a *view generator* into a *block view*.
- A **context** is a value
having access to the model,
returning either a *block view*, a *mix view generator*,
or a `miso` HTML element,
and created using a *block view maker*.
- A **block view generator** is a *context* returning a *mix view generator*.
- A **view maker** is a function from both a `miso` HTML element
and its inner *views* into a *view generator*.
- A **block view maker** is a *view maker* into a *block view generator*.
- A **mix view maker** is a *view maker* into a *block view*.
- An **element view generator** is a *view generator* into an *element view*.
- An **element view maker** is a *view maker* into an *element view generator*.

## Notes

- All the examples are decorated
according to the `Default value` column
from [Table 1](https://github.com/monadosquito/bem#table-1).
- In all the examples the topmost BEM block is the `Root` Bem block.
- The *context* type is the `Reader` type constructor
applied to a model type and a `View` type constructor
applied to an action or a function type
whose rightmost type is the latter.
- The [`miso-bem-example`](https://github.com/monadosquito/miso-bem-example) repository is the working source code of all the examples
presented in this document.

## Stipulations

- Something
prefixed with the 'Bem' prefix
refers to a [`bem`](https://github.com/monadosquito/bem) library term.

# Installation

Follow [installing the `bem` library](https://github.com/monadosquito/bem#installation-flow)
but regarding the `miso-bem` library.

# Usage flow

1. Create a Bem scheme. ([?](https://github.com/monadosquito/bem#define-scheme))
2.
    -
        1. Create *element views* ([?](#create-element-view-using-default-decorations))
        2. and *block views*
        using the default decorations ([?](#create-block-view-using-default-decorations))
    - or configure the *views makers* ([?](#configure-view-makers))
        1. then create *element views* ([?](#create-element-view-using-custom-decorations))
        2. and *block views* using custom decorations.
        ([?](#create-block-view-using-custom-decorations))
3. Optionally, create a top-level *context* binding.
([?](#create-top-level-context))
4. Run the lop-level *context*. ([?](#run-top-level-context))

# Create element view using default decorations

Define an *element view generator*
1. obtaining needed data as arguments
2. and partially applying an appropriate [*element view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:elem) function
to both a `miso` HTML element
and its inner *element views* ([?](#include-element-view))
and *mix views* ([?](#include-mix-view))
including the obtained data.

## Hints

- The [*element view maker* type signatures](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:Elem-39-) can be used
as type signatures for *element view generators*.

# Create mix view using default decorations

Follow [creating an *element view*](#create-element-view)
but regarding a *mix view generator* and a [*mix view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:blkElem) function.

## Example

`src/View/Search.hs`
<!-- 'src/View/Search.hs' -->
```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module View.Search where


import Bem.Scheme

import Bem.Miso.View.Html
import Bem.Miso.View.Mk.Mk
import Bem.Utl.Intr
import Miso

import Bem.Init

import Bem.Miso.View.Mk.Cfg


search :: FromBlkElem (View ())
search
    =
    _blkElem mks (NonVoidHtmlElem section_)
        ( []
        , ([ blkElem (VoidHtmlElem input_)
                 [placeholder_ "Text to search"]
                 TextInput
                 [TextInput_Dark]
                 Search
                 Search_TextInput
                 [SearchTextInput_Size Big]
           , blkElem (VoidHtmlElem input_)
                 [type_ "button", value_ "Search"]
                 Btn
                 [Btn_Dark]
                 Search
                 Search_Btn
                 [SearchBtn_Size Big]
           ]
          )
        )
```

## Hints

- The [*mix view maker* type signatures](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:BlkElem-39-) can be used
as type signatures for *mix view generators*.
- If an *element view generator* is also needed to have a BEM block,
create an equivalent *mix view generator*
using a [*mix view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:blkElem) instead.
- If an *element view generator* is too complex or used in multiple places,
then it should be a top-level binding.

# Create block view using default decorations

Define a *context*,
1. obtaining needed data
2. and returning the *block view generator*
partially applying an appropriate [*block view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:mkBlkElem)
to both a `miso` HTML element including its inner *views*
and the obtained data,

from it.

## Example

`src/View/Header.hs`
<!-- 'src/View/Header.hs' -->
```hs
{-# LANGUAGE OverloadedStrings #-}


module View.Header where


import Bem.Scheme
import View.Search

import Bem.Miso.View.Html
import Control.Monad.Reader
import Miso
import Miso.String
import Bem.Miso.Utl.Utl

import Bem.Init


mkHeader :: Reader MisoString (BlkNoModsElem ())
mkHeader = do
    userName <- ask
    _mkBlkNoModsElem mks (NonVoidHtmlElem header_)
        ( []
        , [ noModsBlkNoModsElem (NonVoidHtmlElem span_)
                ([], [text "Logo"])
                Logo
                Root
                Root_Logo
          , span_ [] [text userName]
          , search Search [Search_Dark] Header Header_Search []
          ]
        )
```

## Hints

- The [*block view generator* type aliases](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:MkBlkElem-39-) can be used
as type signatures for *block view generators*.
- The partial [*view makers*](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#v:blkNoModsElem) can be used
in place
of the [regular ones](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:elem)
to omit corresponding needless modifiers.

# Configure view makers

Follow [configuring the Bem class generators](https://github.com/monadosquito/bem#configure-class-generators)
but regarding the `miso-bem` [`init`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#v:init) function
and [`Mks`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#t:Mks) record.

# Create element view using custom decorations

Follow [creating an *element view* using the default decorations](#create-element-view-using-default-decorations)
but regarding a configured ([?](#configure-view-makers)) [*element view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#t:Mks) field-function.

# Create mix view using custom decorations

Follow [creating an *element view* using the default decorations](#create-element-view-using-default-decorations)
but regarding a *mix view generator*
and a configured ([?](#configure-view-makers)) [*mix view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#t:Mks) field-function.

## Example

`src/View/Search.hs`
<!-- 'src/View/Search.hs' -->
```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module View.Search where


import Bem.Scheme

import Bem.Miso.View.Html
import Bem.Miso.View.Mk.Mk
import Bem.Utl.Intr
import Miso

import Bem.Init

import Bem.Miso.View.Mk.Cfg


search :: FromBlkElem (View ())
search
    =
    _blkElem mks (NonVoidHtmlElem section_)
        ( []
        , ([ blkElem (VoidHtmlElem input_)
                 [placeholder_ "Text to search"]
                 TextInput
                 [TextInput_Dark]
                 Search
                 Search_TextInput
                 [SearchTextInput_Size Big]
           , blkElem (VoidHtmlElem input_)
                 [type_ "button", value_ "Search"]
                 Btn
                 [Btn_Dark]
                 Search
                 Search_Btn
                 [SearchBtn_Size Big]
           ]
          )
        )
```

# Create block view using custom decorations

Follow [creating a *block view* using the default decorations](#create-block-view-using-default-decorations)
but regarding a configured [*block view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Cfg.html#t:Mks) field-function.

## Example

`src/View/Header.hs`
<!-- 'src/View/Header.hs' -->
```hs
{-# LANGUAGE OverloadedStrings #-}


module View.Header where


import Bem.Scheme
import View.Search

import Bem.Miso.View.Html
import Control.Monad.Reader
import Miso
import Miso.String
import Bem.Miso.Utl.Utl

import Bem.Init


mkHeader :: Reader MisoString (BlkNoModsElem ())
mkHeader = do
    userName <- ask
    _mkBlkNoModsElem mks (NonVoidHtmlElem header_)
        ( []
        , [ noModsBlkNoModsElem (NonVoidHtmlElem span_)
                ([], [text "Logo"])
                Logo
                Root
                Root_Logo
          , span_ [] [text userName]
          , search Search [Search_Dark] Header Header_Search []
          ]
        )
```

## Notes

- In order that a `miso` HTML element can be passed into a *view maker*,
wrap it into the [`HtmlElem`](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Html.html#t:HtmlElem) GADT,
namely,
    - a non-void one into the `NonVoidHtmlElem` data constructor
    - and a void one into the `VoidHtmlElem` data constructor.
- In order that the `Mks` field-functions using custom decorations can be used,
they must be configured. ([?](#configure-view-makers))

## Hints

- The partial [*view makers*](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:Mks) can be used
in place
of the [full ones](https://monadosquito.github.io/miso-bem/Bem-Miso-Utl-Utl.html#t:Mks)
to omit corresponding needless modifiers.

# Create top-level context

1. Bind a name to a *context*
returning either a `miso` HTML element or a *block view*.
2. Include each top-level *block view* into the `miso` HTML element
that it returns. ([?](#include-block-view))
3. Set the resulting `miso` HTML element
as a value of the `view` field of the `App` record.

## Example

`src/View/Root.hs`
<!-- 'src/View/Root.hs' -->
```hs
{-# LANGUAGE OverloadedStrings #-}


module View.Root where


import Bem.Scheme
import View.Header

import Bem.Miso.Utl.Utl
import Control.Monad.Reader
import Miso
import Miso.String


mkRoot :: Reader MisoString (View ())
mkRoot = do
    BlkNoModsElem header <- mkHeader
    return
        $ div_
              [class_ "Root"]
              [header Header [Header_Dark] Root Root_Header]
```

## Hints

- The topmost BEM block should be invisible HTML element.
- If a top-level *context* returns a `miso` HTML element
having a topmost BEM block,
then global styles can be set on the topmost BEM block.
- In order that the BEM rules are obeyed,
the topmost BEM block should be used as a parent for top-level BEM elements

# Include block view

1. Run an included *block view generator* in an including *context*
to obtain the wrapped *block view generator* using `do` notation.
2. Unwrap the obtained wrapped *block view generator*
using pattern matching
over an appropriate [wrapper data constructor](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#t:BlkElem).
3. Bind a name to the unwrapped *block view generator*
inside the mattern match.
4. Follow
    - [Bem classes generating using the default decorations](https://github.com/monadosquito/bem#generate-classes-using-default-decorations)
    - or [Bem classes generating using custom decorations](https://github.com/monadosquito/bem#generate-classes-using-custom-decorations)
but regarding the bound *block view generator* to obtain a *block view*.
5. Pass the obtained *block view*
into the [*block view maker*](https://monadosquito.github.io/miso-bem/Bem-Miso-View-Mk-Mk.html#v:mkBlkElem)
used
to create the returned *view block generator*.

# Include element view

Follow [including *block view*](#include-block-view)
but from the fourth step and regarding an *element view generator*.

# Include mix view

Follow [including *element view*](#include-element-view)
but regarding *mix view generators*.

# Run top-level context

1. Run a lop-level *context* applying the `runReader` function to it
to obtain a `miso` HTML element.
2. Set the obtained `miso` HTML element
as a value of the `view` field of the `App` record.

## Example

`src/Main.hs`
<!-- 'src/Main.hs' -->
```hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}


import View.Root

import Control.Monad.Reader
import Language.Javascript.JSaddle.Warp
import Miso


main :: IO ()
main
    =
    runApp
        $ startApp
              App { events = defaultEvents
                  , initialAction = ()
                  , logLevel = Off
                  , model = "Monadosquito"
                  , mountPoint = Nothing
                  , subs = []
                  , update = \_ mdl -> noEff mdl
                  , view = runReader mkRoot
                  }


#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp = debug 8000
#else
runApp :: IO () -> IO ()
runApp = id
#endif
```

## Notes

- The example runs a local HTTP server
serving the following HTML code on the `8000` port:

`result.html`
<!-- 'result.html' -->
```html
<html>
    <head>
    </head>
    <body>
        <script src="/jsaddle.js"></script>
        <div class="Root">
            <header class="header header_dark root__header">
                <span class="logo root__logo">Logo</span>
                <span>Monadosquito</span>
                <section class="search search_dark header__search">
                    <input class="text-input text-input_dark search__text-input search__text-input_size_big" placeholder="Text to search">
                    <input class="btn btn_dark search__btn search__btn_size_big" type="button" value="Search">
                </section>
            </header>
        </div>
    </body>
</html>
```

## Hints

- If a name is bound to a top-level *context*,
then use it as a top-level *context* by its name.

# Contributing

This library can be contributed into
the [way that the `bem` library can](https://github.com/monadosquito/bem#contributing).

## Convention

This library follows the [convention](https://github.com/monadosquito/bem#convention)
followed by the [`bem` library](https://github.com/monadosquito/bem).

## Defined scopes

- cfg
- utl
- view
