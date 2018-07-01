{-# LANGUAGE OverloadedStrings #-}
{-|
 -
Module      : Reflex.Dom.Tags
Description : HTML tags
Copyright   : (c) Robert Klotzner, 2018

If you want an EDSL for rendering HTML with a little less clutter, you can import this module instead of "Reflex.Dom.Core".

All HTML 5 tags are available in two variants, div as example:

div'  .... Corresponds to el "div", elClass "div", elAttr "div", elDynClass "div" and elDynAttr "div"
div'' .... Corresponds to el' "div", elClass' "div", elAttr' "div", elDynClass' "div" and elDynAttr' "div"

Note: This module has a conflicting variant of (=:), which is specialized to
`Map Text Text` instead of a generic `Map a b`. This means you should not
import "Reflex.Dom.Tags" and "Reflex.Dom.Core" simultaneously. Most definitions
from Reflex.Dom.Core are re-exported by this module anyway.

-}
module Reflex.Dom.Tags 
  ( -- * Types
    Cls (..)
  , AttrMap
  , (Reflex.Dom.Tags.Internal.=:)
    -- * HTML Tags
  , module Reflex.Dom.Tags
    -- * Re-export for Reflex.Dom.Core primitives
  , module Primitive
  ) where

import Reflex.Dom.Tags.Internal
import Reflex.Dom.Core as Primitive hiding ((=:), tag)

a' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
a' = tag "a"

a'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
a'' = tag' "a"

abbr' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
abbr' = tag "abbr"

abbr'' :: (DomBuilder t m, ReflexTag arg t m a) =>arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
abbr'' = tag' "abbr"

address' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
address' = tag "address"

address'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
address'' = tag' "address"

area' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
area' = tag "area"

area'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
area'' = tag' "area"

article' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
article' = tag "article"

article'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
article'' = tag' "article"

aside' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
aside' = tag "aside"

aside'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
aside'' = tag' "aside"

audio' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
audio' = tag "audio"

audio'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
audio'' = tag' "audio"

b' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
b' = tag "b"

b'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
b'' = tag' "b"

base' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
base' = tag "base"

base'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
base'' = tag' "base"


bdi' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
bdi' = tag "bdi"

bdi'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
bdi'' = tag' "bdi"

bdo' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
bdo' = tag "bdo"

bdo'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
bdo'' = tag' "bdo"

blockquote' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
blockquote' = tag "blockquote"

blockquote'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
blockquote'' = tag' "blockquote"

body' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
body' = tag "body"

body'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
body'' = tag' "body"

br' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
br' = tag "br"

br'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
br'' = tag' "br"

button' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
button' = tag "button"

button'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
button'' = tag' "button"

canvas' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
canvas' = tag "canvas"

canvas'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
canvas'' = tag' "canvas"

caption' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
caption' = tag "caption"

caption'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
caption'' = tag' "caption"

cite' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
cite' = tag "cite"

cite'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
cite'' = tag' "cite"

code' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
code' = tag "code"

code'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
code'' = tag' "code"

col' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
col' = tag "col"

col'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
col'' = tag' "col"

colgroup' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
colgroup' = tag "colgroup"

colgroup'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
colgroup'' = tag' "colgroup"

data' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
data' = tag "data"

data'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
data'' = tag' "data"

datalist' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
datalist' = tag "datalist"

datalist'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
datalist'' = tag' "datalist"

dd' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
dd' = tag "dd"

dd'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
dd'' = tag' "dd"

del' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
del' = tag "del"

del'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
del'' = tag' "del"

details' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
details' = tag "details"

details'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
details'' = tag' "details"

dfn' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
dfn' = tag "dfn"

dfn'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
dfn'' = tag' "dfn"

dialog' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
dialog' = tag "dialog"

dialog'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
dialog'' = tag' "dialog"

div' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
div' = tag "div"

div'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
div'' = tag' "div"

dl' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
dl' = tag "dl"

dl'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
dl'' = tag' "dl"

dt' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
dt' = tag "dt"

dt'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
dt'' = tag' "dt"

em' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
em' = tag "em"

em'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
em'' = tag' "em"

embed' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
embed' = tag "embed"

embed'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
embed'' = tag' "embed"

fieldset' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
fieldset' = tag "fieldset"

fieldset'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
fieldset'' = tag' "fieldset"

figcaption' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
figcaption' = tag "figcaption"

figcaption'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
figcaption'' = tag' "figcaption"

figure' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
figure' = tag "figure"

figure'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
figure'' = tag' "figure"

footer' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
footer' = tag "footer"

footer'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
footer'' = tag' "footer"

form' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
form' = tag "form"

form'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
form'' = tag' "form"

h1' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h1' = tag "h1"

h1'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h1'' = tag' "h1"

h2' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h2' = tag "h2"

h2'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h2'' = tag' "h2"

h3' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h3' = tag "h3"

h3'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h3'' = tag' "h3"

h4' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h4' = tag "h4"

h4'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h4'' = tag' "h4"

h5' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h5' = tag "h5"

h5'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h5'' = tag' "h5"

h6' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
h6' = tag "h6"

h6'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
h6'' = tag' "h6"

head' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
head' = tag "head"

head'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
head'' = tag' "head"

header' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
header' = tag "header"

header'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
header'' = tag' "header"

hgroup' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
hgroup' = tag "hgroup"

hgroup'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
hgroup'' = tag' "hgroup"

hr' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
hr' = tag "hr"

hr'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
hr'' = tag' "hr"

html' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
html' = tag "html"

html'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
html'' = tag' "html"

i' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
i' = tag "i"

i'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
i'' = tag' "i"

iframe' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
iframe' = tag "iframe"

iframe'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
iframe'' = tag' "iframe"

img' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
img' = tag "img"

img'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
img'' = tag' "img"

input' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
input' = tag "input"

input'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
input'' = tag' "input"

ins' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
ins' = tag "ins"

ins'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ins'' = tag' "ins"

kbd' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
kbd' = tag "kbd"

kbd'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
kbd'' = tag' "kbd"

keygen' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
keygen' = tag "keygen"

keygen'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
keygen'' = tag' "keygen"

label' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
label' = tag "label"

label'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
label'' = tag' "label"

legend' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
legend' = tag "legend"

legend'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
legend'' = tag' "legend"

li' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
li' = tag "li"

li'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
li'' = tag' "li"

link' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
link' = tag "link"

link'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
link'' = tag' "link"

main' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
main' = tag "main"

main'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
main'' = tag' "main"

map' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
map' = tag "map"

map'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
map'' = tag' "map"

mark' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
mark' = tag "mark"

mark'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
mark'' = tag' "mark"

menu' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
menu' = tag "menu"

menu'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
menu'' = tag' "menu"

menuitem' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
menuitem' = tag "menuitem"

menuitem'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
menuitem'' = tag' "menuitem"

meta' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
meta' = tag "meta"

meta'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
meta'' = tag' "meta"

meter' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
meter' = tag "meter"

meter'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
meter'' = tag' "meter"

nav' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
nav' = tag "nav"

nav'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
nav'' = tag' "nav"

noscript' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
noscript' = tag "noscript"

noscript'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
noscript'' = tag' "noscript"

object' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
object' = tag "object"

object'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
object'' = tag' "object"

ol' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
ol' = tag "ol"

ol'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ol'' = tag' "ol"

optgroup' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
optgroup' = tag "optgroup"

optgroup'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
optgroup'' = tag' "optgroup"

option' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
option' = tag "option"

option'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
option'' = tag' "option"

output' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
output' = tag "output"

output'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
output'' = tag' "output"

p' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
p' = tag "p"

p'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
p'' = tag' "p"

param' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
param' = tag "param"

param'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
param'' = tag' "param"

pre' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
pre' = tag "pre"

pre'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
pre'' = tag' "pre"

progress' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
progress' = tag "progress"

progress'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
progress'' = tag' "progress"

q' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
q' = tag "q"

q'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
q'' = tag' "q"

rb' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
rb' = tag "rb"

rb'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
rb'' = tag' "rb"

rp' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
rp' = tag "rp"

rp'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
rp'' = tag' "rp"

rt' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
rt' = tag "rt"

rt'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
rt'' = tag' "rt"

rtc' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
rtc' = tag "rtc"

rtc'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
rtc'' = tag' "rtc"

ruby' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
ruby' = tag "ruby"

ruby'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ruby'' = tag' "ruby"

s' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
s' = tag "s"

s'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
s'' = tag' "s"

samp' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
samp' = tag "samp"

samp'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
samp'' = tag' "samp"

script' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
script' = tag "script"

script'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
script'' = tag' "script"

section' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
section' = tag "section"

section'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
section'' = tag' "section"

select' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
select' = tag "select"

select'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
select'' = tag' "select"

small' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
small' = tag "small"

small'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
small'' = tag' "small"

source' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
source' = tag "source"

source'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
source'' = tag' "source"

span' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
span' = tag "span"

span'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
span'' = tag' "span"

strong' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
strong' = tag "strong"

strong'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
strong'' = tag' "strong"

style' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
style' = tag "style"

style'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
style'' = tag' "style"

sub' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
sub' = tag "sub"

sub'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
sub'' = tag' "sub"

summary' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
summary' = tag "summary"

summary'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
summary'' = tag' "summary"

sup' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
sup' = tag "sup"

sup'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
sup'' = tag' "sup"

table' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
table' = tag "table"

table'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
table'' = tag' "table"

tbody' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
tbody' = tag "tbody"

tbody'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
tbody'' = tag' "tbody"

td' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
td' = tag "td"

td'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
td'' = tag' "td"

template' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
template' = tag "template"

template'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
template'' = tag' "template"

textarea' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
textarea' = tag "textarea"

textarea'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
textarea'' = tag' "textarea"

tfoot' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
tfoot' = tag "tfoot"

tfoot'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
tfoot'' = tag' "tfoot"

th' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
th' = tag "th"

th'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
th'' = tag' "th"

thead' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
thead' = tag "thead"

thead'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
thead'' = tag' "thead"

time' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
time' = tag "time"

time'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
time'' = tag' "time"

title' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
title' = tag "title"

title'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
title'' = tag' "title"

tr' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
tr' = tag "tr"

tr'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
tr'' = tag' "tr"

track' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
track' = tag "track"

track'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
track'' = tag' "track"

u' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
u' = tag "u"

u'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
u'' = tag' "u"

ul' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
ul' = tag "ul"

ul'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
ul'' = tag' "ul"

var' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
var' = tag "var"

var'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
var'' = tag' "var"

video' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
video' = tag "video"

video'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
video'' = tag' "video"

wbr' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m a
wbr' = tag "wbr"

wbr'' :: (DomBuilder t m, ReflexTag arg t m a) => arg -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
wbr'' = tag' "wbr"
