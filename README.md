# Nenuvar LilyPond Framework

Framework used by `Edition Nicolas Sceaux' scores.

add this directory to LilyPond include path, then use:

```
\include "nenuvar-lib.ily"
```

## Patches to LilyPond source files

See `lilypond-scm.patch`

## Font installation

In file `~/.config/fontconfig/conf.d/10-lilypond-fonts.conf`
(adapting nenuvar-lib path accordingly):

```
<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
  <dir>~/Documents/LilyPond/nenuvar-lib/fonts/</dir>
<fontconfig>
```

then:
```
$ fc-cache
```
