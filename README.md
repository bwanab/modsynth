# modsynth

A modular synth system somewhat along the lines of PD, but at a higher level of abstraction. A closer analogy is Alsa Modular Synth orignally built by Matthias Nagorni.

## Usage

```clj
(use 'modsynth.core)
(-main)
```

## Roadmap

### What it still needs before it's usable:
1. Save/restore dialog
* save/restore needs to be better tested.
* Save values for sliders and const.
* Fix slider.
* A disconnect method.
* splitter and mixer nodes for audio and control.
* More synth types:
** Random note and timing
** some kind of file input for compositions if there is an overtone standard use that.

## License

Copyright © 2013 Bill Allen

Distributed under the Eclipse Public License either version 1.0.
