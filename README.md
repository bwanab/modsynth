# modsynth

A modular synth system somewhat along the lines of PD, but at a higher level of abstraction. A closer analogy is Alsa Modular Synth orignally built by Matthias Nagorni.

## Usage

```clj
(use 'modsynth.core)
(-main)
```

The gui is more or less self contained. 
There are a number of examples. One that gives an idea and doesn't require any user interaction is rand-pend-splitter-example.oms.

## Roadmap

### What it still needs before it's usable:

- [x] Save/restore dialog
- [x] save/restore needs to be better tested.
- [x] Save values for sliders and const.
- [x] Fix slider.
- [x] A disconnect method.
- [x] splitter and
- [x] mixer nodes for audio and control.
- [ ] More synth types:
- [x] Random note and timing
- [ ] some kind of file input for compositions if there is an overtone standard use that.

## License

Copyright © 2013 Bill Allen

Distributed under the Eclipse Public License either version 1.0.
