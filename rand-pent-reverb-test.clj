{:nodes '({:x 636.0, :y 152.0, :w :note-in:1} {:v 5, :x 2.0, :y 246.0, :w :const:3} {:v 10, :x 26.0, :y 69.0, :w :const:4} {:x 346.0, :y 96.0, :w :rand-in:1} {:v 25, :x 24.0, :y 121.0, :w :const:5} {:x 625.0, :y 228.0, :w :saw-osc:2} {:v 60, :x 595.0, :y 416.0, :w :const:8} {:v 70, :x 600.0, :y 464.0, :w :const:9} {:x 877.0, :y 399.0, :w :freeverb:7} {:v -10, :x 162.0, :y 310.0, :w :const:2} {:x 112.0, :y 246.0, :w :sin-vco:2} {:x 301.0, :y 309.0, :w :val-add:1} {:v 30, :x 598.0, :y 508.0, :w :const:10} {:x 640.0, :y 305.0, :w :moog-filt:6} {:x 885.0, :y 255.0, :w :audio-out:3} {:v 50, :x 473.0, :y 363.0, :w :const:12} {:x 588.0, :y 74.0, :w :rand-pent:1} {:v 300, :x 476.0, :y 315.0, :w :const:11}), :connections '[[:const:2-val :val-add:1-val] [:val-add:1-out :rand-in:1-trig] [:rand-in:1-out :rand-pent:1-val] [:rand-pent:1-out :note-in:1-note] [:note-in:1-freq :saw-osc:2-freq] [:saw-osc:2-sig :moog-filt:6-in] [:moog-filt:6-out :freeverb:7-in] [:freeverb:7-out :audio-out:3-b1] [:const:3-val :sin-vco:2-freq] [:sin-vco:2-out :val-add:1-in] [:const:4-val :rand-in:1-lo] [:const:5-val :rand-in:1-hi] [:const:8-val :freeverb:7-wet-dry] [:const:10-val :freeverb:7-dampening] [:const:11-val :moog-filt:6-cutoff] [:const:12-val :moog-filt:6-lpf-res] [:const:9-val :freeverb:7-room-size]], :master-vol 0.3, :frame {:height 600.0, :width 1152.0}}
