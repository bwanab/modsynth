{:nodes '({:x 201.0, :y 177.0, :w :c-splitter:1} {:x 6.0, :y 199.0, :w :sin-vco:4} {:v 0.5, :x 11.0, :y 267.0, :w :const:3} {:v 100, :x 213.0, :y 335.0, :w :const:2} {:v 50, :x 415.0, :y 495.0, :w :const:1} {:x 797.0, :y 223.0, :w :audio-out:6} {:x 588.0, :y 197.0, :w :a-splitter:5} {:x 393.0, :y 47.0, :w :square-osc:2} {:x 0.0, :y 0.0, :w :midi-in:1}), :connections '([:c-splitter:1-ob2 :a-splitter:5-pos] [:c-splitter:1-ob1 :square-osc:2-width] [:sin-vco:4-out :c-splitter:1-in] [:midi-in:1-freq :square-osc:2-freq] [:const:3-val :sin-vco:4-freq] [:square-osc:2-sig :a-splitter:5-in] [:a-splitter:5-ob1 :audio-out:6-b1] [:a-splitter:5-ob2 :audio-out:6-b2] [:const:2-val :a-splitter:5-lev]), :master-vol 0.3}