{:nodes '({:v 1000, :x 2.0, :y 84.0, :w :const:2} {:x 152.0, :y 0.0, :w :amp:1} {:v 50, :x 160.0, :y 176.0, :w :const:5} {:v 10, :x 160.0, :y 114.0, :w :const:4} {:x 536.0, :y 88.0, :w :audio-out:3} {:x 321.0, :y 61.0, :w :echo:2} {:x 0.0, :y 0.0, :w :audio-in:1}), :connections '([:amp:1-out :echo:2-in] [:audio-in:1-out :amp:1-in] [:const:2-val :amp:1-gain] [:const:4-val :echo:2-delay-time] [:const:5-val :echo:2-decay-time] [:echo:2-out :audio-out:3-b1]), :master-vol 0.3}