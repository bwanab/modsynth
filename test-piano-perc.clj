{:nodes '({:x 389.0, :y 243.0, :w :audio-out:6} {:v 100, :x -9.0, :y 267.0, :w :const:5} {:v 0.1, :x -2.0, :y 207.0, :w :const:4} {:x 149.0, :y 172.0, :w :perc-env:3} {:x 78.0, :y 85.0, :w :saw-osc:2} {:x 0.0, :y 0.0, :w :piano-in:1}), :connections '[[:piano-in:1-freq :saw-osc:2-freq] [:saw-osc:2-sig :perc-env:3-in] [:perc-env:3-out :audio-out:6-b1] [:const:4-val :perc-env:3-attack] [:const:5-val :perc-env:3-release]], :master-vol 0.3}