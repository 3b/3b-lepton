(in-package 3b-lepton)
#++
(with-lepton (l)
  (print (3b-i2c:get-functionality (i2c l)))
  (print (get-status l))
  (format t "~&ping: ~s~%" (ping l))
  (format t "~&status: ~s~%" (status l))
  (format t "~&serial: ~s~%" (serial-number l))
  (format t "~&uptime: ~s~%" (loop repeat 4 collect (uptime l)) )
  (format t "~&temp: aux: ~s~%      fpa: ~s~%" (aux-temp l) (fpa-temp l))
  (format t "~&telemetry: ~s @ ~s~%"
	  (telemetry-enable l) (telemetry-location l))
  (format t "~&   set enable: ~s, header:~s~%"
	  (setf (telemetry-enable l) t)
	  (setf (telemetry-location l) :header))
  (format t "~&telemetry: ~s @ ~s~%"
	  (telemetry-enable l) (telemetry-location l))
  (format t "~&   set disable: ~s footer:~s~%"
	  (setf (telemetry-enable l) nil)
	  (setf (telemetry-location l) :footer))
  (format t "~&customer serial number: ~s~%" (customer-serial-number l))
  (format t "scene stats ~s~%" (video-scene-statistics l))
  (format t "roi: ~s~%" (sys-roi-select l))
  (format t " -> 40,50:60,70: ~s~%" (setf (sys-roi-select l) '(40 50 60 70)))
  (format t "roi: ~s~%" (sys-roi-select l))
  (format t "scene stats ~s~%" (video-scene-statistics l))
  (format t " -> 0,0:159,119: ~s~%"
	  (setf (sys-roi-select l) '(:x1 0 :x2 159 :y1 0 :y2 119)))
  (format t "shutter position control: ~s~%"
	  (shutter-position-control l))
  (format t "ffc status: ~s~%" (ffc-status l))
  (format t "gain-mode: ~s~%" (gain-mode l))
  (format t "  -> auto: ~s~%" (setf (gain-mode l) :auto))
  (format t "gain-mode: ~s~%" (gain-mode l))
  (format t "  -> high: ~s~%" (setf (gain-mode l) :high))
  (format t "ffc-state: ~s~%" (ffc-state l))
  (format t "gain mode params: ~s~%" (gain-mode-object l))
  (format t "color lut: ~s~%" (color-lut-select l))
  (finish-output *standard-output*)
  ;; user-lut causes problems, skipping for now...
  #++
  (format t "user lut: ~{~2,'0x~2,'0x ~2,'0x~2,'0x ~2,'0x~2,'0x ~2,'0x~2,'0x~%~^          ~}"
	  (user-lut l))
  (format t "focus calculation: ~s~%" (focus-calculation-enable l))
  (format t "focus roi: ~s~%" (focus-roi l))
  (format t "focus threshold: ~s~%" (focus-metric-threshold l))
  (format t "focus metric: ~s~%" (focus-metric l))
  (format t "freeze: ~s~%" (freeze l))
  (format t "low-gain color lut: ~s~%" (low-gain-color-lut-select l))
  (format t "video-format: ~s~%" (vid-output-format l))

  (format t "~%part number ~s (~s)~%"
	  (oem-part-number l) (lookup-part-number l))
  (format t "software version: ~s~%" (oem-software-revisions l))
  (format t "video output enable: ~s~%" (video-output-enable l))
  (format t "oem video format: ~s~%" (oem-video-output-format l))
  (format t "oem video source: ~s~%" (oem-video-source l))
  (format t "customer part number: ~s~%" (oem-customer-part-number l))
  (format t "constant output value: ~s~%" (oem-video-output-source-constant l))
  (format t "ffc normalization target: ~s~%" (oem-ffc-normalization-target l))
  (format t "frame mean intensity: ~s~%" (frame-mean-intensity l))
  (format t "gpio mode: ~s~%" (gpio-mode-select l))
  (format t "gpio vsync phase delay: ~s~%" (gpio-vsync-phase-delay l))
  (format t "user-defaults: ~s~%" (user-defaults l))
  
  (format t "shutter-profile: ~s~%" (shutter-profile l))
  (format t "thermal shutdown enable: ~s~%" (thermal-shutdown-enable l))
  (format t "bad-pixel-replacement-control: ~s~%"
	  (bad-pixel-replacement-control l))
  (format t "temporal filter: ~s~%" (temporal-filter-control l))
  (format t "column noise filter: ~s~%" (column-noise-filter-control l))
  (format t "pixel noise filter: ~s~%" (pixel-noise-filter-control l))
  (format t "rfbo params: ~s~%" (rfbo-external-params l))
  (format t "radiometry control: ~s~%" (radiometry-control l))
  (format t "tshutter mode: ~s~%" (t-shutter-mode l))
  (format t "tshutter temp: ~s~%" (t-shutter-temperature l))
  (format t "rad run status: ~s~%" (rad-run-status l))
  (format t "flux linear params: ~s~%" (flux-linear-parameters l))
  (format t "t-linear: ~s, resolution ~s, auto ~s~%"
	  (t-linear l) (t-linear-resolution l) (t-linear-auto-resolution l))
  (format t "spotmeter roi: ~s~%" (spotmeter-roi l))
  (format t "spotmeter value: ~s~%" (spotmeter-value l))
  (format t "spotmeter value (f): ~s~%" (spotmeter-value l :units :f))
  (format t "low gain rfbo params: ~s~%" (low-gain-rfbo-external-params l))

  (format t "agc: ~s~%" (agc-enabled l))
  (format t "agc policy: ~s~%" (agc-policy l))
  (format t "agc roi: ~s~%" (agc-roi l))
  (format t "agc histogram: ~s~%" (agc-histogram l))
  (format t "heq dampening factor: ~s~%" (heq-dampening-factor l))
  (format t "heq clip limits: ~s ~s~%"
	  (heq-clip-limit-low l) (heq-clip-limit-high l))
  #++ ;; doesn't work?
  (format t "heq empty counts: ~s~%" (heq-empty-counts l))
  (format t "heq output scale: ~s~%" (heq-output-scale l))
  (format t "heq linear %: ~s~%" (heq-linear-percent l))
  (format t "agc calculation enabled: ~s~%" (agc-calculation-enabled l))
  
  )