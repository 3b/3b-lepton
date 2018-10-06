## 3b-lepton: Common Lisp library for controlling [FLIR Lepton](https://www.flir.com/products/lepton/) thermal cameras over i2c/spi

Currently just the i2c parts. Most commands are implemented, though
some can only be `get` and not `set` yet.

Tested with Lepton 3.5 on [Lepton breakout board](https://www.digikey.com/product-detail/en/flir/250-0587-00/250-0587-00-ND/)
with sbcl on a Raspberry Pi 3 Model B+.


### example dumping (+ a bit of modifying) configuration info from camera

```Lisp


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
  (format t "agc calculation enabled: ~s~%" (agc-calculation-enabled l)))
```

output:

```
(:I2C :SMBUS-PEC :SMBUS-QUICK :SMBUS-READ-BYTE :SMBUS-WRITE-BYTE
 :SMBUS-READ-BYTE-DATA :SMBUS-WRITE-BYTE-DATA :SMBUS-READ-WORD-DATA
 :SMBUS-WRITE-WORD-DATA :SMBUS-PROC-CALL :SMBUS-WRITE-BLOCK-DATA
 :SMBUS-READ-I2C-BLOCK :SMBUS-WRITE-I2C-BLOCK) 
(:OK :READY :BOOTED :BOOT-OK 6) 
ping: T
status: (:STATUS :READY :COMMAND-COUNT 0 :RESERVED 0)
serial: "00:00:00:00:00:12:34:56"
:GET :SYS :UPTIME: (:OK :BUSY :BOOTED :BOOT-OK 7)
uptime: (16391.94 16392.045 16392.049 16392.053)
temp: aux: (:K 301.41 :C 28.26001 :F 82.86801)
      fpa: (:K 301.86 :C 28.709991 :F 83.67798)
:GET :SYS :TELEMETRY-LOCATION: (:OK :BUSY :BOOTED :BOOT-OK 7)
telemetry: NIL @ :FOOTER
   set enable: (1 0), header:(0 0)
telemetry: :ENABLED @ :HEADER
   set disable: (0 0) footer:(1 0)
customer serial number: "12:23:34:45:56:67:78:89:9A:AB:BC:CD:DE:EF:F1:23:45:67:89:AB:CD:EF:12:34:56:78:90:AB:CD:EF:01:23"
scene stats (:MIN 3257 :MAX 3421 :MEAN 3217 :PIXEL-COUNT 19200)
roi: (:X1 0 :Y1 0 :X2 159 :Y2 119)
 -> 40,50:60,70: (40 50 60 70)
roi: (:X1 40 :Y1 50 :X2 60 :Y2 70)
scene stats (:MIN 3257 :MAX 3421 :MEAN 3217 :PIXEL-COUNT 19200)
 -> 0,0:159,119: (0 0 159 119)
shutter position control: :IDLE
ffc status: :READY
gain-mode: :HIGH
  -> auto: (2 0)
gain-mode: :AUTO
  -> high: (0 0)
ffc-state: :DONE
gain mode params: (:X1 0 :Y1 0 :X2 159 :Y2 119 :P-HIGH-TO-LOW 25 :P-LOW-TO-HIGH
                   90 :C-HIGH-TO-LOW 115 :C-LOW-TO-HIGH 85 :T-HIGH-TO-LOW 388
                   :T-LOW-TO-HIGH 358 :ROI-POPULATION 19200 :TEMP-ENABLED 1
                   :FLUX-THRESHOLD-LOW-TO-HIGH 6953 :FLUX-THRESHOLD-HIGH-TO-LOW
                   9398)
color lut: :GREYSCALE
focus calculation: :DISABLE
focus roi: (:X1 1 :Y1 1 :X2 158 :Y2 118)
focus threshold: 30
focus metric: 0
freeze: NIL
low-gain color lut: :GREYSCALE
video-format: :RAW14

part number "500-0771-01" ("Lepton 3.5")
software version: (:GPP (3 3 26) :DSP (3 3 26) :RESERVED 0)
video output enable: :ENABLED
oem video format: :RAW14
oem video source: :COOKED
customer part number: "0123456789abcdef0123456789abcdef"
constant output value: 0
ffc normalization target: 4096
frame mean intensity: 3257
:GET :OEM :GPIO-MODE-SELECT: (:OK :BUSY :BOOTED :BOOT-OK 7)
gpio mode: :GPIO
gpio vsync phase delay: 0
user-defaults: :NOT-WRITTEN
shutter-profile: (:CLOSE 4 :OPEN 1)
thermal shutdown enable: :ENABLED
bad-pixel-replacement-control: :ENABLED
temporal filter: :ENABLED
column noise filter: :ENABLED
pixel noise filter: :ENABLED
rfbo params: (:R 354747 :B 1435.0 :F 1.0 :O 391.358)
radiometry control: :ENABLED
tshutter mode: :CAL
tshutter temp: 30000
rad run status: :READY
flux linear params: (:SCENE-EMISSIVITY 100.0 :T-BG-K 295.15 :TAU-WINDOW 100.0
                     :T-WINDOW-K 295.15 :TAU-ATM 100.0 :T-ATM-K 295.15
                     :REFL-WINDOW 0.0 :T-REFL-K 295.15)
t-linear: :ENABLED, resolution :|0.01|, auto NIL
spotmeter roi: (:X1 59 :Y1 79 :X2 60 :Y2 80)
:GET :RAD :T-LINEAR-RESOLUTION: (:OK :BUSY :BOOTED :BOOT-OK 7)
spotmeter value: (:VALUE 296.77 :MAX 296.8 :MIN 296.74 :POPULATION 4)
spotmeter value (f): (:VALUE 74.51599 :MAX 74.56999 :MIN 74.46199 :POPULATION 4)
low gain rfbo params: (:R 102145 :B 1470.0 :F 1.0 :O 353.796)
agc: NIL
agc policy: :HEQ
agc roi: (:X1 0 :Y1 0 :X2 159 :Y2 119)
agc histogram: (:MIN 0 :MAX 16383 :MEAN 3847 :PIXELS 14560)
heq dampening factor: 0
heq clip limits: 512 19200
heq output scale: :8-BITS
heq linear %: 20
agc calculation enabled: :ENABLED

```

