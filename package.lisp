(defpackage #:3b-lepton
  (:use :cl)
  (:export
   ;; general
   #:get-status
   #:busy
   #:open-lepton
   #:close-lepton
   #:with-lepton
   ;; agc
   #:agc-enabled
   #:agc-policy
   #:agc-roi
   #:agc-histogram
   #:heq-dampening-factor
   #:heq-clip-limit-high
   #:heq-clip-limit-low
   #:heq-empty-counts
   #:heq-output-scale
   #:agc-calculation-enabled
   #:heq-linear-percent
   ;; sys
   #:ping
   #:status
   #:serial-number
   #:uptime
   #:aux-temp
   #:fpa-temp
   #:aux-temp-kelvin
   #:fpa-temp-kelvin
   #:telemetry-enable
   #:telemetry-location
   #:customer-serial-number
   #:video-scene-statistics
   #:sys-roi-select
   #:thermal-shutdown-count
   #:shutter-position-control
   #:run-ffc-normalization
   #:ffc-status
   #:gain-mode
   #:ffc-state
   #:gain-mode-object
   ;; vid
   #:color-lut-select
   #:focus-calculation-enable
   #:focus-roi
   #:focus-metric-threshold
   #:focus-metric
   #:freeze
   #:vid-output-format
   #:low-gain-color-lut-select
   ;; oem
   #:oem-part-number
   #:lookup-part-number
   #:oem-software-revisions
   #:video-output-enable
   #:oem-video-output-format
   #:oem-video-source
   #:oem-customer-part-number
   #:oem-video-output-source-constant
   #:oem-ffc-normalization-target
   #:oem-status
   #:frame-mean-intensity
   #:gpio-mode-select
   #:gpio-vsync-phase-delay
   #:user-defaults
   #:shutter-profile
   #:thermal-shutdown-enable
   #:bad-pixel-replacement-control
   #:temporal-filter-control
   #:column-noise-filter-control
   #:pixel-noise-filter-control
   ;; rad
   #:rfbo-external-params
   #:radiometry-control
   #:t-shutter-mode
   #:t-shutter-temperature
   #:rad-run-status
   #:flux-linear-parameters
   #:t-linear
   #:t-linear-resolution
   #:t-linear-auto-resolution
   #:spotmeter-roi
   #:spotmeter-value*
   #:spotmeter-value
   #:low-gain-rfbo-external-params))
