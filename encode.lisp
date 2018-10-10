(in-package 3b-lepton)

;; some utilties for piping video data to ffmpeg for encoding (sbcl
;; only, uses pi specific encoder)

;; https://stackoverflow.com/questions/51903888/is-it-possible-to-send-ffmpeg-images-by-using-pipe
;; https://www.reddit.com/r/raspberry_pi/comments/5677qw/hardware_accelerated_x264_encoding_with_ffmpeg/

#++
(format nil "ffmpeg ~
  -y -f rawvideo -pix_fmt argb -s 160x120 -r 9 -i - ~
  -c:v h264_omx -b:v 500k ~a" filename)
;; -profile:v baseline -level:v 3 -b:v 2500 -an out_vid.h264

(defun run-ffmpeg (output)
  (sb-ext:run-program
   "ffmpeg"
   (list "-y" "-f" "rawvideo" "-pix_fmt" "argb"
	 "-s" "160x120" "-r" "9" "-i" "-"
	 "-c:v" "h264_omx" "-b:v" "1500k"
	 output)
   :search t
   :wait nil
   :output t
   :input :stream))

(defmacro with-ffmpeg-stream ((s filename) &body body)
  (alexandria:with-gensyms (process)
    `(let ((,process (run-ffmpeg ,filename)))
       (with-open-stream (,s (sb-ext:process-input ,process))
         ,@body))))
