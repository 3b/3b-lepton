(in-package :3b-lepton)

(defparameter *lep-errors*
  (alexandria:plist-hash-table
   (let ((a '(0 :OK
              -1 :ERROR
              -2 :NOT_READY
              -3 :RANGE_ERROR
              -4 :CHECKSUM_ERROR
              -5 :BAD_ARG_POINTER_ERROR
              -6 :DATA_SIZE_ERROR
              -7 :UNDEFINED_FUNCTION_ERROR
              -8 :FUNCTION_NOT_SUPPORTED
              -9 :DATA_OUT_OF_RANGE_ERROR
              -11 :COMMAND_NOT_ALLOWED
              -15 :OTP_WRITE_ERROR
              -16 :OTP_READ_ERROR
              -18 :OTP_NOT_PROGRAMMED_ERROR
              -20 :ERROR_I2C_BUS_NOT_READY
              -22 :ERROR_I2C_BUFFER_OVERFLOW
              -23 :ERROR_I2C_ARBITRATION_LOST
              -24 :ERROR_I2C_BUS_ERROR
              -25 :ERROR_I2C_NACK_RECEIVED
              -26 :ERROR_I2C_FAIL
              -80 :DIV_ZERO_ERROR
              -101 :COMM_PORT_NOT_OPEN
              -102 :COMM_INVALID_PORT_ERROR
              -103 :COMM_RANGE_ERROR
              -104 :ERROR_CREATING_COMM
              -105 :ERROR_STARTING_COMM
              -106 :ERROR_CLOSING_COMM
              -107 :COMM_CHECKSUM_ERROR
              -108 :COMM_NO_DEV
              -109 :TIMEOUT_ERROR
              -110 :COMM_ERROR_WRITING_COMM
              -111 :COMM_ERROR_READING_COMM
              -112 :COMM_COUNT_ERROR
              -126 :OPERATION_CANCELED
              -127 :UNDEFINED_ERROR_CODE)))
     (loop for (k v) on a by #'cddr
        collect k
        collect v
        when (minusp k)
        collect (ldb (byte 8 0) k)
        and collect v))))
